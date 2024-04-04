# # # # # # # # # # # # # # # # #
# Null Model Data Frame Prep and Null Distribution Generation 
# Code adapted from Kohli et al 2022
# Prep -----------------------------------------------------------------------
# Load packages
require(data.table)  
require(gawdis)      
require(FD)          
require(ape)         
require(phytools)    
require(picante)      
require(hypervolume) 
require(dplyr)        
require(usedist)      
require(vegan)        
require(moments) 

library(FD)
library(gawdis)
library(tidyverse)
library(funrar)
# Step 1: -----------------------------------------------------------------------
# Need to convert output of distance corrected abundances, filtered by range maps to a data table that can be easily randomized, sub-setting for NEON Domains to allow for regional species pools that differ by Domain. 

data <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_2017_df_update_DistCorrAbund20000_SpRangeNEONDomainOverlap.csv")
key <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/Key_spec_scinames.csv")

key_scinames <- key$scientificName
old_names <- colnames(data)

# Rename columns from list
col_names = c(old_names[1:6], key_scinames) # replace "V#" format with scientific names
colnames(data) <- c(col_names)

# Pivot columns to rows. 
data2 <- data %>% tidyr::pivot_longer(
  cols = "Acanthis flammea":"Zonotrichia leucophrys", # range of columns to turn into rows
  names_to = "scientificName", # column header for new column
  values_to = "count") # name of new column that came from values from old columns 

data2

# Fix spacing in species names 
data2$scientificName <- gsub(" ", "_", data2$scientificName) # replace spaces with "_" to match BirdTree

data2$spId <- as.numeric(as.factor(data2$scientificName))

length(unique(data2$scientificName)) # 260: All species included
# Filter out low probability occurances
data3 <- data2 %>%filter(count>=0.95) # remove all NAs and less than 0.95 abundances

summary(data3)
length(unique(data3$scientificName)) # Drop 2 species with low abundances (Cistothorus platensis and Pinicola_enucleator)

# as.data.frame(data3)%>%filter(plotNum==344)%>%summary() # check species richnesses for accuracy

data4 <- as.data.frame(dplyr::select(data3, plotNum, plotID, siteID, domainID, scientificName, Abundance=count, spId))

length(data4$plotNum)
#### Data now has domainID 
# data4$plotID_factor <- as.factor(data4$plotID) #create new column to hold factors of mtn IDs
# data4$spId <- as.numeric(as.factor(data4$scientificName)) #create new column to hold numeric species IDs

# Extra data quality prep/check # Not strictly neccisary 
data4<- data4 %>% distinct() # filter out duplicated columns (shouldn't be neccisary)

tail(data4[order(data4$scientificName), ]) # should be 260 species IDs 
length(unique(data4$spId)) # should be 258 since 2 species are dropped due to filtering. 

summary(data4$Abundance) # shouldn't have any NAs at this point. All species should have >0.95
# Step 2 -----------------------------------------------------------------------

###################################################
#######Constructing Null Assemblages 
###################################################

#### Randomize via Independent swap null (holds row and column totals constant; richness and species occurrence frequencies across the gradient)
######

head(data4)
data.species <- dplyr::select(data4, scientificName, spId)
data.speciesU <- distinct(data.species)
data.speciesU <- data.speciesU %>% mutate_if(is.numeric, as.factor)

domainIDs <- unique(data4$domainID)

# comm.mo <- subset(data, domainID == "D18") # test only prior to for loop

# Make sure WD is where you want it
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/output")

##########################################################################################
# Null community generation -------------------------------------------------------------
##########################################################################################

# Run null assemblages 
for (i in 1:100){
  comm.null.all <- list()
  
  for (k in 1:length(domainIDs)) {
    comm.null.mo <- list()
    comm.mo <- subset(data4, domainID == domainIDs[k])
    comm.mo.samp <- dplyr::select(comm.mo, plotID, Abundance, spId)
    comm.mo.mat <- sample2matrix(comm.mo.samp) # Convert to community data matrix.
    comm.mo.mat <- decostand(comm.mo.mat, method = "pa") # scale x to presence/absence scale (0/1).
    # Randomize Community # 
    comm.mo.mat.rdm <- randomizeMatrix(comm.mo.mat, null.model = "independentswap", iterations = 1000) # Randomize community data matrix with the independent swap algorithm (Gotelli 2000) maintaining species occurrence frequency and sample species richness
    comm.mo.samp.rdm <- matrix2sample(comm.mo.mat.rdm) # Convert to a Phylocom database-format community sample
    colnames(comm.mo.samp.rdm) <- c("plotID.null", "Abundance.null", "spId.null")
    
    comm.mo.samp.rdm.order <-comm.mo.samp.rdm[order(comm.mo.samp.rdm$plotID.null),] #put plotID in order of random community 
    comm.mo.order <- comm.mo[order(comm.mo$plotID),] #put plotID in order of original community
    comm.null.mo1 <- cbind(comm.mo.samp.rdm.order, comm.mo.order) # Combine random and non-random together
    #  [1] "plotID.null"    "Abundance.null" "spId.null"      "plotNum"        "plotID"         "siteID"
    #  [7] "domainID"       "scientificName" "Abundance"      "spId"  
    comm.null.mo1 <- dplyr::select(comm.null.mo1, plotID, domainID, siteID, Abundance, spId.null) # Basically, associate the new speciesID with the OLD Abundance to get a new species with the same abundance at each plot. Species richness should be the same for each site
    
    comm.null.mo.k <- inner_join(comm.null.mo1, data.speciesU, by = c("spId.null" = "spId")) ## 
    # Adds the scientific name to the data
    # data.speciesU has 258 species 

    comm.null.mo = comm.null.mo.k %>% dplyr::select(1:4, scientificName, spId.null) #####
    # Slightly reorders the columns
    
    comm.null.all <- rbind(comm.null.all, comm.null.mo)  # Add each domain to the list of all other domains to save and export at the end, once this loops through all the iterations. 
  }
  
  
  saveRDS(comm.null.all, file=paste0("null_distcorrect_rangefiltered_IndSwap_",i,".rds"))
}

# Step 3 -----------------------------------------------------------------------
# Calculate Functional and Phylogenetic indices for each of the 100 random assemblages 

####################################################################

# First, Load in Trait data and do some data preping 
##############-----------------------------------------------------------------
# Add Elton Traits: Body Mass, Foraging Stratum, Diet 
Elton <-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/Elton/Elton_Birds_reduced.csv")
#head(Elton)
#colnames(Elton)[27] ="scientificName"
Elton2 <-Elton
# Fix spacing in species names 
Elton2$scientificName <- gsub(" ", "_", Elton2$scientificName) # replace spaces with "_" to match BirdTree

Elton2 <- subset(Elton2, English != "Leaf-love" )
Elton2 <- subset(Elton2, English != "Blue-winged Warbler" )
Elton2 <- subset(Elton2, English != "Dapple-throat" )

# TESTING FOR ERRORS
length(Elton$Order)
length(Elton2$Order)

#filter(Elton2, scientificName=="Haemorhous_cassinii")
#filter(Elton2, scientificName=="Setophaga_pinus")
#filter(Elton2, scientificName=="Haemorhous_mexicanus")

##############-----------------------------------------------------------------
# Add AVONET Traits
AVONET <-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/AVONET/ELEData/TraitData/AVONET_Raw_Data.csv")

head(AVONET)

AVONET2 <-dplyr::select(AVONET,scientificName=Species1_BirdLife, Sex,Beak.Width,Beak.Depth,Beak.Length_Culmen, Hand.wing.Index)

AVONET3<-AVONET2 %>% group_by(scientificName)%>%
  summarize_at(c('Beak.Width','Beak.Depth','Beak.Length_Culmen','Hand.wing.Index'),mean, na.rm = TRUE) %>%as.data.frame()# create average values for each species

head(AVONET3)

##############-----------------------------------------------------------------
# 1) Find the Scientific names for the missing 14 species
# 2) RENAME Scientific Names to Match NEON Scientific Names
AVONET4<-AVONET3 # Test with AVONET4

# Data prep for trait data (matching names) # # # # # # #
# Henslow's Sparrow ---------------------------------------
Elton[Elton$English == "Henslow's Sparrow",]
AVONET3[AVONET3$scientificName == "Passerculus henslowii",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Passerculus henslowii"] <- "Ammodramus henslowii"

AVONET4[AVONET4$scientificName == "Ammodramus henslowii",]
AVONET3[AVONET3$scientificName == "Passerculus henslowii",]# check to see if the numbers match

#American Wigeon ---------------------------------------
Elton[Elton$English == "American Wigeon",]
AVONET3[AVONET3$scientificName == "Mareca americana",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Mareca americana"] <- "Anas americana"

#Blue-winged Teal ---------------------------------------
Elton[Elton$English == "Blue-winged Teal",]
AVONET3[AVONET3$scientificName == "Spatula discors",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Spatula discors"] <- "Anas discors"

#Sedge Wren ---------------------------------------
Elton[Elton$English == "Sedge Wren",]
AVONET3[AVONET3$scientificName == "Cistothorus platensis",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Cistothorus platensis"] <- "Cistothorus platensis"

#Pileated Woodpecker ---------------------------------------
Elton[Elton$English == "Pileated Woodpecker",]
AVONET3[AVONET3$scientificName == "Hylatomus pileatus",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Hylatomus pileatus"] <- "Dryocopus pileatus"

#Sandhill Crane ---------------------------------------
Elton[Elton$English == "Sandhill Crane",]
AVONET3[AVONET3$scientificName == "Antigone canadensis",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Antigone canadensis"] <- "Grus canadensis"

#Bullock's Oriole ---------------------------------------
Elton[Elton$English == "Bullock's Oriole",]
AVONET3[AVONET3$scientificName == "Icterus bullockiorum",]
AVONET3[grep("bullockiorum", AVONET3$scientificName), ]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Icterus bullockiorum"] <- "Icterus bullockii"

#Double-crested Cormorant ---------------------------------------
Elton[Elton$English == "Double-crested Cormorant",]
AVONET3[AVONET3$scientificName == "Nannopterum auritus",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Nannopterum auritus"] <- "Phalacrocorax auritus"

#Wilson's Phalarope ---------------------------------------
Elton[Elton$English == "Wilson's Phalarope",]
AVONET3[AVONET3$scientificName == "Steganopus tricolor",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Steganopus tricolor"] <- "Phalaropus tricolor"

#White-headed Woodpecker ---------------------------------------
Elton[Elton$English == "White-headed Woodpecker",]
AVONET3[AVONET3$scientificName == "Leuconotopicus albolarvatus",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Leuconotopicus albolarvatus"] <- "Picoides albolarvatus"

#Nuttall's Woodpecker ---------------------------------------
Elton[Elton$English == "Nuttall's Woodpecker",]
AVONET3[AVONET3$scientificName == "Dryobates nuttallii",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Dryobates nuttallii"] <- "Picoides nuttallii"

#Ladder-backed Woodpecker ---------------------------------------
Elton[Elton$English == "Ladder-backed Woodpecker",]
AVONET3[AVONET3$scientificName == "Dryobates scalaris",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Dryobates scalaris"] <- "Picoides scalaris"

#Hairy Woodpecker ---------------------------------------
Elton[Elton$English == "Hairy Woodpecker",]
AVONET3[AVONET3$scientificName == "Leuconotopicus villosus",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Leuconotopicus villosus"] <- "Picoides villosus"

#American Tree Sparrow ---------------------------------------
Elton[Elton$English == "American Tree Sparrow",]
AVONET3[AVONET3$scientificName == "Passerella arborea",]
#Replace name for AVONET
AVONET4["scientificName"][AVONET4["scientificName"] == "Passerella arborea"] <- "Spizelloides arborea"


# Fix spacing in species names 
AVONET4$scientificName <- gsub(" ", "_", AVONET4$scientificName) # replace spaces with "_" to match BirdTree


# Original Data
dfB <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_2017_df_update_DistCorrAbund20000_SpRangeNEONDomainOverlap.csv")
plot_Key <- select(dfB, plotNum, plotID) # Create Key so that plotID and plotNum can be matched. 
# summary(dfB)

# End prep ## # # # # #

################################################################################
# Step 2: For Loop. Load in 100 random communities and calculate indices for each 
# look at an example
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/output")
FEve_null<-c()
FDiv_null<-c()
FRic_null<-c()

for(i in 1:100){
  ##################################################################
  n1 <-readRDS(file=paste0("null_distcorrect_rangefiltered_IndSwap_",i,".rds"))
  ##################################################################
  # Take output of Null Model Creation and create dataframes with Trait data associated. 
  data7<-right_join(n1, Elton2, by="scientificName") # Add Traits
  data8<- data7[!is.na(data7$plotID),] # remove extra rows
  data9<-right_join(data8, AVONET4, by="scientificName") # Add more Traits
  data10<- data9[!is.na(data9$plotID),] # remove extra rows
  
  # Turn n1 into Correct data format for species abundances 
  data11 <- dplyr::select(data10, plotID, scientificName, Abundance) # subset
  data12 <- data11 %>% tidyr::pivot_wider(names_from =scientificName, values_from =Abundance)%>%as.data.frame()
  
  ##################################################################
  data13 <- left_join(plot_Key, data12, by="plotID") # use key to match the plot number and plotID
  data13<-data13 %>% remove_rownames %>% column_to_rownames(var="plotNum")# make sure row names correspond to plot numbers
  data13 <- data13%>%subset(select = -plotID) # remove extra column
  
  ##################################################################
  # Trait Data 
  # Subset trait data to include one copy of traits per scientific name
  trait_data <- dplyr::select(data10, scientificName,Diet.Inv,Diet.Vend,Diet.Vect,Diet.Vfish,Diet.Vunk,Diet.Scav,Diet.Fruit,Diet.Nect,Diet.Seed,Diet.PlantO,ForStrat.watbelowsurf,ForStrat.wataroundsurf,ForStrat.ground,ForStrat.understory,ForStrat.midhigh,ForStrat.canopy,ForStrat.aerial,PelagicSpecialist,BodyMass.Value,Beak.Width,Beak.Depth,Beak.Length_Culmen,Hand.wing.Index)
  
  trait_data<-trait_data[!duplicated(trait_data$scientificName), ]
  
  trait_data2 <-trait_data%>% remove_rownames %>%tibble::column_to_rownames(var="scientificName")
  ##################################################################
  #use gawdis for distance matrix calculation to improve the equal weighting of traits/trait groupings
  
  # Designate trait groupings for gawdis (groups = different values)
  # Trait groups: Diet (10), ForStrat (8), BodyMass, Beak, Hand-wing Index
  Trait.groups = c(rep(1, 10), rep(2, 8), 3, rep(4, 3), 5)
  
  #identify the trait groups that represent fuzzy coding or dummy variables - values matching the group values above.
  fuzzy.groups = c(1, 2, 4)  # Diet, ForStrat, Beak
  
  ###################################################################
  suppressWarnings(funcdist <- gawdis(trait_data2, 
                                      w.type = "optimized", 
                                      groups = Trait.groups, 
                                      fuzzy = fuzzy.groups, 
                                      opti.maxiter = 300) )#300 is the default
  #### Functional Diversity Calculations -----------------------------------------
  # Original way: FD package
  # OLD: df_test3<-df_test2
  
  df_test3 <- suppressWarnings(mutate_all(data13, function(x) as.numeric(as.character(x))))
  df_test3<- funrar::make_relative(as.matrix(df_test3)) # make abundance relative (sums to 1 per row)
  
  dbfd_output <- dbFD(funcdist, df_test3, 
                      corr = "lingoes", 
                      #ord = c("podani", "metric"),
                      w.abun = T,
                      calc.FGR = F, 
                      calc.CWM = F, 
                      print.pco = F,
                      m=5)
  
  # Save output for each null community
  saveRDS(dbfd_output, file=paste0("//Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FD/dbfd_output_chp1_Null_",i,".rds"))
  
  # Save values of interest for each community. 
  FEve_null_temp <- dbfd_output$FEve
  FDiv_null_temp <- dbfd_output$FDiv
  FRic_null_temp <- dbfd_output$FRic
  
  # Have to start out pre for loop by creating FEve_null objects 
  FEve_null<-rbind(FEve_null, NullCommunity=FEve_null_temp) # Will save each Null community as a new row
  FDiv_null<-rbind(FDiv_null, NullCommunity=FDiv_null_temp)
  FRic_null<-rbind(FRic_null, NullCommunity=FRic_null_temp)
  
}

write_csv(as.data.frame(FEve_null), "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FEve_null.csv")
write_csv(as.data.frame(FDiv_null), "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FDiv_null.csv")
write_csv(as.data.frame(FRic_null), "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FRic_null.csv")

saveRDS(FEve_null, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FEve_null_chp1.rds")
saveRDS(FDiv_null, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FDiv_null_chp1.rds")
saveRDS(FRic_null, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/FRic_null_chp1.rds")

################################################################################
################################################################################
################################################################################
################################################################################
# Original Data
# dbfd_output <- readRDS(file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/dbfd_output_chp1_super20000.rds")

#
FRic_null # 100 Null community
test_null <- FRic_null
row.names(test_null) <- c(1:100)

#
dbfd_output$FRic # Original 
test <- as.data.frame(cbind(plotNum=c(1:448), original_FRic=dbfd_output$FRic)) # Original output 
head(test)

################################################################################
t(FRic_null)
t(test_b)

try <- cbind(plotNum=c(1:448), original=dbfd_output$FRic, t(test_null)) #the first column of values is the observed value, the other columns are random iterations' values, ##rows are for each site (bin).
head(try)
tail(try)

#  Matrix of just values. 
## Rank observed value, calculate SES and p value
obs.null.output <- as.matrix(try[,2:(ncol(try))])

# RANKING 
#quantify the rank or the observed value in the null distribution for each community
test$obs.rank <- apply(obs.null.output, MARGIN = 1, rank)[1,] # Margin (1)=rows, (2)=columns

#### SES
#quantify the Standardized Effect Size;  SES = (observed - null mean) / null sd
# 448 rows, 100 columns
test$SES.FRic <- (obs.null.output[,1] - apply(obs.null.output[,2:(ncol(obs.null.output))], 1, mean)) / apply(obs.null.output[,2:(ncol(obs.null.output))], 1, sd)

# p Values
#calculate p values: 
ncol(obs.null.output) # 101
test$FRic.p <- apply(cbind(obs.null.output[,1], obs.null.output[,2:(ncol(obs.null.output))]), MARGIN = 1, rank)[1,] / 101 

# Examine data as needed 
View(test)
head(test)
summary(test)
hist(test$original_FRic)
hist(test$obs.rank)
hist(test$SES.FRic)

# Save final SES values 
write.csv(test, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_FRic_Chap1.csv")
# FD metrics are now complete 




################################################################################
#### PD NULL Community calculations---------------------------------------------
################################################################################

# Original Data
# df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter_transposed_traits2.csv")

library(ape)
setwd("/Users/colinsweeney/Documents/Documents/Datasets/Phylogentics/BirdTree/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/")

BirdTree <- read.tree("AllBirdsEricson1.tre") # 1000 random phylogentic trees

##############-----------------------------------------------------------------
# head(n1[n1$scientificName == "Spizelloides_arborea",])
# head(df2[df2$scientificName == "Spizella_arborea",])# check to see if the numbers match
################################################################################
# Prepare Dataframes for final data input at the end of for loops 
# Run once per data for loop run
PD_Null_means <- matrix(NA,length(list_plotNum),100)
colnames(PD_Null_means)<-c(1:100)
PD_Null_means <- cbind(list_plotNum, PD_Null_means) # 

MPD_Null_means <- matrix(NA,length(list_plotNum),100)
colnames(MPD_Null_means)<-c(1:100)
MPD_Null_means <- cbind(list_plotNum, MPD_Null_means) # 

################################################################################

for(j in 1:10){ # 1:100
  # Read in null community
  setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/output/")
  df2 <-readRDS(file=paste0("null_distcorrect_rangefiltered_IndSwap_",j,".rds")) 
  
  # RENAME Scientific Names to Match NEON Scientific Names
  df2["scientificName"][df2["scientificName"] == "Acanthis_flammea"] <- "Carduelis_flammea"
  df2["scientificName"][df2["scientificName"] == "Ardea_alba"] <- "Casmerodius_albus"
  df2["scientificName"][df2["scientificName"] == "Cardellina_pusilla"] <- "Wilsonia_pusilla"
  df2["scientificName"][df2["scientificName"] == "Geothlypis_formosa"] <- "Oporornis_formosus"
  df2["scientificName"][df2["scientificName"] == "Geothlypis_philadelphia"] <- "Oporornis_philadelphia"
  df2["scientificName"][df2["scientificName"] == "Geothlypis_tolmiei"] <- "Oporornis_tolmiei"
  df2["scientificName"][df2["scientificName"] == "Haemorhous_cassinii"] <- "Carpodacus_cassinii"
  df2["scientificName"][df2["scientificName"] == "Haemorhous_mexicanus"] <- "Carpodacus_mexicanus"
  df2["scientificName"][df2["scientificName"] == "Haemorhous_purpureus"] <- "Carpodacus_purpureus"
  df2["scientificName"][df2["scientificName"] == "Ixoreus_naevius"] <- "Zoothera_naevia"
  df2["scientificName"][df2["scientificName"] == "Melozone_crissalis"] <- "Pipilo_crissalis"
  df2["scientificName"][df2["scientificName"] == "Parkesia_motacilla"] <- "Seiurus_motacilla"
  df2["scientificName"][df2["scientificName"] == "Peucaea_aestivalis"] <- "Aimophila_aestivalis"
  df2["scientificName"][df2["scientificName"] == "Peucaea_botterii"] <- "Aimophila_botterii"
  df2["scientificName"][df2["scientificName"] == "Peucaea_carpalis"] <- "Aimophila_carpalis"
  df2["scientificName"][df2["scientificName"] == "Peucaea_cassinii"] <- "Aimophila_cassinii"
  df2["scientificName"][df2["scientificName"] == "Phalaropus_tricolor"] <- "Steganopus_tricolor"
  df2["scientificName"][df2["scientificName"] == "Poecile_atricapillus"] <- "Parus_atricapillus"
  df2["scientificName"][df2["scientificName"] == "Poecile_gambeli"] <- "Parus_gambeli"
  df2["scientificName"][df2["scientificName"] == "Poecile_hudsonicus"] <- "Parus_hudsonicus"
  df2["scientificName"][df2["scientificName"] == "Setophaga_caerulescens"] <- "Dendroica_caerulescens"
  df2["scientificName"][df2["scientificName"] == "Setophaga_citrina"] <- "Wilsonia_citrina"
  df2["scientificName"][df2["scientificName"] == "Setophaga_coronata"] <- "Dendroica_coronata"
  df2["scientificName"][df2["scientificName"] == "Setophaga_discolor"] <- "Dendroica_discolor"
  df2["scientificName"][df2["scientificName"] == "Setophaga_dominica"] <- "Dendroica_dominica"
  df2["scientificName"][df2["scientificName"] == "Setophaga_fusca"] <- "Dendroica_fusca"
  df2["scientificName"][df2["scientificName"] == "Setophaga_magnolia"] <- "Dendroica_magnolia"
  df2["scientificName"][df2["scientificName"] == "Setophaga_nigrescens"] <- "Dendroica_nigrescens"
  df2["scientificName"][df2["scientificName"] == "Setophaga_occidentalis"] <- "Dendroica_occidentalis"
  df2["scientificName"][df2["scientificName"] == "Setophaga_pensylvanica"] <- "Dendroica_pensylvanica"
  df2["scientificName"][df2["scientificName"] == "Setophaga_petechia"] <- "Dendroica_aestiva"
  df2["scientificName"][df2["scientificName"] == "Setophaga_pinus"] <- "Dendroica_pinus"
  df2["scientificName"][df2["scientificName"] == "Setophaga_striata"] <- "Dendroica_striata"
  df2["scientificName"][df2["scientificName"] == "Setophaga_virens"] <- "Dendroica_virens"
  df2["scientificName"][df2["scientificName"] == "Spinus_lawrencei"] <- "Carduelis_lawrencei"
  df2["scientificName"][df2["scientificName"] == "Spinus_pinus"] <- "Carduelis_pinus"
  df2["scientificName"][df2["scientificName"] == "Spinus_psaltria"] <- "Carduelis_psaltria"
  df2["scientificName"][df2["scientificName"] == "Spinus_tristis"] <- "Carduelis_tristis"
  df2["scientificName"][df2["scientificName"] == "Spizelloides_arborea"] <- "Spizella_arborea"
  
  ##################################################################
  df3<-df2
  list_plotNum <- unique(df3$plotID) # make a list of plotNums 
  
  PD_Null <- matrix(NA,length(list_plotNum),100) #to hold PD from across 100 trees
  colnames(PD_Null)<-c(1:100)
  
  MPD_Null <- matrix(NA,length(list_plotNum),100) #to hold MPD from across 100 trees
  colnames(MPD_Null)<-c(1:100)
  
  for (k in 1:100){	#1:100
    tree <- BirdTree[[k]]
    
    for(i in 1:length(list_plotNum)){ # iterate through the list of each plot
      df_temp <- dplyr::filter(df3, plotID== list_plotNum[i]) # pull out all observations for each plot
      # Data Cleaning to  dumby proof the data
      df_temp <- df_temp[!is.na(df_temp$Abundance),] # remove rows with NA for abundance
      df_temp <- dplyr::filter(df_temp, Abundance>=0.95) # filter out abundances lower than 95%
      df_temp <- df_temp[!duplicated(df_temp$spId.null), ] # remove any duplicated species
      
      # # # # # # # # # # # # # #
      list_sp_names <- unique(df_temp$scientificName) # list of species per plot: used to subset trees
      
      if(length(list_sp_names)>1){
        # Create dataframe of single site presences to be used in 
        #temp <- c(rep(1,length(list_sp_names)))
        
        temp <- c(round(df_temp$Abundance,0)) # Will rounding modify results??
        temp2<- rbind(list_sp_names, temp)
        temp2<- as.data.frame(temp2)
        # temp2<- as.matrix(temp2)
        colnames(temp2) <- temp2[1,]
        temp2 <- temp2[-1, ] 
        
        # # # # # # # # # # #
        # Create subset tree
        pruned.tree<-keep.tip(tree, tree$tip.label[match(list_sp_names, tree$tip.label)])
        
        # # # # # # # # # # #
        # Faith’s Phylogenetic Diversity: Sum of branch lengths
        PD_Null[i,k] <- sum(pruned.tree$edge.length) # Sum the branch length
        
        # # # # # # # # # # #
        # Mean pairwise distance: Average branch lengths between closest relatives
        #create a distance matrix based on branch lengths between species pairs
        dist.mat <- cophenetic.phylo(pruned.tree) 
        #dist.mat_2 <- round(cophenetic.phylo(pruned.tree),0)
        mpd.out <- mpd(temp2, dist.mat, abundance.weighted = F)
        # mpd.out2 <- mpd(temp2, dist.mat_2, abundance.weighted = T)
        # mean(dist.mat) # Assuming this is just the average of distance matrix
        
        MPD_Null[i,k] <- mpd.out
      } 
      #else{
      # PD[i,k] <- 0
      #MPD[i,k] <- 0
      #}
    }
    
  }
  # At this point in the loop, a complete null community has been run for 100 trees 
  
  # calculate mean values across the 100 trees
  PD_Null_mean <- rowMeans(PD_Null[,1:100])
  PD_Null2<-cbind(plotID=list_plotNum, PD_Null, PD_Null_mean)
  
  MPD_Null_mean <- rowMeans(MPD_Null[,1:100])
  MPD_Null2<-cbind(plotID=list_plotNum, MPD_Null, MPD_Null_mean)
  
  # Write to both CSV and RDS file formats
  write.csv(PD_Null2, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/100Null_100Trees/CSV/PD_Null_",j,".csv"))
  saveRDS(PD_Null2, file=paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/100Null_100Trees/RDS/PD_Null_",j,".rds"))
  
  write.csv(MPD_Null2, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/100Null_100Trees/CSV/MPD_Null_",j,".csv"))
  saveRDS(MPD_Null2, file=paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/100Null_100Trees/RDS/MPD_Null_",j,".rds"))
  
  # Enter data into column for mean values of each null community 
  # Average values are the result of 100 random trees that have been averaged by site
  PD_Null_means[,j+1] <- PD_Null_mean # populate column 
  MPD_Null_means[,j+1] <- MPD_Null_mean
  
}


View(PD_Null_means)

# Save final outputs 
write.csv(PD_Null_means, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/PD_Null_means.csv"))
saveRDS(PD_Null_means, file=paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/PD_Null_means.rds"))

write.csv(MPD_Null_means, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/MPD_Null_means.csv"))
saveRDS(MPD_Null_means, file=paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/PD/MPD_Null_means.rds"))

# Combine Null data with real values
MPD2<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Phylogenetic/MPD_100trees_noweighting_95distsamp_rangefilter.csv")
PD2 <- read.csv( "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Phylogenetic/FaithsPD_100trees_noweighting_95distsamp_rangefilter.csv")

# MPD2 and PD2 are in alphabetical order/plotNum order
# Repair data so the columns can be matched up with the Null Data
Key <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/PlotNum_Key.csv")

MPD3 <- as.data.frame(cbind(list_plotNum=Key$plotID, MPD_mean=MPD2$MPD_mean))
PD3 <- as.data.frame(cbind(list_plotNum=Key$plotID, PD_mean=PD2$PD_mean))

head(MPD3)
tail(MPD3)



# PD # # # # # # 
PD_Null_means # null values 
PD2 # original values

# scrap_PD <- as.matrix(cbind(PD_Null_means[,1], original_means=as.data.frame(PD2)$PD_mean ,PD_Null_means[,2:ncol(PD_Null_means)]))
PD_Null_means2 <- as.data.frame(PD_Null_means)
# change_columns <- c(colnames(PD_Null_means2[2:101])) 

scrap_PD <- merge (PD3, PD_Null_means2, by="list_plotNum")%>% remove_rownames %>% column_to_rownames(var="list_plotNum")

# Convert columns from character to 'double'
scrap_PD2 <- as.data.table(scrap_PD)                               # Duplicate data table
is.data.table(scrap_PD2)
change_columns <- c(colnames(scrap_PD2)) 
scrap_PD2[ ,                                    # Change class of certain columns
           (change_columns) := lapply(.SD, as.numeric),
           .SDcols = change_columns]

# PD_obs.null.output <- as.matrix(scrap_PD[,2:(ncol(scrap_PD))]) # the first column of values is the observed value, the other columns are random iterations' values, ##rows are for each site (bin).

PD_obs.null.output <- as.matrix(scrap_PD2) # the first column of values is the observed value, the other columns are random iterations' values, ##rows are for each site (bin).

# Test to see if this works. 
typeof(obs.null.output) # just to see if if matches the other data
typeof(PD_obs.null.output) # IF THIS IS 'double', THEN IT WORKS. 


# MPD # # # # # # 
MPD_Null_means # Null Values
MPD3 # Original Data with plot names

MPD_Null_means2 <- as.data.frame(MPD_Null_means)

# without first column 
scrap_MPD <- merge (MPD3, MPD_Null_means2, by="list_plotNum")%>% remove_rownames %>% column_to_rownames(var="list_plotNum")

# Convert columns from character to 'double'
scrap_MPD2 <- as.data.table(scrap_MPD)                               # Duplicate data table
is.data.table(scrap_MPD2)
change_columns <- c(colnames(scrap_MPD2)) 
scrap_MPD2[ ,                                    # Change class of certain columns
           (change_columns) := lapply(.SD, as.numeric),
           .SDcols = change_columns]

MPD_obs.null.output <- as.matrix(scrap_MPD2) # the first column of values is the observed value, the other columns are random iterations' values, ##rows are for each site (bin).
# input for SES calculations

################################################################################
# Now use Null values to calculate SES and p-values for Phylogenetic 
################################################################################
## Rank observed value, calculate SES and p value
PD.obs <- cbind(plotID=PD_Null_means[,1], origianal_means=as.data.frame(PD2)$PD_mean)
MPD.obs <- cbind(plotID=MPD_Null_means[,1], origianal_means=as.data.frame(MPD2)$MPD_mean)

PD.obs <- as.data.frame(PD.obs)
MPD.obs <- as.data.frame(MPD.obs)
#quantify the rank or the observed value in the null distribution for each community

# PD
PD.obs$PD.obs.rank <- apply(PD_obs.null.output, MARGIN = 1, rank)[1,]
# MPD
MPD.obs$MPD.obs.rank <- apply(MPD_obs.null.output, MARGIN = 1, rank)[1,]

#values range from 1 to the the number of iterations run +1

##### SES 
#quantify the Standardized Effect Size;  SES = (observed - null mean) / null sd
#typeof(obs.null.output)
#typeof(PD_obs.null.output)

# test$SES.FRic <- (obs.null.output[,1] - apply(obs.null.output[,2:(ncol(obs.null.output))], 1, mean)) / apply(obs.null.output[,2:(ncol(obs.null.output))], 1, sd)

# PD
PD.obs$SES.PD <- (PD_obs.null.output[,1] - apply(PD_obs.null.output[,2:(ncol(PD_obs.null.output))], 1, mean)) / apply(PD_obs.null.output[,2:(ncol(PD_obs.null.output))], 1, sd)

# MPD
MPD.obs$SES.MPD <- (MPD_obs.null.output[,1] - apply(MPD_obs.null.output[,2:(ncol(MPD_obs.null.output))], 1, mean)) / apply(MPD_obs.null.output[,2:(ncol(MPD_obs.null.output))], 1, sd)


View(MPD.obs)

################################################################################
#calculate p values 
PD.obs$PD.p <- apply(cbind(PD_obs.null.output[,1], PD_obs.null.output[,2:(ncol(PD_obs.null.output))]), MARGIN = 1, rank)[1,] / 101 

MPD.obs$MPD.p <- apply(cbind(MPD_obs.null.output[,1], MPD_obs.null.output[,2:(ncol(MPD_obs.null.output))]), MARGIN = 1, rank)[1,] / 101 

head(PD.obs)
summary(PD.obs)
hist(PD.obs$PD_mean)
hist(PD.obs$PD.obs.rank)
hist(PD.obs$SES.PD)

head(MPD.obs)
summary(MPD.obs)
hist(MPD.obs$MPD_mean)
hist(MPD.obs$MPD.obs.rank)
hist(MPD.obs$SES.MPD)

View(MPD.obs)
View(PD.obs)

PD_obs.null.output[29,]

summary(MPD_obs.null.output)

summary(t(MPD_obs.null.output))
MPD_obs.null.output[29,]

################################################################################
# Save outputs
write.csv(PD.obs, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_PD_Chap1.csv")
write.csv(MPD.obs, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_MPD_Chap1.csv")

################################################################################









##################################################################
# DATA QUAILITY CHECK # # # # # # #
# this step is just to make sure the two datasets align in length. 
temp_test <-trait_data$scientificName # Trait Data
temp2_test <- colnames(data13) # Abundance Data
length(temp_test) # 258
length(temp2_test) # 258

temp_test <- sort(temp_test) #traits 
temp2_test <- sort(temp2_test) #abundance data

temp3_test<- cbind(temp_test, temp2_test)
temp3_test # Make sure the data lines up
# # # # # # # # # # # # # # # # # #

############----------------------------------------------------







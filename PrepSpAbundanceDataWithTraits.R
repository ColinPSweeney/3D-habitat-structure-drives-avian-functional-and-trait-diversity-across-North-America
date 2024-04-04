################################################################################
# Create Data Frames for Functional Diversity Metrics
################################################################################
library(tidyverse)
################################################################################
data <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter.csv")
Key <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/Key_spec_scinames.csv")
head(Key)
list<-Key$spec

sp_index <- colnames(data)
sp_index <- sp_index[7:266]

##############-----------------------------------------------------------------
#data4 <-data3
#unique(data4$spec)
data4<-c()
for(i in 1:260){
data2 <-dplyr::select(data,plotNum,domainID,siteID,plotID,decimalLatitude,decimalLongitude,abundance=sp_index[i])

spec<-rep(c(list[i]),times=length(data2$plotNum))
data3<-cbind(data2, spec)
data4<-rbind(data4,data3)
}

View(data4)

########
data5 <- right_join(data4, Key, by="spec")
head(data5)
View(data5)
length(unique(data5$scientificName)) #260

write_csv(data5, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter_transposed_sciname.csv")

########
# data6<- na.omit(data5) #remove NA rows
data6 <- data5[!is.na(data5$plotNum),]

length(unique(data6$scientificName)) #260
length(data5$plotNum)
length(data6$plotNum)
head(data6)
tail(data6)

##############-----------------------------------------------------------------
# Add Elton Traits: Body Mass, Foraging Stratum, Diet 
Elton <-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/Elton/Elton_Birds_reduced.csv")
length(Elton)
colnames(Elton)[27] ="scientificName"

head(Elton)
unique(Elton$scientificName)

Elton2 <-Elton

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

##############-----------------------------------------------------------------
data7<-right_join(data6, Elton, by="scientificName")

head(data7)
tail(data7)
data8<- data7[!is.na(data7$plotNum),] # remove possible mismatches 

head(data8)
tail(data8)
length(unique(data8$scientificName)) #260

# 
data9<-right_join(data8, AVONET4, by="scientificName")
tail(data9)
length(unique(data9$scientificName))

data10<- data9[!is.na(data9$plotNum),]
tail(data10)
length(unique(data10$scientificName))#260

data10$plotNum%>%count()

write_csv(data10, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter_transposed_traits2.csv")
View(data10)
##################################################################################



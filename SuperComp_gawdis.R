###########################
# File to create FD metrics using the output of super computer distance sampling and trait data, using gawdis with code adapted from Brooks and Marta
###########################
library(FD)
library(gawdis)
library(tidyverse)
library(funrar)
##########################
df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_2017_df_update_DistCorrAbund20000_SpRangeNEONDomainOverlap.csv")
df<-df[,c(1,7:266)]
df<-as.data.frame(df)
df<-df %>% remove_rownames %>% column_to_rownames(var="plotNum")

df_test <- df
##################################################################
# Trait Data 
# 260 species (ones with matching names to the abundance sci names)
data10<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter_transposed_traits2.csv")
colnames(data10)
length(unique(data10$spec))
unique(data10$scientificName)
##################################################################
Key <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/Key_spec_scinames.csv")
KeyB <- Key
Key_spec <-Key$spec
Key_spec2 <- paste0("V",Key_spec)

KeyB[,1]<-Key_spec2

colnames(df_test) <- Key$scientificName
df_test # Data frame of species abundances for each plot, with sci names of species

#################################################################
# Subset trait data to include one copy of traits per scientific name
trait_data <- dplyr::select(data10, scientificName,Diet.Inv,Diet.Vend,Diet.Vect,Diet.Vfish,Diet.Vunk,Diet.Scav,Diet.Fruit,Diet.Nect,Diet.Seed,Diet.PlantO,ForStrat.watbelowsurf,ForStrat.wataroundsurf,ForStrat.ground,ForStrat.understory,ForStrat.midhigh,ForStrat.canopy,ForStrat.aerial,PelagicSpecialist,BodyMass.Value,Beak.Width,Beak.Depth,Beak.Length_Culmen,Hand.wing.Index)

trait_data<-trait_data[!duplicated(trait_data$scientificName), ]

length(trait_data$scientificName)  # 260  

# write_csv(trait_data, "/Users/colinsweeney/Documents/Documents/PhD/Traits/chapter1_traits/chapter1_260sp_traits_elton_avonet.csv")

##################################################################
temp <-trait_data$scientificName
temp2 <- colnames(df_test)
length(temp) # 260
length(temp2) # 260

temp <- sort(temp) #traits 
temp2 <- sort(temp2) #abundance data

temp3<- cbind(temp, temp2)
temp3 # Make sure the data lines up


############----------------------------------------------------
# Adapt Brook's code
# Trait.groups = c(1, rep(2, 7), rep(3, 3), 4, rep(5, 7), 4, 6, 7, 8, 9, 10)
trait_data2 <-trait_data%>% remove_rownames %>%tibble::column_to_rownames(var="scientificName")

head(trait_data)
head(trait_data2)

trait_data$scientificName
colnames(trait_data)
#use gawdis for distance matrix calculation to improve the equal weighting of traits/trait groupings

#designate trait groupings for gawdis (groups = different values)
# Trait groups: Diet (10), ForStrat (8), BodyMass, Beak, Hand-wing Index

#Trait.groups = c(1, rep(2, 10), rep(3, 8), 4, rep(5, 3), 6) # 1 is sci name
Trait.groups = c(rep(1, 10), rep(2, 8), 3, rep(4, 3), 5)

funcdatcols <- colnames(trait_data2)
cbind.data.frame(funcdatcols, Trait.groups)

#identify the trait groups that represent fuzzy coding or dummy variables - values matching the group values above.

fuzzy.groups = c(1, 2, 4)  # Diet, ForStrat, Beak

#create functional dissimilarity matrix with equal contribution of traits and trait groups (fuzzy coded categories and log mass/head-body length (corr ~ 95%)) via gawdis
#must use optimized to avoid the negative weights of a few traits

df_test
trait_data2 # funcdat for Brook's code

#############################################
### Final Data prep: remove zero abundance species (grass wren)
df_test2 <- within(df_test, rm('Cistothorus platensis')) # remove species with zero abundance
df_test2[is.na(df_test2)] = 0 # replace NAs with 0
ncol(df_test)
ncol(df_test2)
###
row_names_df_to_remove <- c("Cistothorus platensis")
trait_data3 <- trait_data2[!(row.names(trait_data2) %in% row_names_df_to_remove),]
nrow(trait_data2)
nrow(trait_data3)

#####################################################################################

funcdist <- gawdis(trait_data3, 
                   w.type = "optimized", 
                   groups = Trait.groups, 
                   fuzzy = fuzzy.groups, 
                   opti.maxiter = 300) #300 is the default

# saveRDS(funcdist, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/gawdis_distmat_chp1_super20000.rds")
funcdist <- readRDS("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/gawdis_distmat_chp1_super20000.rds") 

round(funcdist, 3)
# opti.maxiter: max number of itterations to run before the GA search is halted
# Type of used method. w.type = "analytic" (default option) – weights optimized by a mathematical algorithm (no NAs are allowed in this option); w.type = "optimized" – weights optimized by genetic/optimization algorithm based on iteractions; w.type = "equal" – equal weights, w.type = "user" – user defined weights are used. Note that is w.type = "analytic" in case of NAs, the function will apply w.type = "equal".

#check out result
attr(funcdist, "correls")  #initial weights
attr(funcdist, "weights") #weights finally given to traits
attr(funcdist, "group.correls") #weights of each group

plot(attr(funcdist, "correls"))
plot(attr(funcdist, "weights"))
plot(attr(funcdist, "group.correls"))

################################################################################
#### Functional Diversity Calculations -----------------------------------------
# Original way: FD package
?dbFD

df_test3<-df_test2

df_test3<- make_relative(as.matrix(df_test3)) # make abundance relative (sums to 1 per row)

sum(df_test2[1,])
sum(df_test3[1,])

dbfd_output <- dbFD(funcdist, df_test3, 
                    corr = "lingoes", 
                    #ord = c("podani", "metric"),
                    w.abun = T,
                    calc.FGR = F, 
                    calc.CWM = F, 
                    print.pco = F,
                    m=5)

dbfd_output2 <- dbFD(funcdist, df_test3,
                    w.abun = T,
                    calc.FGR = F, 
                    calc.CWM = F, 
                    print.pco = F,
                    m=5)

saveRDS(dbfd_output, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/dbfd_output_chp1_super20000.rds")

saveRDS(dbfd_output2, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/dbfd_output_chp1_super20000_sqrt.rds")

dbfd_output$FEve
dbfd_output$FDiv
dbfd_output$FRic

# Trial <- dbFD(trait_data3, df_test2, calc.FGR = F, calc.CWM = F, print.pco = F)
# head(Trial$FEve)
# View(Trial$FEve)
#### ---------------------------------------------------------------------------
################################################################################


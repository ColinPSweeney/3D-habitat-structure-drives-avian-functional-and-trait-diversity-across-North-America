#Load packages
require(ggplot2)
require(reshape2)
require(dplyr)
require(tidyverse)
require(devtools)
require(maps)
require(maptools)
require(rgdal)
require(grid)
require(viridis)
library(devtools)
require(neonDivData)

###get SR for each plotID
birds <- neonDivData::data_bird
#choose 2017 data

birds <- birds %>% dplyr::mutate(year = lubridate::year(observation_datetime), 
                                 month = lubridate::month(observation_datetime), 
                                 day = lubridate::day(observation_datetime))
birds2017 <- birds %>% dplyr::filter(year == 2017)%>% dplyr::filter(taxon_rank == "species")

birdsPOINTS<- birds2017%>% dplyr::filter(pointID == "21") # All points (non-grid sites)

#birds.harv <- birds2017%>% dplyr::filter(siteID == "HARV")%>% dplyr::filter(pointID == "B2")

###################################################################
## GRID sites only
# Generate a list of species level abundances for each point
birds.GRID <- birds2017%>% dplyr::filter(siteID =="HARV"| siteID == "OSBS"|siteID =="UNDE"|siteID =="KONZ"|siteID =="TALL"|siteID =="WOOD"|siteID =="CPER"|siteID =="CLBJ"|siteID =="NIWO"|siteID =="SRER"|siteID =="ONAQ"|siteID =="SJER"|siteID =="BONA"|siteID =="BART"|siteID =="DSNY"|siteID =="JERC"|siteID =="STEI"|siteID =="UKFS"|siteID =="GRSM"|siteID =="RMNP"|siteID =="OAES"|siteID =="MOAB"|siteID =="JORN"|siteID =="TEAK"|siteID =="BARR"|siteID =="DEJU"|siteID =="HEAL")%>% dplyr::filter(pointID == "B2")

sum1_GRID <- birds.GRID %>% group_by(plotID,taxon_name) %>% summarize(count = n()) %>% ungroup()
sum1_GRID <- sum1_GRID%>% dplyr::mutate(Species = taxon_name)
####
# Now merge with trait data. 
# AVONET
AVONET <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/AVONET/ELEData/TraitData/AVONET1_BirdLife.csv")

View(AVONET)
AVONET <- AVONET %>% dplyr::mutate(Species = Species1)

AVONET_trait_data <- AVONET %>% dplyr::select(Species,Hand.Wing.Index,Mass,Beak.Length_Culmen, Beak.Length_Nares, Beak.Width, Beak.Depth,Tarsus.Length, Wing.Length, Tail.Length, Habitat,Trophic.Level, Trophic.Niche, Primary.Lifestyle,Range.Size)

test <- sum1_GRID%>% right_join(AVONET_trait_data, by = "Species")
View(test)

head(test)
summary(test$count)

test2 <- test %>% filter(!is.na(plotID))
summary(test2$count)
View(test2) # This now has all trait data associated with 2017 species for grid sites


################################################
remotes::install_github("RS-eco/traitdata")
library(traitdata)
data(elton_birds)
View(elton_birds)
head(elton_birds)

elton_birds <- elton_birds %>% dplyr::mutate(Species = scientificNameStd)

elton_birds_trait_data <- elton_birds %>% dplyr::select(Species,Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav,Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO, Diet.5Cat,ForStrat.watbelowsurf, ForStrat.wataroundsurf, ForStrat.ground, ForStrat.understory, ForStrat.midhigh,ForStrat.canopy, ForStrat.aerial, PelagicSpecialist,Nocturnal, BodyMass.Value,BodyMass.SpecLevel)

head(elton_birds_trait_data)

test3 <- test2%>% right_join(elton_birds_trait_data, by = "Species")
summary(test3$count)
test4 <- test3 %>% filter(!is.na(plotID))
summary(test4$count)

View(test4)

write_csv(test4, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/2017_AbundanceAndTraitData_GRID.csv")


#

GGally::ggcorr(test4[,c(5:13,18:28,30:39)], label = TRUE, label_alpha = TRUE)


### Points #################### #################### #################### 
## Core
# Domain 2: SCBI -point
birdsPOINTS_SCBI<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SCBI")
attempt_SCBI <- birdsPOINTS_SCBI %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_SCBI <- attempt_SCBI %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_SCBI %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_SCBI<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SCBI")
summary(unique(birdsPOINTS_SCBI$plotID)) 
length(birdsPOINTS_SCBI$siteID)

birdsPOINTS_SCBI<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SCBI")%>% dplyr::filter(unique_sample_id=="SCBI_002.21.2017-05-19T11:06Z[UTC]" |unique_sample_id=="SCBI_003.21.2017-05-16T11:16Z[UTC]"|
unique_sample_id=="SCBI_004.21.2017-05-19T10:37Z[UTC]" |unique_sample_id=="SCBI_005.21.2017-05-18T11:07Z[UTC]"|
unique_sample_id=="SCBI_006.21.2017-05-19T10:15Z[UTC]" |unique_sample_id=="SCBI_007.21.2017-05-16T12:16Z[UTC]"|
unique_sample_id=="SCBI_008.21.2017-05-15T10:20Z[UTC]" |unique_sample_id=="SCBI_010.21.2017-05-16T10:48Z[UTC]"|
unique_sample_id=="SCBI_011.21.2017-05-15T11:33Z[UTC]" |unique_sample_id=="SCBI_012.21.2017-05-15T12:05Z[UTC]"|
unique_sample_id== "SCBI_013.21.2017-05-17T11:07Z[UTC]"|unique_sample_id== "SCBI_014.21.2017-05-17T12:02Z[UTC]"|
unique_sample_id== "SCBI_015.21.2017-05-15T10:59Z[UTC]"|unique_sample_id== "SCBI_016.21.2017-05-18T10:42Z[UTC]"|
unique_sample_id== "SCBI_017.21.2017-05-16T11:46Z[UTC]"|unique_sample_id== "SCBI_018.21.2017-06-07T11:07Z[UTC]"|
unique_sample_id== "SCBI_019.21.2017-05-18T11:30Z[UTC]"|unique_sample_id== "SCBI_021.21.2017-05-18T10:17Z[UTC]"|
unique_sample_id== "SCBI_022.21.2017-05-16T10:28Z[UTC]"|unique_sample_id== "SCBI_023.21.2017-05-15T10:37Z[UTC]"|
unique_sample_id== "SCBI_033.21.2017-05-17T12:32Z[UTC]"|unique_sample_id== "SCBI_035.21.2017-05-17T10:26Z[UTC]"|
unique_sample_id== "SCBI_039.21.2017-05-18T12:14Z[UTC]"|unique_sample_id== "SCBI_043.21.2017-05-19T11:31Z[UTC]")

unique(birdsPOINTS_SCBI$plotID) # make sure all site are present
summary(unique(birdsPOINTS_SCBI$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_SCBI, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_SCBI.csv")

# Domain 18:TOOL -point (Mix of single and repeat sampling efforts)
birdsPOINTS_TOOL<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TOOL")
attempt_TOOL <- birdsPOINTS_TOOL %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_TOOL <- attempt_TOOL %>% group_by(unique_sample_id) %>% summarize(count = n())

write_csv(attempt2_sampleID_TOOL, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/MixedSamples/attempt2_sampleID_TOOL.csv")

temp<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/MixedSamples/attempt2_sampleID_TOOL.csv")

temp$unique_sample_id

#
birdsPOINTS_TOOL<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TOOL")
summary(unique(birdsPOINTS_TOOL$plotID)) 
length(birdsPOINTS_TOOL$siteID)

birdsPOINTS_TOOL<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TOOL")%>% dplyr::filter(
unique_sample_id=="TOOL_001.21.2017-06-25T11:06Z[UTC]" |
unique_sample_id=="TOOL_002.21.2017-06-22T11:54Z[UTC]"|
unique_sample_id=="TOOL_003.21.2017-06-23T11:33Z[UTC]" |
unique_sample_id=="TOOL_005.21.2017-06-21T11:35Z[UTC]"|
unique_sample_id=="TOOL_006.21.2017-06-22T14:28Z[UTC]" |
unique_sample_id=="TOOL_007.21.2017-06-22T10:48Z[UTC]"|
unique_sample_id=="TOOL_008.21.2017-06-21T11:12Z[UTC]" |
unique_sample_id=="TOOL_009.21.2017-06-22T12:47Z[UTC]"|
unique_sample_id=="TOOL_010.21.2017-06-24T14:49Z[UTC]" |
unique_sample_id=="TOOL_011.21.2017-06-24T10:34Z[UTC]"|
unique_sample_id== "TOOL_012.21.2017-06-21T13:38Z[UTC]"|
unique_sample_id== "TOOL_013.21.2017-06-22T13:30Z[UTC]"|
unique_sample_id== "TOOL_014.21.2017-06-24T11:54Z[UTC]"|
unique_sample_id== "TOOL_016.21.2017-06-23T14:06Z[UTC]"|
unique_sample_id== "TOOL_017.21.2017-06-24T13:24Z[UTC]"|
unique_sample_id== "TOOL_018.21.2017-06-24T10:55Z[UTC]"|
unique_sample_id== "TOOL_019.21.2017-06-22T11:32Z[UTC]"|
unique_sample_id== "TOOL_020.21.2017-06-23T12:06Z[UTC]"|
unique_sample_id== "TOOL_021.21.2017-06-21T11:57Z[UTC]"|
unique_sample_id== "TOOL_022.21.2017-06-22T11:12Z[UTC]"|
unique_sample_id== "TOOL_024.21.2017-06-21T13:05Z[UTC]"|
unique_sample_id== "TOOL_026.21.2017-06-23T13:41Z[UTC]"|
unique_sample_id== "TOOL_027.21.2017-06-24T11:32Z[UTC]"|
unique_sample_id== "TOOL_028.21.2017-06-21T10:48Z[UTC]"|
unique_sample_id== "TOOL_071.21.2017-06-24T13:55Z[UTC]")
 
unique(birdsPOINTS_TOOL$plotID) # make sure all site are present
summary(unique(birdsPOINTS_TOOL$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_TOOL, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_TOOL.csv")


## Reloc
# Domain 2: BLAN -point
birdsPOINTS_BLAN<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "BLAN")
attempt_BLAN <- birdsPOINTS_BLAN %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_BLAN <- attempt_BLAN %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_BLAN %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_BLAN<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "BLAN")
summary(unique(birdsPOINTS_BLAN$plotID)) 
length(birdsPOINTS_BLAN$siteID)

birdsPOINTS_BLAN<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "BLAN")%>% dplyr::filter(                                                                                                   unique_sample_id=="BLAN_001.21.2017-05-24T11:07Z[UTC]" |
unique_sample_id=="BLAN_002.21.2017-06-01T12:15Z[UTC]"|
unique_sample_id=="BLAN_003.21.2017-06-01T11:16Z[UTC]" |
unique_sample_id=="BLAN_004.21.2017-05-24T10:50Z[UTC]"|
unique_sample_id=="BLAN_005.21.2017-05-24T10:07Z[UTC]" |
unique_sample_id=="BLAN_006.21.2017-06-01T11:47Z[UTC]"|
unique_sample_id=="BLAN_007.21.2017-05-24T10:22Z[UTC]" |
unique_sample_id=="BLAN_013.21.2017-06-05T10:55Z[UTC]"|
unique_sample_id=="BLAN_014.21.2017-06-01T10:38Z[UTC]" |
unique_sample_id=="BLAN_015.21.2017-06-01T10:00Z[UTC]"|
unique_sample_id== "BLAN_018.21.2017-06-01T11:31Z[UTC]"|
unique_sample_id== "BLAN_020.21.2017-06-05T12:04Z[UTC]")

unique(birdsPOINTS_BLAN$plotID) # make sure all site are present
summary(unique(birdsPOINTS_BLAN$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_BLAN, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_BLAN.csv")

# Domain 2: SERC -point
birdsPOINTS_SERC<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SERC")
attempt_SERC <- birdsPOINTS_SERC %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_SERC <- attempt_SERC %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_SERC %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_SERC<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SERC")
summary(unique(birdsPOINTS_SERC$plotID)) 
length(birdsPOINTS_SERC$siteID)

birdsPOINTS_SERC<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SERC")%>% dplyr::filter(
unique_sample_id=="SERC_001.21.2017-05-17T13:11Z[UTC]" |
unique_sample_id=="SERC_002.21.2017-05-16T13:34Z[UTC]"|
unique_sample_id=="SERC_003.21.2017-05-18T11:18Z[UTC]" |
unique_sample_id=="SERC_004.21.2017-05-16T14:00Z[UTC]"|
unique_sample_id=="SERC_005.21.2017-05-16T11:17Z[UTC]" |
unique_sample_id=="SERC_006.21.2017-05-16T10:25Z[UTC]"|
unique_sample_id=="SERC_007.21.2017-05-17T11:14Z[UTC]" |
unique_sample_id=="SERC_008.21.2017-05-17T12:46Z[UTC]"|
unique_sample_id=="SERC_009.21.2017-05-16T13:08Z[UTC]" |
unique_sample_id=="SERC_010.21.2017-05-15T14:00Z[UTC]"|
unique_sample_id== "SERC_011.21.2017-05-16T11:40Z[UTC]"|
unique_sample_id== "SERC_012.21.2017-05-18T13:54Z[UTC]"|
unique_sample_id== "SERC_013.21.2017-05-15T14:30Z[UTC]"|
unique_sample_id== "SERC_014.21.2017-05-17T11:30Z[UTC]"|
unique_sample_id== "SERC_016.21.2017-05-18T11:40Z[UTC]"|
unique_sample_id== "SERC_017.21.2017-05-17T11:55Z[UTC]"|
unique_sample_id== "SERC_018.21.2017-05-17T12:15Z[UTC]"|
unique_sample_id== "SERC_019.21.2017-05-19T13:14Z[UTC]"|
unique_sample_id== "SERC_020.21.2017-05-17T13:34Z[UTC]"|
unique_sample_id== "SERC_022.21.2017-05-16T14:33Z[UTC]"|
unique_sample_id== "SERC_024.21.2017-05-18T13:54Z[UTC]"|
unique_sample_id== "SERC_025.21.2017-05-16T10:41Z[UTC]"|
unique_sample_id== "SERC_026.21.2017-05-16T12:04Z[UTC]"|
unique_sample_id== "SERC_029.21.2017-05-17T12:30Z[UTC]"|
unique_sample_id== "SERC_068.21.2017-05-17T14:35Z[UTC]")
 


unique(birdsPOINTS_SERC$plotID) # make sure all site are present
summary(unique(birdsPOINTS_SERC$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_SERC, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_SERC.csv")

# Domain 5: TREE -point
birdsPOINTS_TREE<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TREE")
attempt_TREE <- birdsPOINTS_TREE %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_TREE <- attempt_TREE %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_TREE %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_TREE<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TREE")
summary(unique(birdsPOINTS_TREE$plotID)) 
length(birdsPOINTS_TREE$siteID)

birdsPOINTS_TREE<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "TREE")%>% dplyr::filter(
unique_sample_id=="TREE_001.21.2017-06-20T12:19Z[UTC]" |
unique_sample_id=="TREE_002.21.2017-06-15T12:17Z[UTC]"|
unique_sample_id=="TREE_003.21.2017-06-20T10:05Z[UTC]" |
unique_sample_id=="TREE_004.21.2017-06-16T10:30Z[UTC]"|
unique_sample_id=="TREE_007.21.2017-06-14T12:20Z[UTC]" |
unique_sample_id=="TREE_010.21.2017-06-15T11:42Z[UTC]"|
unique_sample_id=="TREE_012.21.2017-06-20T11:20Z[UTC]" |
unique_sample_id=="TREE_013.21.2017-06-20T11:45Z[UTC]"|
unique_sample_id=="TREE_015.21.2017-06-14T11:21Z[UTC]" |
unique_sample_id=="TREE_016.21.2017-06-15T11:13Z[UTC]"|
unique_sample_id== "TREE_017.21.2017-06-20T10:29Z[UTC]"|
unique_sample_id== "TREE_019.21.2017-06-20T10:57Z[UTC]"|
unique_sample_id== "TREE_020.21.2017-06-14T11:49Z[UTC]"|
unique_sample_id== "TREE_022.21.2017-06-14T10:28Z[UTC]"|
unique_sample_id== "TREE_023.21.2017-06-14T10:55Z[UTC]"|
unique_sample_id== "TREE_026.21.2017-06-14T10:01Z[UTC]"|
unique_sample_id== "TREE_060.21.2017-06-15T10:36Z[UTC]")


unique(birdsPOINTS_TREE$plotID) # make sure all site are present
summary(unique(birdsPOINTS_TREE$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_TREE, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_TREE.csv")

# Domain 8: DELA -point
birdsPOINTS_DELA<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DELA")
attempt_DELA <- birdsPOINTS_DELA %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_DELA <- attempt_DELA %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_DELA %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_DELA<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DELA")
summary(unique(birdsPOINTS_DELA$plotID)) 
length(birdsPOINTS_DELA$siteID)

birdsPOINTS_DELA<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DELA")%>% dplyr::filter(
unique_sample_id=="DELA_001.21.2017-06-23T12:28Z[UTC]" |
unique_sample_id=="DELA_002.21.2017-06-25T13:40Z[UTC]"|
unique_sample_id=="DELA_003.21.2017-06-23T14:51Z[UTC]" |
unique_sample_id=="DELA_004.21.2017-06-25T13:30Z[UTC]"|
unique_sample_id=="DELA_005.21.2017-06-23T12:26Z[UTC]" |
unique_sample_id=="DELA_006.21.2017-06-23T12:10Z[UTC]"|
unique_sample_id=="DELA_007.21.2017-06-23T11:32Z[UTC]" |
unique_sample_id=="DELA_008.21.2017-06-26T12:01Z[UTC]"|
unique_sample_id=="DELA_010.21.2017-06-23T11:08Z[UTC]" |
unique_sample_id=="DELA_011.21.2017-06-23T12:00Z[UTC]"|
unique_sample_id== "DELA_013.21.2017-06-26T12:36Z[UTC]"|
unique_sample_id== "DELA_014.21.2017-06-23T14:37Z[UTC]"|
unique_sample_id== "DELA_015.21.2017-06-23T13:45Z[UTC]"|
unique_sample_id== "DELA_016.21.2017-06-23T12:54Z[UTC]"|
unique_sample_id== "DELA_021.21.2017-06-25T14:42Z[UTC]"|
unique_sample_id== "DELA_026.21.2017-06-23T14:29Z[UTC]"|
unique_sample_id== "DELA_027.21.2017-06-23T13:40Z[UTC]"|
unique_sample_id== "DELA_029.21.2017-06-23T14:02Z[UTC]")

unique(birdsPOINTS_DELA$plotID) # make sure all site are present
summary(unique(birdsPOINTS_DELA$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_DELA, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_DELA.csv")

# Domain 8: LENO -point (ONLY one sample period, don't cut rows)
#
birdsPOINTS_LENO<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "LENO")
summary(unique(birdsPOINTS_LENO$plotID)) 
length(birdsPOINTS_LENO$siteID)

write_csv(birdsPOINTS_LENO, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_LENO.csv")

# Domain 9: DCFS -point
birdsPOINTS_DCFS<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DCFS")
attempt_DCFS <- birdsPOINTS_DCFS %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_DCFS <- attempt_DCFS %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_DCFS %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_DCFS<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DCFS")
summary(unique(birdsPOINTS_DCFS$plotID)) 
length(birdsPOINTS_DCFS$siteID)

birdsPOINTS_DCFS<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "DCFS")%>% dplyr::filter(
unique_sample_id=="DCFS_001.21.2017-06-27T13:45Z[UTC]" |
unique_sample_id=="DCFS_003.21.2017-06-27T11:39Z[UTC]"|
unique_sample_id=="DCFS_004.21.2017-06-28T11:02Z[UTC]" |
unique_sample_id=="DCFS_005.21.2017-06-27T12:45Z[UTC]"|
unique_sample_id=="DCFS_007.21.2017-06-28T12:24Z[UTC]" |
unique_sample_id=="DCFS_009.21.2017-06-27T11:08Z[UTC]"|
unique_sample_id=="DCFS_010.21.2017-06-27T14:06Z[UTC]" |
unique_sample_id=="DCFS_011.21.2017-06-27T15:09Z[UTC]"|
unique_sample_id=="DCFS_012.21.2017-06-28T11:51Z[UTC]" |
unique_sample_id=="DCFS_013.21.2017-06-27T14:34Z[UTC]"|
unique_sample_id== "DCFS_014.21.2017-06-28T14:54Z[UTC]"|
unique_sample_id== "DCFS_015.21.2017-06-28T14:00Z[UTC]"|
unique_sample_id== "DCFS_016.21.2017-06-28T13:09Z[UTC]"|
unique_sample_id== "DCFS_017.21.2017-06-28T13:35Z[UTC]"|
unique_sample_id== "DCFS_018.21.2017-06-27T13:15Z[UTC]"|
unique_sample_id== "DCFS_019.21.2017-06-27T12:18Z[UTC]"|
unique_sample_id== "DCFS_024.21.2017-06-28T10:44Z[UTC]"|
unique_sample_id== "DCFS_027.21.2017-06-27T14:48Z[UTC]"|
unique_sample_id== "DCFS_028.21.2017-06-28T11:20Z[UTC]"|
unique_sample_id== "DCFS_030.21.2017-06-28T11:34Z[UTC]")

unique(birdsPOINTS_DCFS$plotID) # make sure all site are present
summary(unique(birdsPOINTS_DCFS$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_DCFS, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_DCFS.csv")

# Domain 9: NOGP -point
birdsPOINTS_NOGP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "NOGP")
attempt_NOGP <- birdsPOINTS_NOGP %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_NOGP <- attempt_NOGP %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_NOGP %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_NOGP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "NOGP")
summary(unique(birdsPOINTS_NOGP$plotID)) 
length(birdsPOINTS_NOGP$siteID)

birdsPOINTS_NOGP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "NOGP")%>% dplyr::filter(
unique_sample_id=="NOGP_001.21.2017-07-09T14:33Z[UTC]" |
unique_sample_id=="NOGP_002.21.2017-07-08T12:03Z[UTC]"|
unique_sample_id=="NOGP_003.21.2017-07-09T14:56Z[UTC]" |
unique_sample_id=="NOGP_005.21.2017-07-08T13:31Z[UTC]"|
unique_sample_id=="NOGP_007.21.2017-07-08T10:39Z[UTC]" |
unique_sample_id=="NOGP_008.21.2017-07-09T11:00Z[UTC]"|
unique_sample_id=="NOGP_010.21.2017-07-09T13:37Z[UTC]" |
unique_sample_id=="NOGP_013.21.2017-07-09T12:45Z[UTC]"|
unique_sample_id=="NOGP_014.21.2017-07-08T10:57Z[UTC]" |
unique_sample_id=="NOGP_015.21.2017-07-08T13:13Z[UTC]"|
unique_sample_id== "NOGP_016.21.2017-07-09T11:47Z[UTC]"|
unique_sample_id== "NOGP_017.21.2017-07-08T13:52Z[UTC]"|
unique_sample_id== "NOGP_018.21.2017-07-09T13:54Z[UTC]"|
unique_sample_id== "NOGP_019.21.2017-07-08T12:36Z[UTC]"|
unique_sample_id== "NOGP_020.21.2017-07-09T11:18Z[UTC]"|
unique_sample_id== "NOGP_022.21.2017-07-08T11:34Z[UTC]"|
unique_sample_id== "NOGP_027.21.2017-07-08T12:52Z[UTC]"|
unique_sample_id== "NOGP_028.21.2017-07-08T11:17Z[UTC]"|
unique_sample_id== "NOGP_029.21.2017-07-08T14:09Z[UTC]"|
unique_sample_id== "NOGP_041.21.2017-07-09T12:13Z[UTC]")

unique(birdsPOINTS_NOGP$plotID) # make sure all site are present
summary(unique(birdsPOINTS_NOGP$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_NOGP, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_NOGP.csv")

# Domain 10: STER -point (possibly weird)
birdsPOINTS_STER<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "STER")
attempt_STER <- birdsPOINTS_STER %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_STER <- attempt_STER %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_STER %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_STER<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "STER")
summary(unique(birdsPOINTS_STER$plotID)) 
length(birdsPOINTS_STER$siteID)

birdsPOINTS_STER<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "STER")%>% dplyr::filter(
unique_sample_id== "STER_005.21.2017-05-17T15:23Z[UTC]" |
unique_sample_id== "STER_006.21.2017-05-17T15:42Z[UTC]"|
unique_sample_id== "STER_026.21.2017-05-17T15:02Z[UTC]" |
unique_sample_id== "STER_027.21.2017-05-17T13:28Z[UTC]"|
unique_sample_id== "STER_028.21.2017-05-17T13:02Z[UTC]" |
unique_sample_id== "STER_029.21.2017-05-17T14:12Z[UTC]"|
unique_sample_id== "STER_032.21.2017-05-17T14:51Z[UTC]" |
unique_sample_id== "STER_033.21.2017-05-17T13:48Z[UTC]"|
unique_sample_id== "STER_035.21.2017-05-17T12:44Z[UTC]")


unique(birdsPOINTS_STER$plotID) # make sure all site are present
summary(unique(birdsPOINTS_STER$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_STER, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_STER.csv")

# Domain 16: ABBY -point (ABBY 30 has one replicate)
birdsPOINTS_ABBY<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "ABBY")
attempt_ABBY <- birdsPOINTS_ABBY %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_ABBY <- attempt_ABBY %>% group_by(unique_sample_id) %>% summarize(count = n())

write_csv(attempt2_sampleID_ABBY , "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/MixedSamples/birdsPOINTS_ABBY.csv")
temp<-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/MixedSamples/birdsPOINTS_ABBY.csv")
temp$unique_sample_id

#
birdsPOINTS_ABBY<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "ABBY")
summary(unique(birdsPOINTS_ABBY$plotID)) 
length(birdsPOINTS_ABBY$siteID)

birdsPOINTS_ABBY<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "ABBY")%>% dplyr::filter(unique_sample_id=="ABBY_002.21.2017-05-26T14:35Z[UTC]" |
unique_sample_id=="ABBY_003.21.2017-05-27T16:09Z[UTC]"|
unique_sample_id=="ABBY_004.21.2017-05-26T16:06Z[UTC]" |
unique_sample_id=="ABBY_005.21.2017-05-27T14:41Z[UTC]"|
unique_sample_id=="ABBY_007.21.2017-05-27T16:31Z[UTC]" |
unique_sample_id=="ABBY_008.21.2017-05-27T16:54Z[UTC]"|
unique_sample_id=="ABBY_009.21.2017-05-26T16:23Z[UTC]" |
unique_sample_id=="ABBY_010.21.2017-05-27T15:14Z[UTC]"|
unique_sample_id=="ABBY_011.21.2017-05-27T12:12Z[UTC]" |
unique_sample_id=="ABBY_012.21.2017-05-27T13:44Z[UTC]"|
unique_sample_id== "ABBY_013.21.2017-05-27T12:38Z[UTC]"|
unique_sample_id== "ABBY_014.21.2017-05-26T13:51Z[UTC]"|
unique_sample_id== "ABBY_015.21.2017-05-26T14:53Z[UTC]"|
unique_sample_id== "ABBY_016.21.2017-05-26T13:21Z[UTC]"|
unique_sample_id== "ABBY_017.21.2017-05-27T15:42Z[UTC]"|
unique_sample_id== "ABBY_018.21.2017-05-27T14:13Z[UTC]"|
unique_sample_id== "ABBY_021.21.2017-05-26T14:15Z[UTC]"|
unique_sample_id== "ABBY_022.21.2017-05-26T12:50Z[UTC]"|
unique_sample_id== "ABBY_023.21.2017-05-27T13:10Z[UTC]"|
unique_sample_id== "ABBY_027.21.2017-05-26T15:45Z[UTC]"|
unique_sample_id== "ABBY_029.21.2017-05-26T16:58Z[UTC]"|
unique_sample_id== "ABBY_030.21.2017-06-04T17:10Z[UTC]")

unique(birdsPOINTS_ABBY$plotID) # make sure all site are present
summary(unique(birdsPOINTS_ABBY$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_ABBY, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_ABBY.csv")

# Domain 17: SOAP -point
birdsPOINTS_SOAP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SOAP")
attempt_SOAP <- birdsPOINTS_SOAP %>% group_by(unique_sample_id,taxon_name)%>% summarize(count = n()) %>% ungroup()
attempt2_sampleID_SOAP <- attempt_SOAP %>% group_by(unique_sample_id) %>% summarize(count = n())

temp<- attempt2_sampleID_SOAP %>%slice(which(row_number() %% 2 == 1))

temp$unique_sample_id

#
birdsPOINTS_SOAP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SOAP")
summary(unique(birdsPOINTS_SOAP$plotID)) 
length(birdsPOINTS_SOAP$siteID)

birdsPOINTS_SOAP<- birds2017%>% dplyr::filter(pointID == "21")%>% dplyr::filter(siteID == "SOAP")%>% dplyr::filter(unique_sample_id=="SOAP_001.21.2017-05-16T16:08Z[UTC]" |
unique_sample_id=="SOAP_002.21.2017-05-16T13:08Z[UTC]"|
unique_sample_id=="SOAP_003.21.2017-05-16T14:32Z[UTC]" |
unique_sample_id=="SOAP_004.21.2017-05-16T16:47Z[UTC]"|
unique_sample_id=="SOAP_005.21.2017-05-16T14:55Z[UTC]" |
unique_sample_id=="SOAP_006.21.2017-05-16T13:29Z[UTC]"|
unique_sample_id=="SOAP_007.21.2017-05-16T12:43Z[UTC]" |
unique_sample_id=="SOAP_008.21.2017-05-16T14:37Z[UTC]"|
unique_sample_id=="SOAP_009.21.2017-05-16T15:54Z[UTC]" |
unique_sample_id=="SOAP_010.21.2017-05-16T15:48Z[UTC]"|
unique_sample_id== "SOAP_011.21.2017-05-16T15:27Z[UTC]"|
unique_sample_id== "SOAP_015.21.2017-05-16T16:21Z[UTC]"|
unique_sample_id== "SOAP_016.21.2017-05-16T12:22Z[UTC]"|
unique_sample_id== "SOAP_017.21.2017-05-16T13:29Z[UTC]"|
unique_sample_id== "SOAP_019.21.2017-05-16T13:08Z[UTC]"|
unique_sample_id== "SOAP_023.21.2017-05-16T14:11Z[UTC]"|
unique_sample_id== "SOAP_026.21.2017-05-16T15:18Z[UTC]"|
unique_sample_id== "SOAP_028.21.2017-05-16T14:09Z[UTC]"|
unique_sample_id== "SOAP_030.21.2017-05-16T12:48Z[UTC]")
 

unique(birdsPOINTS_SOAP$plotID) # make sure all site are present
summary(unique(birdsPOINTS_SOAP$plotID)) # should match the number you started with. 

write_csv(birdsPOINTS_SOAP, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/Test/AbundancePoint/birdsPOINTS_SOAP.csv")


###################################################################


birds.POINT <- rbind(birdsPOINTS_SCBI,
                     birdsPOINTS_TOOL,
                     birdsPOINTS_BLAN,
                     birdsPOINTS_SERC,
                     birdsPOINTS_TREE,
                     birdsPOINTS_DELA,
                     birdsPOINTS_LENO,
                     birdsPOINTS_DCFS,
                     birdsPOINTS_NOGP,
                     birdsPOINTS_STER,
                     birdsPOINTS_ABBY,
                     birdsPOINTS_SOAP)

sum1_POINT <- birds.POINT %>% group_by(plotID,taxon_name) %>% summarize(count = n()) %>% ungroup()
sum1_POINT  <- sum1_POINT %>% dplyr::mutate(Species = taxon_name)
####
# Now merge with trait data. 
# AVONET

testB <- sum1_POINT%>% right_join(AVONET_trait_data, by = "Species")
View(testB)

head(testB)
summary(testB$count)

testB2 <- testB %>% filter(!is.na(plotID))
summary(testB2$count)
View(testB2) # This now has all trait data associated with 2017 species for point sites


################################################
testB3 <- testB2%>% right_join(elton_birds_trait_data, by = "Species")
summary(testB3$count)

testB4 <- testB3 %>% filter(!is.na(plotID))
summary(testB4$count)

View(testB4)

write_csv(testB4, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/2017_AbundanceAndTraitData_POINT.csv")

######### Combine it all
FULL_Abundance_2017_Trait <- rbind(test4,testB4)

write_csv(FULL_Abundance_2017_Trait, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/FULL_Abundance_2017_Trait.csv")

######### Combine with LiDAR metrics
All_plot_data_full<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/SiteData/All_plot_data_full.csv")

test5 <- FULL_Abundance_2017_Trait%>% right_join(LiDAR_metrics, by = "plotID", copy=T)
View(test5)
colnames(test5)[3] <- "Abundance"
colnames(test5)[54] <- "SpRich"
write_csv(test5, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/FULL_Abundance_2017_Trait_LiDAR_NEONCovariate.csv")

FULL_Abundance_2017_Trait_LiDAR_NEONCovariate <- test5
All_plot_data_full

##################################################################################
# Data prep for multi-species distance models
##################################################################################
# Download 2017 data for all relevant sites
##################################################################################
library(neonUtilities)
library(tidyverse)


birds2017.temp <- loadByProduct(dpID="DP1.10003.001", # Bird Data DP1.10003.001
                                site=c("BARR","BART", "BONA", "CLBJ", "CPER", "DEJU", "DSNY", "GRSM", "HARV", "HEAL", "JERC", "JORN", "KONZ", "MOAB","NIWO", "OAES", "ONAQ", "OSBS", "RMNP", "SJER", "SRER", "STEI", "TALL", "TEAK", "UKFS", "UNDE", "WOOD", "ABBY", "BLAN", "DCFS", "DELA", "LENO", "NOGP", "SCBI", "SERC", "SOAP", "STER", "TOOL", "TREE"),
                                startdate="2017-01", 
                                enddate="2017-12")

birds2017_count <- birds2017.temp$brd_countdata# Individual count data
birds2017_point <- birds2017.temp$brd_perpoint# Info about plot level variables (grid or single point)

# lubridate
birds2017_count_a <- birds2017_count%>% dplyr::mutate(year = lubridate::year(startDate), 
                                 month = lubridate::month(startDate), 
                                 day = lubridate::day(startDate))

########################## Data Cleaning ####################
# Data cleaning (filter out species ID'd to species level and within distance radius)
birds2017_count_b <- birds2017_count_a%>% dplyr::filter(taxonRank == "species") # also gets rid of no observations (including all pointCountMinutes with no observations). Could be an issue if there are NO observations at all at a site (so far not the case). 

## Data cleaning (filter our nocturnal species)- first combine with trait data
# right join nocturnal column
Elton <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/Elton/Elton_Brids.csv")
Elton <- Elton %>% dplyr::mutate(scientificName = scientificNameStd)
Elton_noct_data <- Elton %>% dplyr::select(scientificName, Nocturnal)

test <- birds2017_count_b%>% right_join(Elton_noct_data, by = "scientificName")
View(test)

test2 <- test %>% filter(!is.na(plotID)) # filter out extra columns from Elton (non-present sp)
# test2 <- test %>% drop_na(plotID) # this should be the same result
View(test2)

# filter out nocturnal species
test3 <- test2%>% filter(Nocturnal=="0")
summary(unique(test2$scientificName))
summary(unique(test3$scientificName)) # only loose 11 nocturnal species (336 remain)

# filter out observations that are further than 250m observation radius
test4 <- test3%>% filter(observerDistance<=250) # will also drop all data with (NA) distances

############### Split data for separate filtering of single point and girded points
# filter for grided sites (central point: B2)
birds2017_count_Grid<- test4%>% dplyr::filter(pointID == "B2")
birds2017_point_Grid<- birds2017_point%>% dplyr::filter(pointID == "B2")

# filter for single point sites
birds2017_count_SinglePts<- test4%>% dplyr::filter(pointID == "21")
birds2017_point_SinglePts<- birds2017_point%>% dplyr::filter(pointID == "21")

####################### Filtering of single points.... it's a pain
# Need to filter out repeat sampling AND filter out points that overlap at a 250 m radius
#
# Filter Both "count" file and "point" data
#### Filter "point" co-variate data
birds2017_point_SinglePts_FirstSample<- birds2017_point_SinglePts[order(birds2017_point_SinglePts$plotID, birds2017_point_SinglePts$startDate), ]%>% dplyr::distinct(plotID, .keep_all = T) # distinct keeps the first entry if using the .keep_all specification. Therefore, if sorted by startDate, only the initial sample period is retained, filtering out the repeat sampling if there is one present.

birds2017_point_SinglePts_FirstSample%>%filter(siteID=="STER")%>%summary() # If the filtering step worked, the length of the summary should be the exact number of total bird points (therefore repeat sampling has been removed, leaving only the initial survey point)
####

###########################
#### Filter "count" data
# Filter out by eventID-combo of plotID and data (taken from export excel file from previous data prep process)

temp <- birds2017_count_SinglePts%>%dplyr::filter(
  # SCBI
  eventID== "SCBI_002.21.2017-05-19"|
  eventID== "SCBI_003.21.2017-05-16"|
  eventID== "SCBI_004.21.2017-05-19"|
  eventID== "SCBI_005.21.2017-05-18"|
  eventID== "SCBI_006.21.2017-05-19"|
  eventID== "SCBI_007.21.2017-05-16"|
  eventID== "SCBI_008.21.2017-05-15"|
  eventID== "SCBI_010.21.2017-05-16"|
  eventID== "SCBI_011.21.2017-05-15"|
  eventID== "SCBI_012.21.2017-05-15"|
  eventID== "SCBI_013.21.2017-05-17"|
  eventID== "SCBI_014.21.2017-05-17"|
  eventID== "SCBI_015.21.2017-05-15"|
  eventID== "SCBI_016.21.2017-05-18"|
  eventID== "SCBI_017.21.2017-05-16"|
  eventID== "SCBI_018.21.2017-06-07"|
  eventID== "SCBI_019.21.2017-05-18"|
  eventID== "SCBI_021.21.2017-05-18"|
  eventID== "SCBI_022.21.2017-05-16"|
  eventID== "SCBI_023.21.2017-05-15"|
  eventID== "SCBI_033.21.2017-05-17"|
  eventID== "SCBI_035.21.2017-05-17"|
  eventID== "SCBI_039.21.2017-05-18"|
  eventID== "SCBI_043.21.2017-05-19"|
# TOOL
  eventID== "TOOL_001.21.2017-06-25"|
  eventID== "TOOL_002.21.2017-06-22"|
  eventID== "TOOL_003.21.2017-06-23"|
  eventID== "TOOL_005.21.2017-06-21"|
  eventID== "TOOL_006.21.2017-06-22"|
  eventID== "TOOL_007.21.2017-06-22"| # Got filtered out (single observation had NA for distance)
  eventID== "TOOL_008.21.2017-06-21"|
  eventID== "TOOL_009.21.2017-06-22"|
  eventID== "TOOL_010.21.2017-06-24"|
  eventID== "TOOL_011.21.2017-06-24"|
  eventID== "TOOL_012.21.2017-06-21"|
  eventID== "TOOL_013.21.2017-06-22"|
  eventID== "TOOL_014.21.2017-06-24"|
  eventID== "TOOL_016.21.2017-06-23"|
  eventID== "TOOL_017.21.2017-06-24"|
  eventID== "TOOL_018.21.2017-06-24"|
  eventID== "TOOL_019.21.2017-06-22"|
  eventID== "TOOL_020.21.2017-06-23"|
  eventID== "TOOL_021.21.2017-06-21"|
  eventID== "TOOL_022.21.2017-06-22"|
  eventID== "TOOL_024.21.2017-06-21"|
  eventID== "TOOL_026.21.2017-06-23"|
  eventID== "TOOL_027.21.2017-06-24"|
  eventID== "TOOL_028.21.2017-06-21"|
  eventID== "TOOL_071.21.2017-06-24"|
# BLAN
  eventID== "BLAN_001.21.2017-05-24"|
  eventID== "BLAN_002.21.2017-06-01"|
  eventID== "BLAN_003.21.2017-06-01"|
  eventID== "BLAN_004.21.2017-05-24"|
  eventID== "BLAN_005.21.2017-05-24"|
  eventID== "BLAN_006.21.2017-06-01"|
  eventID== "BLAN_007.21.2017-05-24"|
  eventID== "BLAN_013.21.2017-06-05"|
  eventID== "BLAN_014.21.2017-06-01"|
  eventID== "BLAN_015.21.2017-06-01"|
  eventID== "BLAN_018.21.2017-06-01"|
  eventID== "BLAN_020.21.2017-06-05"|
# SERC
  eventID== "SERC_001.21.2017-05-17"|
  eventID== "SERC_002.21.2017-05-16"|
  eventID== "SERC_003.21.2017-05-18"|
  eventID== "SERC_004.21.2017-05-16"|
  eventID== "SERC_005.21.2017-05-16"|
  eventID== "SERC_006.21.2017-05-16"|
  eventID== "SERC_007.21.2017-05-17"|
  eventID== "SERC_008.21.2017-05-17"|
  eventID== "SERC_009.21.2017-05-16"|
  eventID== "SERC_010.21.2017-05-15"|
  eventID== "SERC_011.21.2017-05-16"|
  eventID== "SERC_012.21.2017-05-18"|
  eventID== "SERC_013.21.2017-05-15"|
  eventID== "SERC_014.21.2017-05-17"|
  eventID== "SERC_016.21.2017-05-18"|
  eventID== "SERC_017.21.2017-05-17"|
  eventID== "SERC_018.21.2017-05-17"|
  eventID== "SERC_019.21.2017-05-19"|
  eventID== "SERC_020.21.2017-05-17"|
  eventID== "SERC_022.21.2017-05-16"|
  eventID== "SERC_024.21.2017-05-18"|
  eventID== "SERC_025.21.2017-05-16"|
  eventID== "SERC_026.21.2017-05-16"|
  eventID== "SERC_029.21.2017-05-17"|
  eventID== "SERC_068.21.2017-05-17"|
# TREE
  eventID== "TREE_001.21.2017-06-20"|
  eventID== "TREE_002.21.2017-06-15"|
  eventID== "TREE_003.21.2017-06-20"|
  eventID== "TREE_004.21.2017-06-16"|
  eventID== "TREE_007.21.2017-06-14"|
  eventID== "TREE_010.21.2017-06-15"|
  eventID== "TREE_012.21.2017-06-20"|
  eventID== "TREE_013.21.2017-06-20"|
  eventID== "TREE_015.21.2017-06-14"|
  eventID== "TREE_016.21.2017-06-15"|
  eventID== "TREE_017.21.2017-06-20"|
  eventID== "TREE_019.21.2017-06-20"|
  eventID== "TREE_020.21.2017-06-14"|
  eventID== "TREE_022.21.2017-06-14"|
  eventID== "TREE_023.21.2017-06-14"|
  eventID== "TREE_026.21.2017-06-14"|
  eventID== "TREE_060.21.2017-06-15"|
# DELA
  eventID== "DELA_001.21.2017-06-23"|
  eventID== "DELA_002.21.2017-06-25"|
  eventID== "DELA_003.21.2017-06-23"|
  eventID== "DELA_004.21.2017-06-25"|
  eventID== "DELA_005.21.2017-06-23"|
  eventID== "DELA_006.21.2017-06-23"|
  eventID== "DELA_007.21.2017-06-23"|
  eventID== "DELA_008.21.2017-06-26"|
  eventID== "DELA_010.21.2017-06-23"|
  eventID== "DELA_011.21.2017-06-23"|
  eventID== "DELA_013.21.2017-06-26"|
  eventID== "DELA_014.21.2017-06-23"|
  eventID== "DELA_015.21.2017-06-23"|
  eventID== "DELA_016.21.2017-06-23"|
  eventID== "DELA_021.21.2017-06-25"|
  eventID== "DELA_026.21.2017-06-23"|
  eventID== "DELA_027.21.2017-06-23"|
  eventID== "DELA_029.21.2017-06-23"|
# LENO
  eventID== "LENO_007.21.2017-06-19"|
  eventID== "LENO_022.21.2017-06-19"|
  eventID== "LENO_020.21.2017-06-19"|
  eventID== "LENO_027.21.2017-06-19"|
  eventID== "LENO_013.21.2017-06-19"|
  eventID== "LENO_026.21.2017-06-19"|
  eventID== "LENO_019.21.2017-06-19"|
  eventID== "LENO_025.21.2017-06-19"|
  eventID== "LENO_011.21.2017-06-19"|
  eventID== "LENO_015.21.2017-06-19"|
  eventID== "LENO_006.21.2017-06-19"|
  eventID== "LENO_030.21.2017-06-19"|
  eventID== "LENO_014.21.2017-06-19"|
  eventID== "LENO_004.21.2017-06-19"|
  eventID== "LENO_016.21.2017-06-19"|
  eventID== "LENO_029.21.2017-06-20"|
  eventID== "LENO_001.21.2017-06-20"|
  eventID== "LENO_021.21.2017-06-20"|
  eventID== "LENO_017.21.2017-06-20"|
  eventID== "LENO_002.21.2017-06-20"|
  eventID== "LENO_012.21.2017-06-20"|
  eventID== "LENO_003.21.2017-06-20"|
  eventID== "LENO_010.21.2017-06-20"|
  eventID== "LENO_009.21.2017-06-20"|
  eventID== "LENO_005.21.2017-06-21"|
# DCFS
  eventID== "DCFS_001.21.2017-06-27"|
  eventID== "DCFS_003.21.2017-06-27"|
  eventID== "DCFS_004.21.2017-06-28"|
  eventID== "DCFS_005.21.2017-06-27"|
  eventID== "DCFS_007.21.2017-06-28"|
  eventID== "DCFS_009.21.2017-06-27"|
  eventID== "DCFS_010.21.2017-06-27"|
  eventID== "DCFS_011.21.2017-06-27"|
  eventID== "DCFS_012.21.2017-06-28"|
  eventID== "DCFS_013.21.2017-06-27"|
  eventID== "DCFS_014.21.2017-06-28"|
  eventID== "DCFS_015.21.2017-06-28"|
  eventID== "DCFS_016.21.2017-06-28"|
  eventID== "DCFS_017.21.2017-06-28"|
  eventID== "DCFS_018.21.2017-06-27"|
  eventID== "DCFS_019.21.2017-06-27"|
  eventID== "DCFS_024.21.2017-06-28"|
  eventID== "DCFS_027.21.2017-06-27"|
  eventID== "DCFS_028.21.2017-06-28"|
  eventID== "DCFS_030.21.2017-06-28"|
# NOGP
  eventID== "NOGP_001.21.2017-07-09"|
  eventID== "NOGP_002.21.2017-07-08"|
  eventID== "NOGP_003.21.2017-07-09"|
  eventID== "NOGP_005.21.2017-07-08"|
  eventID== "NOGP_007.21.2017-07-08"|
  eventID== "NOGP_008.21.2017-07-09"|
  eventID== "NOGP_010.21.2017-07-09"|
  eventID== "NOGP_013.21.2017-07-09"|
  eventID== "NOGP_014.21.2017-07-08"|
  eventID== "NOGP_015.21.2017-07-08"|
  eventID== "NOGP_016.21.2017-07-09"|
  eventID== "NOGP_017.21.2017-07-08"|
  eventID== "NOGP_018.21.2017-07-09"|
  eventID== "NOGP_019.21.2017-07-08"|
  eventID== "NOGP_020.21.2017-07-09"|
  eventID== "NOGP_022.21.2017-07-08"|
  eventID== "NOGP_027.21.2017-07-08"|
  eventID== "NOGP_028.21.2017-07-08"|
  eventID== "NOGP_029.21.2017-07-08"|
  eventID== "NOGP_041.21.2017-07-09"|
# STER
  eventID== "STER_005.21.2017-05-17"|
  eventID== "STER_006.21.2017-05-17"|
  eventID== "STER_026.21.2017-05-17"|
  eventID== "STER_027.21.2017-05-17"|
  eventID== "STER_028.21.2017-05-17"|
  eventID== "STER_029.21.2017-05-17"|
  eventID== "STER_032.21.2017-05-17"|
  eventID== "STER_033.21.2017-05-17"|
  eventID== "STER_035.21.2017-05-17"|
# ABBY
  eventID== "ABBY_002.21.2017-05-26"|
  eventID== "ABBY_003.21.2017-05-27"|
  eventID== "ABBY_004.21.2017-05-26"|
  eventID== "ABBY_005.21.2017-05-27"|
  eventID== "ABBY_007.21.2017-05-27"|
  eventID== "ABBY_008.21.2017-05-27"|
  eventID== "ABBY_009.21.2017-05-26"|
  eventID== "ABBY_010.21.2017-05-27"|
  eventID== "ABBY_011.21.2017-05-27"|
  eventID== "ABBY_012.21.2017-05-27"|
  eventID== "ABBY_013.21.2017-05-27"|
  eventID== "ABBY_014.21.2017-05-26"|
  eventID== "ABBY_015.21.2017-05-26"|
  eventID== "ABBY_016.21.2017-05-26"|
  eventID== "ABBY_017.21.2017-05-27"|
  eventID== "ABBY_018.21.2017-05-27"|
  eventID== "ABBY_021.21.2017-05-26"|
  eventID== "ABBY_022.21.2017-05-26"|
  eventID== "ABBY_023.21.2017-05-27"|
  eventID== "ABBY_027.21.2017-05-26"|
  eventID== "ABBY_029.21.2017-05-26"|
  eventID== "ABBY_030.21.2017-06-04"|
# SOAP
  eventID== "SOAP_001.21.2017-05-16"|
  eventID== "SOAP_002.21.2017-05-16"|
  eventID== "SOAP_003.21.2017-05-16"|
  eventID== "SOAP_004.21.2017-05-16"|
  eventID== "SOAP_005.21.2017-05-16"|
  eventID== "SOAP_006.21.2017-05-16"|
  eventID== "SOAP_007.21.2017-05-16"|
  eventID== "SOAP_008.21.2017-05-16"|
  eventID== "SOAP_009.21.2017-05-16"|
  eventID== "SOAP_010.21.2017-05-16"|
  eventID== "SOAP_011.21.2017-05-16"|
  eventID== "SOAP_015.21.2017-05-16"|
  eventID== "SOAP_016.21.2017-05-16"|
  eventID== "SOAP_017.21.2017-05-16"|
  eventID== "SOAP_019.21.2017-05-16"|
  eventID== "SOAP_023.21.2017-05-16"|
  eventID== "SOAP_026.21.2017-05-16"|
  eventID== "SOAP_028.21.2017-05-16"|
  eventID== "SOAP_030.21.2017-05-16"
)

summary(unique(temp$plotID)) # 235/236: TOOL_007 missing. (235 from hand count)

# Export csv for the records before filtering to remove overlapping sites
write_csv(temp, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/birds2017_count_SinglePts_firstCountOnly_SpeciesRank_NoNoct_250Distance.csv")

#############################################
# Merge point and count data
# Single Points

birds2017_SinglePts_combined <- temp %>%right_join(birds2017_point_SinglePts_FirstSample, by = "plotID")
write_csv(birds2017_SinglePts_combined, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/birds2017_SinglePts_combined_firstCountOnly_SpeciesRank_NoNoct_250Distance.csv")
temp%>%filter(plotID=="ABBY_016")
# Grided Points
birds2017_Gird_combined <- birds2017_count_Grid%>%right_join(birds2017_point_Grid, by = "plotID")
View(birds2017_Gird_combined)
write_csv(birds2017_Gird_combined, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/birds2017_Gird_combined_firstCountOnly_SpeciesRank_NoNoct_250Distance.csv")

#############################################
# Filter out points with overlap at 250m radius 

#### Filter out points with overlap 
birds2017_SinglePts_combined_filtered <- birds2017_SinglePts_combined%>%filter(
  # SCBI
  plotID== "SCBI_002" |
  plotID== "SCBI_005" |
  plotID== "SCBI_006" |
  plotID== "SCBI_007" |
  plotID== "SCBI_012" |
  plotID== "SCBI_013" |
  plotID== "SCBI_014" |
  plotID== "SCBI_016" |
  plotID== "SCBI_017" |
  plotID== "SCBI_018" |
  plotID== "SCBI_019" |
  plotID== "SCBI_022" |
  plotID== "SCBI_023" |
  plotID== "SCBI_033" |
  plotID== "SCBI_035" |
  plotID== "SCBI_039" |
  plotID== "SCBI_043" |
  # TOOL
  plotID== "TOOL_001" |
  plotID== "TOOL_002" |
  plotID== "TOOL_003" |
  plotID== "TOOL_005" |
  plotID== "TOOL_006" |
  plotID== "TOOL_007" |
  plotID== "TOOL_008" |
  plotID== "TOOL_009" |
  plotID== "TOOL_010" |
  plotID== "TOOL_011" |
  plotID== "TOOL_012" |
  plotID== "TOOL_013" |
  plotID== "TOOL_014" |
  plotID== "TOOL_016" |
  plotID== "TOOL_017" |
  plotID== "TOOL_018" |
  plotID== "TOOL_019" |
  plotID== "TOOL_020" |
  plotID== "TOOL_021" |
  plotID== "TOOL_022" |
  plotID== "TOOL_024" |
  plotID== "TOOL_026" |
  plotID== "TOOL_027" |
  plotID== "TOOL_028" |
  plotID== "TOOL_071" |
  # BLAN
  plotID== "BLAN_002" |
  plotID== "BLAN_004" |
  plotID== "BLAN_005" |
  plotID== "BLAN_013" |
  plotID== "BLAN_014" |
  plotID== "BLAN_015" |
  plotID== "BLAN_020" |
  # SERC
  plotID== "SERC_001" |
  plotID== "SERC_002" |
  plotID== "SERC_003" |
  plotID== "SERC_005" |
  plotID== "SERC_006" |
  plotID== "SERC_008" |
  plotID== "SERC_010" |
  plotID== "SERC_011" |
  plotID== "SERC_012" |
  plotID== "SERC_013" |
  plotID== "SERC_014" |
  plotID== "SERC_017" |
  plotID== "SERC_018" |
  plotID== "SERC_019" |
  plotID== "SERC_022" |
  plotID== "SERC_024" |
  plotID== "SERC_025" |
  # TREE
  plotID== "TREE_001" |
  plotID== "TREE_002" |
  plotID== "TREE_003" |
  plotID== "TREE_004" |
  plotID== "TREE_007" |
  plotID== "TREE_013" |
  plotID== "TREE_015" |
  plotID== "TREE_019" |
  plotID== "TREE_020" |
  plotID== "TREE_022" |
  plotID== "TREE_060" |
  # DELA
  plotID== "DELA_002" |
  plotID== "DELA_005" |
  plotID== "DELA_006" |
  plotID== "DELA_010" |
  plotID== "DELA_013" |
  plotID== "DELA_015" |
  plotID== "DELA_021" |
  plotID== "DELA_026" |
  plotID== "DELA_027" |
  # LENO
  plotID== "LENO_001" |
  plotID== "LENO_002" |
  plotID== "LENO_004" |
  plotID== "LENO_005" |
  plotID== "LENO_006" |
  plotID== "LENO_010" |
  plotID== "LENO_012" |
  plotID== "LENO_013" |
  plotID== "LENO_014" |
  plotID== "LENO_017" |
  plotID== "LENO_020" |
  plotID== "LENO_021" |
  plotID== "LENO_022" |
  plotID== "LENO_025" |
  plotID== "LENO_026" |
  # DCFS
  plotID== "DCFS_003" |
  plotID== "DCFS_005" |
  plotID== "DCFS_007" |
  plotID== "DCFS_009" |
  plotID== "DCFS_010" |
  plotID== "DCFS_012" |
  plotID== "DCFS_014" |
  plotID== "DCFS_016" |
  plotID== "DCFS_017" |
  plotID== "DCFS_018" |
  plotID== "DCFS_019" |
  plotID== "DCFS_027" |
  plotID== "DCFS_028" |
  # NOGP
  plotID== "NOGP_001" |
  plotID== "NOGP_002" |
  plotID== "NOGP_005" |
  plotID== "NOGP_007" |
  plotID== "NOGP_008" |
  plotID== "NOGP_010" |
  plotID== "NOGP_013" |
  plotID== "NOGP_014" |
  plotID== "NOGP_017" |
  plotID== "NOGP_019" |
  plotID== "NOGP_020" |
  plotID== "NOGP_028" |
  plotID== "NOGP_041" |
  # STER
  plotID== "STER_005" |
  plotID== "STER_027" |
  plotID== "STER_028" |
  plotID== "STER_029" |
  plotID== "STER_032" |
  plotID== "STER_033" |
  # ABBY
  plotID== "ABBY_002" |
  plotID== "ABBY_003" |
  plotID== "ABBY_005" |
  plotID== "ABBY_007" |
  plotID== "ABBY_008" |
  plotID== "ABBY_009" |
  plotID== "ABBY_010" |
  plotID== "ABBY_011" |
  plotID== "ABBY_012" |
  plotID== "ABBY_013" |
  plotID== "ABBY_014" |
  plotID== "ABBY_015" |
  plotID== "ABBY_016" |
  plotID== "ABBY_017" |
  plotID== "ABBY_018" |
  plotID== "ABBY_021" |
  plotID== "ABBY_022" |
  plotID== "ABBY_023" |
  plotID== "ABBY_027" |
  plotID== "ABBY_029" |
  plotID== "ABBY_030" |
  # SOAP
  plotID== "SOAP_003" |
  plotID== "SOAP_004" |
  plotID== "SOAP_005" |
  plotID== "SOAP_006" |
  plotID== "SOAP_008" |
  plotID== "SOAP_009" |
  plotID== "SOAP_010" |
  plotID== "SOAP_011" |
  plotID== "SOAP_016" |
  plotID== "SOAP_019" |
  plotID== "SOAP_026" |
  plotID== "SOAP_028" |
  plotID== "SOAP_030" )

################## Data check
# Check data befor row bind (make sure everything is there)
# Single points
summary(unique(birds2017_SinglePts_combined_filtered$plotID)) # should be 167 plots (out of the original 236) ~ 70.8% remaining after filting for spatial overlap at 250m 

# Grid points
summary(unique(birds2017_Gird_combined$plotID)) # should be 285 

birds2017_Gird_combined%>%distinct(plotID, .keep_all = T)%>%filter(siteID.x=="HARV")%>%distinct(plotID)%>%summary # how to check on individual sites 


#############################################
# Row bind single point and grid data

birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap <- rbind(birds2017_Gird_combined, birds2017_SinglePts_combined_filtered)

summary(unique(birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap$plotID)) # 285 + 167 = 452

write_csv(birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap.csv")

df <- birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap

#############################################


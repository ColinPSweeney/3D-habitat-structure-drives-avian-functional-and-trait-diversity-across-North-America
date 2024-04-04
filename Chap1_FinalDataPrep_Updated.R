###############################################################################
# Data preping: create Final data data table with Phylogenetic, Daymet Climate Data, and 'fixed' structural metrics
# Feb 15th, 2023
# Chapter 1
# Combine data into data frame (contains all data MINUS PCoA)
# 
# This file was originally 2 separate files. 
# Part 1: SpRich, FD, environmental data (out of date structural/climate metrics)
# Part 2: Phylogenetic data, updated climate data (out of date structural)
# Part 3: Updated structural metrics 

###############################################################################
library(ggplot2)
library(cowplot)
library(bayestestR)
library(bayesplot)
library(rstan)
library(brmstools)
library(tidybayes)
library(dplyr)

############################################################
# PART 1: Combine SpRich, Functional -------------------------------------------
# Final Data Prep for Chapter 1 (excluding phylogenetics)
# PlotNum Key
PlotNum_Key <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/PlotNum_Key.csv")

# Species Richness
sp_rich <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/2017_20000_supercomputer_distcorrected_spRich_combined.csv") # Output From: DistSamp_SpRichCalculations.R file
sp_rich <- sp_rich%>%select(plotNum, Original_SpRich,rangefilt_95_SpRich)

Data <- left_join(PlotNum_Key, sp_rich, by="plotNum")

# 2017Data_final
OldDataFile <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DataPrep/FinalData/2017Data_Final.csv")

OldDataFile <- OldDataFile%>%select(plotID,nlcdClass,elevation,Variance,Shannon,Simpson,Mean, Understory, Midstory, Canopy, CHM_Mean,Temp.Range,Temp.Mean,Precip.Annual)

Data2 <- left_join(Data, OldDataFile, by="plotID")

# 3D Fragmentation Data
Frag3d <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output.csv") # Out of date. 

Frag3d <- Frag3d%>%select(plotID,clumpy_und, clumpy_mid, clumpy_can, te_und, te_mid, te_can)
Frag3d_b <-Frag3d%>%select(plotID, te_und, te_mid, te_can)

Data3 <- left_join(Data2, Frag3d, by="plotID")

# Functional Richness
dbfd_output<- readRDS("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Functional/dbfd_output_chp1_super20000.rds")

DBFD <- matrix(NA,length(Data$plotNum),4)
colnames(DBFD) <- c("plotNum", "FEve", "FDiv","FRic")

DBFD[, 1] <- Data$plotNum
DBFD[, 2] <- dbfd_output$FEve
DBFD[, 3] <- dbfd_output$FDiv
DBFD[, 4] <- dbfd_output$FRic

DBFD<-as.data.frame(DBFD)

final_DATA_withoutPhylo <- left_join(Data3, DBFD, by="plotNum")

write_csv(final_DATA_withoutPhylo, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Chp1FinalData_withoutPhylo.csv")

################################################################################
# PART 2: Add phylogenetic data and climate data -------------------------------
# Combine all data 
final_DATA_withoutPhylo <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Chp1FinalData_withoutPhylo.csv") # Main data file

# Functional and Phylogenetic Data taken from: NullModel_DataPrep_and_Calculations.R file
# Funcitonal Richness: SES values and original 
SES_NullAdjusted_FRic_Chap1 <-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_FRic_Chap1.csv")
SES_NullAdjusted_FRic_Chap1B <- select(SES_NullAdjusted_FRic_Chap1, plotNum, original_FRic, SES.FRic, FRic.p)

# Phylogenetic Diversity (PD and MPD): SES values and original 
SES_NullAdjusted_PD_Chap1 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_PD_Chap1.csv")
SES_NullAdjusted_PD_Chap1B<- select(SES_NullAdjusted_PD_Chap1, plotID, original_PD=origianal_means,SES.PD, PD.p)
head(SES_NullAdjusted_PD_Chap1B)
  
SES_NullAdjusted_MPD_Chap1 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Null_Models/SES_NullAdjusted_MPD_Chap1.csv")
SES_NullAdjusted_MPD_Chap1B<- select(SES_NullAdjusted_MPD_Chap1, plotID, original_MPD=origianal_means,SES.MPD, MPD.p)
head(SES_NullAdjusted_MPD_Chap1B)

final_DATA_withoutPhylo_B <- left_join(final_DATA_withoutPhylo, SES_NullAdjusted_FRic_Chap1B, by="plotNum")
final_DATA_withoutPhylo_B <- left_join(final_DATA_withoutPhylo_B, SES_NullAdjusted_PD_Chap1B, by="plotID")
final_DATA_withoutPhylo_B <- left_join(final_DATA_withoutPhylo_B, SES_NullAdjusted_MPD_Chap1B, by="plotID")

View(final_DATA_withoutPhylo_B)

###############################################################################
# OLD 
# Create new column of ALL vegetation amount -----------------------------------
# Sum Understory, Midstory, and Canopy. Create new column with full vertical veg. 
final_DATA_withoutPhylo_C <- final_DATA_withoutPhylo_B%>%
  mutate(Total_veg_amt = select(., Understory:Canopy) %>% rowSums(na.rm = TRUE))

cor(final_DATA_withoutPhylo_C$Understory, final_DATA_withoutPhylo_C$Total_veg_amt, use="pairwise.complete.obs") # 0.7097935
cor(final_DATA_withoutPhylo_C$Midstory, final_DATA_withoutPhylo_C$Total_veg_amt, use="pairwise.complete.obs") # 0.9119189
cor(final_DATA_withoutPhylo_C$Canopy, final_DATA_withoutPhylo_C$Total_veg_amt, use="pairwise.complete.obs") # 0.8889985

###############################################################################
# Download Climate Data from Daymet -------------------------------------------
# Load required packages
install.packages("daymetr")
library("daymetr")
library(sp)

# Set GPS coordinates for locations of interest
locations <- data.frame(
  plotID = c(final_DATA_withoutPhylo_B$plotID),
  lat = c(final_DATA_withoutPhylo_B$decimalLatitude),
  long = c(final_DATA_withoutPhylo_B$decimalLongitude)
)
write.table(locations, file="/Users/colinsweeney/Desktop/locations.txt", append = FALSE, sep = ",", dec = ".",
            row.names = F, col.names = TRUE)

# Download daily Daymet data for each location
daymet_data <-download_daymet_batch(
  file_location = "/Users/colinsweeney/Desktop/locations.txt",
  start = 2017,
  end = 2017,
  internal = TRUE,
  force = FALSE,
  silent = FALSE,
  path = tempdir(),
  simplify = FALSE
)

View(daymet_data)

range_temp_site <- c()
ave_temp_site <- c()
total_precip_site <- c()

##############
# Loop to calculated mean values per site for percipitation and temp
for(i in 1:448){
  daymet_site <- daymet_data[[i]]$data

# Filter out days to include only May-June 2017 (breeding season)
daymet_site_mod <- daymet_site%>%filter(yday>=121)%>%filter(yday<=212)

# Temperature range (max-min per day, averaged for May-June)
range_temp_site[i] <-mean(daymet_site_mod$tmax..deg.c. - daymet_site_mod$tmin..deg.c.) 

# Average Temperature (calculate average per day: (max + min)/2, then averaged over May-June) 
ave_temp_site[i] <- mean((daymet_site_mod$tmax..deg.c. + daymet_site_mod$tmin..deg.c.)/(2))

# Total precipitation (sum for May-June)
total_precip_site[i] <- sum(daymet_site_mod$prcp..mm.day.)

}
##############

daymet_data_bysite <- cbind(locations, temp_range=range_temp_site, temp_ave=ave_temp_site, precip_total=total_precip_site)

################################################################################
# Add Daymet data to the rest of the data. 
final_DATA_withoutPhylo_D <- left_join(final_DATA_withoutPhylo_C, daymet_data_bysite, by="plotID")
View(final_DATA_withoutPhylo_D)

# Not a lot of correlation between CHELSA and Daymet 
cor(final_DATA_withoutPhylo_D$Precip.Annual, final_DATA_withoutPhylo_D$precip_total)
cor(final_DATA_withoutPhylo_D$Temp.Range, final_DATA_withoutPhylo_D$temp_range)
cor(final_DATA_withoutPhylo_D$Temp.Mean, final_DATA_withoutPhylo_D$temp_ave)

# about how many have repeat values ~ 46 
length(which(table(final_DATA_withoutPhylo_D$precip_total)>1))
length(which(table(final_DATA_withoutPhylo_D$temp_range)>1))
length(which(table(final_DATA_withoutPhylo_D$temp_ave)>1))

################################################################################
# At this point all climate, phylo, functional, etc are included. Now need to check NA and quality control before the data is actually final FINAL
write.csv(final_DATA_withoutPhylo_D, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/final_DATA_withoutPhylo_D.csv")

# Check how many 3D frag metrics are missing/need repair. 
length(final_DATA_withoutPhylo_D$te_und) #448

summary(final_DATA_withoutPhylo_D$te_und) #146 NAs 32.6%
summary(final_DATA_withoutPhylo_D$te_mid) #146 NAs 32.6%
summary(final_DATA_withoutPhylo_D$te_can) #145 NAs 32.4%

final_DATA_withoutPhylo_D$domainID
unique(subset(final_DATA_withoutPhylo_D,is.na(te_und))$plotID) # list of all plots with missing 3D frag data
unique(subset(final_DATA_withoutPhylo_D,is.na(te_und))$domainID)

###
# Input in missing LiDAR data
output <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output_missing.csv")

repair <- output%>%dplyr::select(plotID, clumpy_und, clumpy_mid, clumpy_can, te_und, te_mid, te_can)
output$plotID

# Old List of LiDAR indicies 
Old_indicies <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output.csv")

Old_indicies <- Old_indicies%>%dplyr::select(plotID, clumpy_und, clumpy_mid, clumpy_can, te_und, te_mid, te_can)
output$plotID

# Combine new and old values
Old_indicies_repair <- rbind(Old_indicies, repair)

length(sort(unique(Old_indicies_repair$plotID))) #446
Old_indicies_repair[duplicated(Old_indicies_repair$plotID),] # make sure nothing is repeated. 

# Add to Full Data
final_DATA_withoutPhylo_E <- final_DATA_withoutPhylo_D%>%dplyr::select(
# Identification 
plotID, plotNum, domainID, siteID, decimalLatitude,decimalLongitude,
# Dependent variables
Original_SpRich, rangefilt_95_SpRich, FEve, FDiv,original_FRic, SES.FRic,FRic.p,original_PD,SES.PD,PD.p, original_MPD, SES.MPD, MPD.p,      
# Climate Independent variables
nlcdClass, elevation, temp.range_CHELSA=Temp.Range, temp.ave_CHELSA=Temp.Mean,  precip.annual_CHELSA=Precip.Annual, temp.range_daymet=temp_range, temp.ave_daymet=temp_ave, precip.total_daymet=precip_total,
# Structural Independent variables
Vert.Variance=Variance, Vert.Shannon=Shannon, Vert.Simpson=Simpson, Vert.Mean=Mean, Understory, Midstory,Canopy,Total_veg_amt, CHM_Mean)

# Add new data
final_DATA_withoutPhylo_F <- left_join(final_DATA_withoutPhylo_E, Old_indicies_repair, by="plotID")

length(final_DATA_withoutPhylo_F$plotID) # 448
View(final_DATA_withoutPhylo_F)
colnames(final_DATA_withoutPhylo_F)

# Now, repair NA values for site sites that have no volume
final_DATA_withoutPhylo_F_b <-final_DATA_withoutPhylo_F %>% mutate(te_und = ifelse(Understory == 0,0,te_und))
final_DATA_withoutPhylo_F_b <-final_DATA_withoutPhylo_F_b %>% mutate(te_mid = ifelse(Midstory == 0,0,te_mid))
final_DATA_withoutPhylo_F_b <-final_DATA_withoutPhylo_F_b %>% mutate(te_can = ifelse(Canopy == 0,0,te_can))

View(final_DATA_withoutPhylo_F_b)
summary(as.numeric(final_DATA_withoutPhylo_F_b$te_can))

# This should be the last step to get the complete data. 
# STEI_028, STEI_034, and STEI_035 are all missing. They are the ones without ANY structural data. 

# OLD VERSION 2  ---------------------------------------------------------------
# Export CSV
write.csv(final_DATA_withoutPhylo_F_b, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_datarepair.csv")


#################################################################################
# PART 3: Update structural metrics --------------------------------------------
# Update: Fixed the structural indices. Now need to recreate dataframe
# New Values: 4 height bins with 0.5m and 1m voxel and raster resolutions respectively. 
# New values from: 3DMetrics_Chp1_REDO_4Bins_resolution.R file

# Load new data 
new_df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DnMetrics_UndMidSubcanCan_2017_250m_output_1m.csv")

Total_veg_amt_NEW <- new_df%>%dplyr::select(volume_und, volume_mid, volume_subcan, volume_can)%>%rowSums(na.rm = TRUE)

new_df_B <- cbind(new_df, Total_veg_amt_NEW)
length(new_df_B)

GGally::ggcorr(new_df_B[,c(#"para_mn_und", "para_mn_mid", "para_mn_subcan", "para_mn_can", 
                           "np_und", "np_mid", "np_subcan", "np_can", 
                           "clumpy_und", "clumpy_mid", "clumpy_subcan", "clumpy_can", 
                           #"te_und", "te_mid", "te_subcan", "te_can", 
                           "volume_und", "volume_mid", "volume_subcan", "volume_can", 
                           "Total_veg_amt_NEW" )], label = TRUE, label_alpha = TRUE, nbreaks = 5, label_round = 2)

new_df_C <- new_df_B%>%dplyr::select(plotID, volume_und, volume_mid, volume_subcan, volume_can, clumpy_und, clumpy_mid, clumpy_subcan, clumpy_can, te_und, te_mid, te_subcan, te_can, np_und, np_mid, np_subcan, np_can, Total_veg_amt_NEW)

################################################################################
# Read in old file to update (if not already loaded in)
final_DATA_withoutPhylo_F_b <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_datarepair.csv")


final_DATA_withoutPhylo_F_C <- final_DATA_withoutPhylo_F_b%>%dplyr::select(plotID,plotNum,domainID, siteID, decimalLatitude, decimalLongitude, Original_SpRich, rangefilt_95_SpRich,FEve,FDiv,original_FRic, SES.FRic, FRic.p,original_PD,SES.PD,PD.p,original_MPD,SES.MPD,MPD.p,nlcdClass,elevation,temp.range_CHELSA, temp.ave_CHELSA, precip.annual_CHELSA, temp.range_daymet, temp.ave_daymet, precip.total_daymet, Vert.Variance, Vert.Shannon, Vert.Simpson, Vert.Mean)
length(final_DATA_withoutPhylo_F_C$plotID) # 448

# Combine old and new values
final_DATA_withoutPhylo_F_D <- left_join(final_DATA_withoutPhylo_F_C, new_df_C, by="plotID") 
length(final_DATA_withoutPhylo_F_D$plotID) #448

write.csv(final_DATA_withoutPhylo_F_D, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins.csv")

# Data Cleaning
final_DATA_withoutPhylo_F_E <- final_DATA_withoutPhylo_F_D%>%drop_na(volume_und) # Data cleaning. Remove NA data. 
final_DATA_withoutPhylo_F_E <- final_DATA_withoutPhylo_F_E%>%filter(rangefilt_95_SpRich>=5) # Remove site with less than 5 species
length(final_DATA_withoutPhylo_F_E$plotID) # 390 plots
View(final_DATA_withoutPhylo_F_E)

# Now, repair NA values for site sites that have no volume (enter 0s for NAs)
# volume
final_DATA_withoutPhylo_F_E["volume_mid"][is.na(final_DATA_withoutPhylo_F_E["volume_mid"])] <- 0
final_DATA_withoutPhylo_F_E["volume_subcan"][is.na(final_DATA_withoutPhylo_F_E["volume_subcan"])] <- 0
final_DATA_withoutPhylo_F_E["volume_can"][is.na(final_DATA_withoutPhylo_F_E["volume_can"])] <- 0
# number of patches
final_DATA_F_E_repair <-final_DATA_withoutPhylo_F_E %>% dplyr::mutate(np_und = ifelse(volume_und == 0,0,np_und))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(np_mid = ifelse(volume_mid == 0,0,np_mid))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(np_subcan = ifelse(volume_subcan == 0,0,np_subcan))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(np_can = ifelse(volume_can == 0,0,np_can))
# total edge
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(te_und = ifelse(volume_und == 0,0,te_und))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(te_mid = ifelse(volume_mid == 0,0,te_mid))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(te_subcan = ifelse(volume_subcan == 0,0,te_subcan))
final_DATA_F_E_repair <-final_DATA_F_E_repair %>% dplyr::mutate(te_can = ifelse(volume_can == 0,0,te_can))

View(final_DATA_F_E_repair)

write.csv(final_DATA_F_E_repair, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair.csv")

GGally::ggcorr(final_DATA_F_E_repair[,c(#"para_mn_und", "para_mn_mid", "para_mn_subcan", "para_mn_can", 
  "np_und", "np_mid", "np_subcan", "np_can", 
  "clumpy_und", "clumpy_mid", "clumpy_subcan", "clumpy_can", 
  "te_und", "te_mid", "te_subcan", "te_can", 
  "volume_und", "volume_mid", "volume_subcan", "volume_can", 
  "Total_veg_amt_NEW" )], label = TRUE, label_alpha = TRUE, nbreaks = 7, label_round = 2)

################################################################################
# Update Vertical Configuration Structure Indices 
# read in data from: 3DMetrics_Chp1_REDO_4Bins_resolution.R file

# output_vert_config2 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_vert_config2.csv")
output_vert_configB2<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_vert_configB2.csv")

output_vert_config3 <- output_vert_configB2%>%dplyr::select(plotID, shannon_plot,simpson_plot,variance_plot, range_abs, dist_traveled)

final_DATA_F_E_repairB <- left_join(final_DATA_F_E_repair, output_vert_config3, by="plotID")
final_DATA_F_E_repairB

# Check new values
cor(final_DATA_F_E_repairB$Vert.Shannon, final_DATA_F_E_repairB$shannon_plot) # not the same as the old values
cor(final_DATA_F_E_repairB$Vert.Simpson, final_DATA_F_E_repairB$simpson_plot) # not the same as the old values
cor(final_DATA_F_E_repairB$Vert.Variance, final_DATA_F_E_repairB$variance_plot) # not the same as the old values

hist(final_DATA_F_E_repairB$shannon_plot, breaks=100)
hist(final_DATA_F_E_repairB$simpson_plot, breaks=100)
hist(final_DATA_F_E_repairB$variance_plot, breaks=100)

GGally::ggcorr(final_DATA_F_E_repairB[,c(#"para_mn_und", "para_mn_mid", "para_mn_subcan", "para_mn_can", 
  "np_und", "np_mid", "np_subcan", "np_can", 
  #"clumpy_und", "clumpy_mid", "clumpy_subcan", "clumpy_can", 
  #"te_und", "te_mid", "te_subcan", "te_can", 
  "volume_und", "volume_mid", "volume_subcan", "volume_can", 
  "shannon_plot", "simpson_plot", "variance_plot",
 "range_abs", "dist_traveled",
  #"Vert.Shannon", "Vert.Simpson", "Vert.Variance",
  "Total_veg_amt_NEW" )], label = TRUE, label_alpha = TRUE, nbreaks = 7, label_round = 2)


# Save output 
write.csv(final_DATA_F_E_repairB, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1.csv")

#########################################################################################
# Add values for total volume fragmentation. 
output_frag_TotVol2 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_frag_TotVol2.csv")
output_frag_TotVol2 <- output_frag_TotVol2%>%dplyr::select(plotID, clumpy_TotVol, np_TotVol, te_TotVol)

final_DATA_F_E_repairB <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1.csv")


final_DATA_F_E_repairB_2 <- left_join(final_DATA_F_E_repairB, output_frag_TotVol2, by="plotID")


GGally::ggcorr(final_DATA_F_E_repairB_2[,c(#"para_mn_und", "para_mn_mid", "para_mn_subcan", "para_mn_can", 
  "np_und", "np_mid", "np_subcan", "np_can", 
  "clumpy_und", "clumpy_mid", "clumpy_subcan", "clumpy_can", 
  "te_und", "te_mid", "te_subcan", "te_can", 
  "volume_und", "volume_mid", "volume_subcan", "volume_can", 
  "shannon_plot", "simpson_plot", "variance_plot",
  "range_abs", "dist_traveled",
  "clumpy_TotVol", "np_TotVol", "te_TotVol",
  #"Vert.Shannon", "Vert.Simpson", "Vert.Variance",
  "Total_veg_amt_NEW" )], label = TRUE, label_alpha = TRUE, nbreaks = 7, label_round = 2)

################################################################################
# Save output ------------------------------------------------------------------
write.csv(final_DATA_F_E_repairB_2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag.csv")




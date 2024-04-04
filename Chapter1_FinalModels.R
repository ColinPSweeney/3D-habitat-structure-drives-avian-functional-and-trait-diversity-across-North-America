############################################################
# Chapter 1: Final Models 
# Feb 2023
# Author: Colin P Sweeney
############################################################
# Load Packages------
library(ggplot2)
library(cowplot)
library(bayestestR)
library(bayesplot)
library(rstan)
# devtools::install_github("mvuorre/brmstools")
library(brmstools)
library(tidybayes)
library(tidyverse)
library(dplyr)

# Read in data------
#df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair.csv") # 390 sites 

#df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1.csv")

#df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag.csv")

#df <- read.csv(file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_PCA_ratings.csv")

df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/July2023/NewPCoA/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_NEW_PCoA.csv") # df_D from new PCoA output

# Data Quality Check: Make sure there are no NAs for any critical columns ------
# df <- df%>%drop_na(volume_und) # Data cleaning. Remove NA data. 
# df <- df%>%filter(rangefilt_95_SpRich>=5) # Remove site with less than 5 species

length(df$plotID) #385

# Taxonomic
summary(df$rangefilt_95_SpRich) # no sites with less than 5 species 
# Functional
summary(df$FEve) 
summary(df$FDiv) 
summary(df$SES.FRic) 
# Phylogenetic
summary(df$SES.PD) 
summary(df$SES.MPD) 
# Trait-based PC Axes
summary(df$PC1)
summary(df$PC2)
summary(df$PC3)
summary(df$PC4)

###############################################################################
# MODELS 
###############################################################################
# 4 sets of models for each of the biodiversity indices (Full, Structure only, Configuration, Composition)
# Taxonomic: Species Richness
# Functional: Functional Evenness, Functional Divergence, Functional Richness (SES), PCA1, PCA2
# Phylogenetic: Mean Pairwise Distance (SES), Faith's Phylogenetic Diversity

# Set data to test variable 
test <-df
# Scale Data--------------------------------------------------------------------

# Site/Plot------
test$siteID.factor <- as.factor(test$siteID)
test$plotID.factor <- as.factor(test$plotID)
test$domainID.factor <- as.factor(test$domainID)

# Structural Metrics-----------------------------------
# Configuration ---------------------------------------
# Vertical----------------
# OLD:
# test$Variance.s<- scale(test$Vert.Variance)
# test$Shannon.s<- scale(test$Vert.Shannon)
# test$Simpson.s<- scale(test$Vert.Simpson)
# test$Mean.s<- scale(test$Vert.Mean)
# NEW:
test$Variance.s<- scale(test$variance_plot) # vertical heterogeneity

# test$range_abs.s <- scale(test$range_abs)
# test$dist_traveled.s <- scale(test$dist_traveled)

# Horizontal -------------
# test$te_und.s <- scale(test$te_und)
# test$te_mid.s <- scale(test$te_mid)
# test$te_subcan.s <- scale(test$te_subcan)
# test$te_can.s <- scale(test$te_can)
# 
# test$clumpy_und.s <- scale(test$clumpy_und)
# test$clumpy_mid.s <- scale(test$clumpy_mid)
# test$clumpy_subcan.s <- scale(test$clumpy_subcan)
# test$clumpy_can.s <- scale(test$clumpy_can)

test$np_und.s <- scale(test$np_und)
test$np_mid.s <- scale(test$np_mid)
test$np_subcan.s <- scale(test$np_subcan)
test$np_can.s <- scale(test$np_can)

# Total Configuration ----
test$np_TotVol.s <- scale(test$np_TotVol)

# Composition ---------------------------------------
# Amount at height strata ----
test$Understory.s <- scale(test$volume_und)
test$Midstory.s<- scale(test$volume_mid)
test$SubCanopy.s<- scale(test$volume_subcan)
test$Canopy.s<- scale(test$volume_can)

# Total Vegetation Amount Across All Heights ---
test$Total_veg_amt.s<- scale(test$Total_veg_amt_NEW)
# test$CHM_Mean.s<- scale(test$CHM_Mean)

# Climate---------------------------------------------
test$decimalLatitude.s<- scale(test$decimalLatitude)
test$elevation.s<- scale(test$elevation)

test$temp.range_daymet.s <- scale(test$temp.range_daymet)

# test$temp.ave_daymet.s <- scale(test$temp.ave_daymet)
# test$precip.total_daymet.s <- scale(test$precip.total_daymet)

# test$Temp.Range.s<- scale(test$Temp.Range)
# test$Temp.Mean.s<- scale(test$Temp.Mean)
# test$Precip.Annual.s<- scale(test$Precip.Annual)

# Landcover Covariates --------------------------------
# (Not used for bayesian models)
test$nlcdClass.factor <- as.factor(test$nlcdClass)

# TEST for NAs ----------------------------------------
  # Vert Configuration
  summary(test$Variance.s)
  # Horiz Configuration
  summary(test$np_und.s)
  summary(test$np_mid.s)
  summary(test$np_subcan.s)
  summary(test$np_can.s )
  # Total Configuration
  summary(test$np_TotVol)
  # Horiz Composition
  summary(test$Understory.s)
  summary(test$Midstory.s)
  summary(test$SubCanopy.s)
  summary(test$Canopy.s)
  # Total Composition
  summary(test$Total_veg_amt.s)
  
  # Climate
  summary(test$decimalLatitude.s)
  summary(test$elevation.s)
  summary(test$temp.range_daymet.s)

###############################################################################
# Priors
# Full
get_prior(rangefilt_95_SpRich ~ 
            # Vert Configuration
            Variance.s + 
            # Horiz Configuration
            np_und.s +
            np_mid.s + 
            np_subcan.s +
            np_can.s +
            # Total Configuration
            np_TotVol +
            # Horiz Composition
            Understory.s +
            Midstory.s +
            SubCanopy.s +
            Canopy.s +
            # Total Composition
            Total_veg_amt.s +
            
            # Climate
            decimalLatitude.s +
            elevation.s +
            temp.range_daymet.s +
            #temp.ave_daymet.s +
            # precip.total_daymet.s +
            # Random effects
            (1|siteID.factor),
            
            data = test,
            family = poisson())

# Set Priors -------------------------------------------------------------------
# Check correlations 
GGally::ggcorr(test[,c(
  # "Shannon.s", "Variance.s", "Simpson.s",
  "shannon_plot", "simpson_plot", "variance_plot",
  "dist_traveled", "range_abs", 
  #"para_mn_und", "para_mn_mid", "para_mn_subcan", "para_mn_can", 
  "np_und", "np_mid", "np_subcan", "np_can", 
  "clumpy_und", "clumpy_mid", "clumpy_subcan", "clumpy_can", 
  "te_und", "te_mid", "te_subcan", "te_can", 
  "volume_und", "volume_mid", "volume_subcan", "volume_can", 
  "Total_veg_amt_NEW", 
  "decimalLatitude.s", "elevation.s", "temp.range_daymet.s", "temp.ave_daymet.s", "precip.total_daymet.s"
  )], label = TRUE, label_alpha = TRUE, nbreaks = 7, label_round = 2)

GGally::ggcorr(test[,c(
  "volume_und", "volume_mid", "volume_subcan", "volume_can", 
  "Total_veg_amt_NEW", 
  "np_und", "np_mid", "np_subcan", "np_can", 
  "variance_plot",
  "np_TotVol", "clumpy_TotVol",
  "decimalLatitude.s", "elevation.s", 
  "temp.range_daymet.s"#, #"temp.ave_daymet.s", 
  #"precip.total_daymet.s"
)], label = TRUE, label_alpha = TRUE, nbreaks = 7, label_round = 2)

################################################################################
# Start of Priors --------------------------------------------------------------
# 1) Full Model ---
prior_Full <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  
  prior(normal(0,2), coef = 'np_TotVol.s'), # Total/2D Horizontal Heterogeneity 
  
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount # highly correlated
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  # prior(normal(0,2), coef = 'temp.ave_daymet.s'),
  # prior(normal(0,2), coef = 'precip.total_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#######################
# 1.5) Full Model (Minus total volume) ---
prior_Full_minusTotVol <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  
  prior(normal(0,2), coef = 'np_TotVol.s'), # Total/2D Horizontal Heterogeneity 
  
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount # highly correlated
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  # prior(normal(0,2), coef = 'temp.ave_daymet.s'),
  # prior(normal(0,2), coef = 'precip.total_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#######################
# 2) Structure Only ---
prior_Structure <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  
  prior(normal(0,2), coef = 'np_TotVol.s'), # Total/2D Horizontal Heterogeneity 
  
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount # highly correlated
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#######################
# 3) Configuration Only ---
prior_Configuration <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  
  prior(normal(0,2), coef = 'np_TotVol.s'), # Total/2D Horizontal Heterogeneity 
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

# 3b) Configuration Only (with climate)---
prior_Configuration_b <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  
  prior(normal(0,2), coef = 'np_TotVol.s'), # Total/2D Horizontal Heterogeneity 
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#######################
# 4) Composition Only ---
prior_Composition <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount # highly correlated
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

# 4b) Composition Only (with climate) ---
prior_Composition_b <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount # highly correlated
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#######################
# 5) Horizontal Only ---
prior_Horizontal <- c( 
  # Structure 
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

# 5b) Horizontal Only (with climate)---
prior_Horizontal_b <- c( 
  # Structure 
  prior(normal(0,2), coef = 'np_und.s'), # Horizontal Heterogeneity
  prior(normal(0,2), coef = 'np_mid.s'),
  prior(normal(0,2), coef = 'np_can.s'),
  prior(normal(0,2), coef = 'np_subcan.s'),
  
  prior(normal(0,2), coef = 'Understory.s'), # Horizontal Amount
  prior(normal(0,2), coef = 'Midstory.s'),
  prior(normal(0,2), coef = 'SubCanopy.s'),
  prior(normal(0,2), coef = 'Canopy.s'),
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#####################
# 6) Amount Model ---
prior_Amount <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Total_veg_amt.s'), # highly correlated
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  # prior(normal(0,2), coef = 'temp.ave_daymet.s'),
  # prior(normal(0,2), coef = 'precip.total_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#####################
# 7) Vertical Model ---
prior_Vertical <- c( 
  # Structure 
  prior(normal(0,2), coef = 'Variance.s'), # Vertical Heterogeneity
  
  prior(normal(0,2), coef = 'Total_veg_amt.s'), # Total Amount
  
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  # prior(normal(0,2), coef = 'temp.ave_daymet.s'),
  # prior(normal(0,2), coef = 'precip.total_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))

#####################
# 8) Climate Model ---
prior_Climate <- c( 
  # Climate
  prior(normal(0,2), coef = 'decimalLatitude.s'),
  prior(normal(0,2), coef = 'elevation.s'),
  
  prior(normal(0,2), coef = 'temp.range_daymet.s'),
  # prior(normal(0,2), coef = 'temp.ave_daymet.s'),
  # prior(normal(0,2), coef = 'precip.total_daymet.s'),
  
  # Intercept
  prior(student_t(30, 0, 2.5), class = 'Intercept'))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/FinalModels")
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/July2023/FinalModels_Redo")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODELS 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

################################################################################
###### Taxonomic Diversity #############################
# Species Richness -------------------------------------------------------------
# T1) SpRich: Full Model------------------------------------------------------------
spRich_chapter1_full <- brm(rangefilt_95_SpRich ~  
                              # Vert Configuration
                              Variance.s + 
                              # Horiz Configuration
                              np_und.s +
                              np_mid.s + 
                              np_subcan.s +
                              np_can.s +
                              # Total Configuration
                              np_TotVol.s +
                              # Horiz Composition
                              Understory.s +
                              Midstory.s +
                              SubCanopy.s +
                              Canopy.s +
                              # Total Composition
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              #temp.ave_daymet.s +
                              # precip.total_daymet.s +
                              # Random effects
                              (1|siteID.factor),
                             
                             data = test,    
                             family = 'poisson',
                             file="FullModal_SpRich",
                             iter= 4000,
                             cores = 4,
                             prior=prior_Full,
                             control = list(adapt_delta = 0.98,
                                            max_treedepth = 20),
                             save_pars = save_pars(all = TRUE),
                             file_refit = 'always')

# Check output
spRich_chapter1_full
describe_posterior(spRich_chapter1_full, rope_ci = 1)
pp_check(spRich_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# T1.5) SpRich: Full Model (minus total veg amount) ------------------------------------------------------------
spRich_chapter1_full_minusTotVol <- brm(rangefilt_95_SpRich ~  
                              # Vert Configuration
                              Variance.s + 
                              # Horiz Configuration
                              np_und.s +
                              np_mid.s + 
                              np_subcan.s +
                              np_can.s +
                              # Total Configuration
                              np_TotVol.s +
                              # Horiz Composition
                              Understory.s +
                              Midstory.s +
                              SubCanopy.s +
                              Canopy.s +
                              # Total Composition
                              # Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              #temp.ave_daymet.s +
                              # precip.total_daymet.s +
                              # Random effects
                              (1|siteID.factor),
                            
                            data = test,    
                            family = 'poisson',
                            file="FullModal_SpRich_minusTotVol",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Full_minusTotVol,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')

# Check output
spRich_chapter1_full_minusTotVol
describe_posterior(spRich_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(spRich_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_full_minusTotVol, ci=0.95, range = c(-0.01, 0.01)))

# T2) Sp Rich: Structure Only Model ------------------------------------------------
spRich_chapter1_structure <- brm(rangefilt_95_SpRich ~  
                                   # Vert Configuration
                                   Variance.s + 
                                   # Horiz Configuration
                                   np_und.s +
                                   np_mid.s + 
                                   np_subcan.s +
                                   np_can.s +
                                   # Total Configuration
                                   np_TotVol.s +
                                   # Horiz Composition
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                   # Total Composition
                                   # Total_veg_amt.s +
                              
                              (1|siteID.factor),
                            
                            data = test,    
                            family = 'poisson',
                            file="Structure_SpRich",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Structure,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
spRich_chapter1_structure
describe_posterior(spRich_chapter1_structure, rope_ci = 1)
pp_check(spRich_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_structure, ci=0.95, range = c(-0.001, 0.001)))

# T3) Sp Rich: Configuration Only Model --------------------------------------------
spRich_chapter1_configuration <- brm(rangefilt_95_SpRich ~  
                                       # Vert Configuration
                                       Variance.s + 
                                       # Horiz Configuration
                                       np_und.s +
                                       np_mid.s + 
                                       np_subcan.s +
                                       np_can.s +
                                       # Total Configuration
                                       np_TotVol.s +
                                 
                                 (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'poisson',
                                 file="Configuration_SpRich",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Configuration,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
spRich_chapter1_configuration
describe_posterior(spRich_chapter1_configuration, rope_ci = 1)
pp_check(spRich_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_configuration, ci=0.95, range = c(-0.001, 0.001)))

# T3_B) Sp Rich: Configuration Only Model --------------------------------------------
spRich_chapter1_configuration_b <- brm(rangefilt_95_SpRich ~  
                                       # Vert Configuration
                                       Variance.s + 
                                       # Horiz Configuration
                                       np_und.s +
                                       np_mid.s + 
                                       np_subcan.s +
                                       np_can.s +
                                       # Total Configuration
                                       np_TotVol.s +
                                         # Climate
                                         decimalLatitude.s +
                                         elevation.s +
                                         temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'poisson',
                                     file="Configuration_SpRich_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Configuration_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
spRich_chapter1_configuration_b
describe_posterior(spRich_chapter1_configuration_b, rope_ci = 1)
pp_check(spRich_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_configuration_b, ci=0.95, range = c(-0.001, 0.001)))

# T4) SpRich: Composition Only Model -----------------------------------------------
spRich_chapter1_composition <- brm(rangefilt_95_SpRich ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s
                                 # Total_veg_amt.s
                                 
                                 + (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'poisson',
                                 file="Composition_SpRich",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
spRich_chapter1_composition
describe_posterior(spRich_chapter1_composition, rope_ci = 1)
pp_check(spRich_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_composition, ci=0.95, range = c(-0.001, 0.001)))

# T4_b) SpRich: Composition Only Model (with climate) -----------------------------------------------
spRich_chapter1_composition_b <- brm(rangefilt_95_SpRich ~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                   # Total_veg_amt.s
                                   # Climate
                                   decimalLatitude.s +
                                     elevation.s +
                                     temp.range_daymet.s +
                                     
                                   (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'poisson',
                                   file="Composition_SpRich_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')
# Check output
spRich_chapter1_composition_b
describe_posterior(spRich_chapter1_composition_b, rope_ci = 1)
pp_check(spRich_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# T5) Sp Rich: Horizontal Only Model ------------------------------------------------
spRich_chapter1_horizontal <- brm(rangefilt_95_SpRich ~  
                                   np_und.s +
                                   np_mid.s +
                                   np_subcan.s + 
                                   np_can.s +
                                   
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s
                                 # Total_veg_amt.s
                                 
                                 + (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'poisson',
                                 file="Horizontal_SpRich",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Horizontal,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
spRich_chapter1_horizontal
describe_posterior(spRich_chapter1_horizontal, rope_ci = 1)
pp_check(spRich_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# T5_b) Sp Rich: Horizontal Only Model ------------------------------------------------
spRich_chapter1_horizontal_b <- brm(rangefilt_95_SpRich ~  
                                    np_und.s +
                                    np_mid.s +
                                    np_subcan.s + 
                                    np_can.s +
                                    
                                    Understory.s +
                                    Midstory.s +
                                    SubCanopy.s +
                                    Canopy.s +
                                  # Total_veg_amt.s
                                    
                                    # Climate
                                    decimalLatitude.s +
                                    elevation.s +
                                    temp.range_daymet.s +
                                    
                                  
                                   (1|siteID.factor),
                                  
                                  data = test,    
                                  family = 'poisson',
                                  file="Horizontal_SpRich_b",
                                  iter= 4000,
                                  cores = 4,
                                  prior=prior_Horizontal_b,
                                  control = list(adapt_delta = 0.98,
                                                 max_treedepth = 20),
                                  save_pars = save_pars(all = TRUE),
                                  file_refit = 'always')
# Check output
spRich_chapter1_horizontal_b
describe_posterior(spRich_chapter1_horizontal_b, rope_ci = 1)
pp_check(spRich_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))


# T6) Sp Rich: Amount Only Model ------------------------------------------------
spRich_chapter1_amount <- brm(rangefilt_95_SpRich ~  
                                    Total_veg_amt.s +
                              
                                # Climate
                                decimalLatitude.s +
                                elevation.s +
                                temp.range_daymet.s +
                                
                                  (1|siteID.factor),
                                  
                                  data = test,    
                                  family = 'poisson',
                                  file="Amount_SpRich",
                                  iter= 4000,
                                  cores = 4,
                                  prior=prior_Amount,
                                  control = list(adapt_delta = 0.98,
                                                 max_treedepth = 20),
                                  save_pars = save_pars(all = TRUE),
                                  file_refit = 'always')
# Check output
spRich_chapter1_amount
describe_posterior(spRich_chapter1_amount, rope_ci = 1)
pp_check(spRich_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# T7) Sp Rich: Vertical Structure Model ------------------------------------------------
spRich_chapter1_Vertical <- brm(rangefilt_95_SpRich ~  
                                  # Vert Configuration
                                  Variance.s + 
                                  
                                  # Total amount 
                                  Total_veg_amt.s +
                                
                                  # Climate
                                decimalLatitude.s +
                                elevation.s +
                                temp.range_daymet.s +
                                
                                (1|siteID.factor),
                              
                              data = test,    
                              family = 'poisson',
                              file="Vertical_SpRich",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Vertical,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
spRich_chapter1_Vertical
describe_posterior(spRich_chapter1_Vertical, rope_ci = 1)
pp_check(spRich_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# T8) Sp Rich: Climate Only ------------------------------------------------
spRich_chapter1_Climate <- brm(rangefilt_95_SpRich ~  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'poisson',
                                file="Climate_SpRich",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Climate,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
spRich_chapter1_Climate
describe_posterior(spRich_chapter1_Climate, rope_ci = 1)
pp_check(spRich_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(spRich_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

spRich_chapter1_full
spRich_chapter1_full_minusTotVol
spRich_chapter1_structure
spRich_chapter1_configuration
spRich_chapter1_configuration_b
spRich_chapter1_composition
spRich_chapter1_composition_b
spRich_chapter1_horizontal
spRich_chapter1_horizontal_b
spRich_chapter1_amount
spRich_chapter1_Vertical
spRich_chapter1_Climate

loo_SpRich_full <- loo(spRich_chapter1_full,moment_match = TRUE)
loo_SpRich_full_minusTotVol <- loo(spRich_chapter1_full_minusTotVol, moment_match = TRUE)
loo_SpRich_structure <- loo(spRich_chapter1_structure, moment_match = TRUE)
loo_SpRich_configuration <- loo(spRich_chapter1_configuration, moment_match = TRUE)
loo_SpRich_configuration_b <- loo(spRich_chapter1_configuration_b, moment_match = TRUE)
loo_SpRich_composition <- loo(spRich_chapter1_composition, moment_match = TRUE)
loo_SpRich_composition_b <- loo(spRich_chapter1_composition_b, moment_match = TRUE)
loo_SpRich_horizontal <- loo(spRich_chapter1_horizontal, moment_match = TRUE)
loo_SpRich_horizontal_b <- loo(spRich_chapter1_horizontal_b, moment_match = TRUE)
loo_SpRich_amount <- loo(spRich_chapter1_amount, moment_match = TRUE)
loo_SpRich_Vertical <- loo(spRich_chapter1_Vertical, moment_match = TRUE)
loo_SpRich_Climate <- loo(spRich_chapter1_Climate, moment_match = TRUE)

# Loo Compare
SpRich_loo_compare <- loo_compare(loo_SpRich_full, loo_SpRich_full_minusTotVol, loo_SpRich_structure, loo_SpRich_configuration, loo_SpRich_configuration_b, loo_SpRich_composition, loo_SpRich_composition_b, loo_SpRich_horizontal, loo_SpRich_horizontal_b, loo_SpRich_amount, loo_SpRich_Vertical, loo_SpRich_Climate)

SpRich_loo_compare
# Output
#                                 elpd_diff se_diff
# spRich_chapter1_Climate           0.0       0.0   
# spRich_chapter1_amount           -0.9       0.6   
# spRich_chapter1_Vertical         -1.6       0.8   
# spRich_chapter1_composition_b    -2.8       1.4   
# spRich_chapter1_configuration_b  -3.6       1.6   
# spRich_chapter1_horizontal_b     -4.9       1.7   
# spRich_chapter1_composition      -6.2       2.5   
# spRich_chapter1_full_minusTotVol -6.4       1.8   
# spRich_chapter1_full             -6.7       1.8   
# spRich_chapter1_configuration    -7.0       2.8   
# spRich_chapter1_horizontal       -8.3       2.7   
# spRich_chapter1_structure        -9.9       2.9 

################################################################################
###### Functional Diversity #############################
# Functional Evenness -------------------------------------------------------------
# F_e_1) FEven: Full Model------------------------------------------------------------
FEve_chapter1_full <- brm( FEve~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                            
                            data = test,    
                            family = 'beta',
                            file="FullModal_FEve",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Full,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')

# Check output
FEve_chapter1_full
describe_posterior(FEve_chapter1_full, rope_ci = 1)
pp_check(FEve_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_full, ci=0.95, range = c(-0.001, 0.001)))

# F_e_1.5) FEven: Full Model------------------------------------------------------------
FEve_chapter1_full_minusTotVol  <- brm( FEve~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             # Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'beta',
                           file="FullModal_FEve_minusTotVol",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full_minusTotVol ,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
FEve_chapter1_full_minusTotVol 
describe_posterior(FEve_chapter1_full_minusTotVol , rope_ci = 1)
pp_check(FEve_chapter1_full_minusTotVol , type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_full_minusTotVol , ci=0.95, range = c(-0.001, 0.001)))

# F_e_2) FEve: Structure Only Model ------------------------------------------------
FEve_chapter1_structure <- brm(FEve ~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +

                                (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'beta',
                                 file="Structure_FEve",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Structure,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FEve_chapter1_structure
describe_posterior(FEve_chapter1_structure, rope_ci = 1)
pp_check(FEve_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_structure, ci=0.95, range = c(-0.001, 0.001)))

# F_e_3) FEve: Configuration Only Model --------------------------------------------
FEve_chapter1_configuration <- brm(FEve ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     
                                     # Total Configuration
                                     np_TotVol.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'beta',
                                     file="Configuration_FEve",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Configuration,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
FEve_chapter1_configuration
describe_posterior(FEve_chapter1_configuration, rope_ci = 1)
pp_check(FEve_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_configuration, ci=0.95, range = c(-0.001, 0.001)))

# F_e_3_b) FEve: Configuration Only Model (with climate) --------------------------------------------
FEve_chapter1_configuration_b <- brm(FEve ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     
                                     # Total Configuration
                                     np_TotVol.s +
                                     
                                     # Climate
                                     decimalLatitude.s +
                                     elevation.s +
                                     temp.range_daymet.s +
                                       
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'beta',
                                   file="Configuration_FEve_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
FEve_chapter1_configuration_b
describe_posterior(FEve_chapter1_configuration_b, rope_ci = 1)
pp_check(FEve_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_configuration_b, ci=0.95, range = c(-0.001, 0.001)))

# F_e_4) FEve: Composition Only Model -----------------------------------------------
FEve_chapter1_composition <- brm(FEve ~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s
                                   # Total_veg_amt.s
                                   
                                   + (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'beta',
                                   file="Composition_FEve",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')
# Check output
FEve_chapter1_composition
describe_posterior(FEve_chapter1_composition, rope_ci = 1)
pp_check(FEve_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_composition, ci=0.95, range = c(-0.001, 0.001)))

# F_e_4_b) FEve: Composition Only Model -----------------------------------------------
FEve_chapter1_composition_b <- brm(FEve ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                 # Total_veg_amt.s
                                 
                                 # Climate
                                 decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                   
                                 (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'beta',
                                 file="Composition_FEve_b",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition_b,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FEve_chapter1_composition_b
describe_posterior(FEve_chapter1_composition_b, rope_ci = 1)
pp_check(FEve_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# F_e_5) FEve: Horizontal Only Model ------------------------------------------------
FEve_chapter1_horizontal <- brm(FEve ~  
                                    np_und.s +
                                    np_mid.s +
                                    np_subcan.s +
                                    np_can.s +
                                    
                                    Understory.s +
                                    Midstory.s +
                                    SubCanopy.s +
                                    Canopy.s
                                  # Total_veg_amt.s
                                  
                                  + (1|siteID.factor),
                                  
                                  data = test,    
                                  family = 'beta',
                                  file="Horizontal_FEve",
                                  iter= 4000,
                                  cores = 4,
                                  prior=prior_Horizontal,
                                  control = list(adapt_delta = 0.98,
                                                 max_treedepth = 20),
                                  save_pars = save_pars(all = TRUE),
                                  file_refit = 'always')
# Check output
FEve_chapter1_horizontal
describe_posterior(FEve_chapter1_horizontal, rope_ci = 1)
pp_check(FEve_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# F_e_5_b) FEve: Horizontal Only Model (With Climate)------------------------------------------------
FEve_chapter1_horizontal_b <- brm(FEve ~  
                                  np_und.s +
                                  np_mid.s +
                                  np_subcan.s +
                                  np_can.s +
                                  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                # Total_veg_amt.s
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                
                                (1|siteID.factor),
                                
                                data = test,    
                                family = 'beta',
                                file="Horizontal_FEve_b",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal_b,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FEve_chapter1_horizontal_b
describe_posterior(FEve_chapter1_horizontal_b, rope_ci = 1)
pp_check(FEve_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# F_e_6) FEve: Amount Only Model ------------------------------------------------
FEve_chapter1_amount <- brm(FEve ~  
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                                (1|siteID.factor),
                              
                              data = test,    
                              family = 'beta',
                              file="Amount_FEve",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Amount,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
FEve_chapter1_amount
describe_posterior(FEve_chapter1_amount, rope_ci = 1)
pp_check(FEve_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# F_e_7) FEve: Vertical Structure Model ------------------------------------------------
FEve_chapter1_Vertical <- brm(FEve ~   
                                  # Vert Configuration
                                  Variance.s + 
                                  
                                  # Total amount 
                                  Total_veg_amt.s +
                                  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'beta',
                                file="Vertical_FEve",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Vertical,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FEve_chapter1_Vertical
describe_posterior(FEve_chapter1_Vertical, rope_ci = 1)
pp_check(FEve_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# F_e_8) FEve: Climate Only ------------------------------------------------
FEve_chapter1_Climate <- brm(FEve ~  
                                 # Climate
                                 decimalLatitude.s +
                                 elevation.s +
                                 temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'beta',
                               file="Climate_FEve",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Climate,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
FEve_chapter1_Climate
describe_posterior(FEve_chapter1_Climate, rope_ci = 1)
pp_check(FEve_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(FEve_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

FEve_chapter1_full
FEve_chapter1_full_minusTotVol
FEve_chapter1_structure
FEve_chapter1_configuration
FEve_chapter1_configuration_b
FEve_chapter1_composition
FEve_chapter1_composition_b
FEve_chapter1_horizontal
FEve_chapter1_horizontal_b
FEve_chapter1_amount
FEve_chapter1_Vertical
FEve_chapter1_Climate

loo_FEve_full <- loo(FEve_chapter1_full,moment_match = TRUE)
loo_FEve_full_minusTotVol <- loo(FEve_chapter1_full_minusTotVol, moment_match = TRUE)
loo_FEve_structure <- loo(FEve_chapter1_structure, moment_match = TRUE)
loo_FEve_configuration <- loo(FEve_chapter1_configuration, moment_match = TRUE)
loo_FEve_configuration_b <- loo(FEve_chapter1_configuration_b, moment_match = TRUE)
loo_FEve_composition <- loo(FEve_chapter1_composition, moment_match = TRUE)
loo_FEve_composition_b <- loo(FEve_chapter1_composition_b, moment_match = TRUE)
loo_FEve_horizontal <- loo(FEve_chapter1_horizontal, moment_match = TRUE)
loo_FEve_horizontal_b <- loo(FEve_chapter1_horizontal_b, moment_match = TRUE)
loo_FEve_amount <- loo(FEve_chapter1_amount, moment_match = TRUE)
loo_FEve_Vertical <- loo(FEve_chapter1_Vertical, moment_match = TRUE)
loo_FEve_Climate <- loo(FEve_chapter1_Climate, moment_match = TRUE)

# Loo Compare
FEve_loo_compare <- loo_compare(loo_FEve_full, loo_FEve_full_minusTotVol, loo_FEve_structure, loo_FEve_configuration, loo_FEve_configuration_b, loo_FEve_composition, loo_FEve_composition_b, loo_FEve_horizontal, loo_FEve_horizontal_b, loo_FEve_amount, loo_FEve_Vertical, loo_FEve_Climate)

FEve_loo_compare
# Output
# elpd_diff se_diff
# FEve_chapter1_Climate           0.0       0.0   
# FEve_chapter1_amount           -1.2       0.4   
# FEve_chapter1_Vertical         -2.2       0.4   
# FEve_chapter1_composition      -2.6       1.9   
# FEve_chapter1_composition_b    -2.9       1.7   
# FEve_chapter1_horizontal       -4.6       3.7   
# FEve_chapter1_horizontal_b     -5.2       3.7   
# FEve_chapter1_structure        -5.8       3.9   
# FEve_chapter1_full             -6.1       3.8   
# FEve_chapter1_configuration    -6.1       1.9   
# FEve_chapter1_full_minusTotVol -6.2       3.8   
# FEve_chapter1_configuration_b  -6.3       1.7  


# Functional Divergence ---------------------------------------------------------
# F_d_1) FDiv: Full Model------------------------------------------------------------
FDiv_chapter1_full <- brm( FDiv~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_FDiv",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
FDiv_chapter1_full
describe_posterior(FDiv_chapter1_full, rope_ci = 1)
pp_check(FDiv_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_full, ci=0.95, range = c(-0.001, 0.001)))

# F_d_1.5) FDiv: Full Model_minusTotVol-----------------------------------------
FDiv_chapter1_full_minusTotVol <- brm( FDiv~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             # Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_FDiv_minusTotVol",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full_minusTotVol,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
FDiv_chapter1_full_minusTotVol
describe_posterior(FDiv_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(FDiv_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_full_minusTotVol, ci=0.95, range = c(-0.001, 0.001)))

# F_d_2) FDiv: Structure Only Model ------------------------------------------------
FDiv_chapter1_structure <- brm(FDiv ~  
                                 # Shannon.s +
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +
                                 
                                (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_FDiv",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
FDiv_chapter1_structure
describe_posterior(FDiv_chapter1_structure, rope_ci = 1)
pp_check(FDiv_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_structure, ci=0.95, range = c(-0.001, 0.001)))
           
# F_d_3) FDiv: Configuration Only Model --------------------------------------------
FDiv_chapter1_configuration <- brm(FDiv ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     # Total Configuration
                                     np_TotVol.s +
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Configuration_FDiv",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
FDiv_chapter1_configuration
describe_posterior(FDiv_chapter1_configuration, rope_ci = 1)
pp_check(FDiv_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_configuration, ci=0.95, range = c(-0.001, 0.001)))

# F_d_3_b) FDiv: Configuration Only Model (with climate)--------------------------------------------
FDiv_chapter1_configuration_b <- brm(FDiv ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     # Total Configuration
                                     np_TotVol.s +
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Configuration_FDiv_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
FDiv_chapter1_configuration_b
describe_posterior(FDiv_chapter1_configuration_b, rope_ci = 1)
pp_check(FDiv_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_configuration_b, ci=0.95, range = c(-0.001, 0.001)))

# F_d_4) FDiv: Composition Only Model -----------------------------------------------
FDiv_chapter1_composition <- brm(FDiv ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                 # Total_veg_amt.s
                                 
                                  (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Composition_FDiv",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FDiv_chapter1_composition
describe_posterior(FDiv_chapter1_composition, rope_ci = 1)
pp_check(FDiv_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_composition, ci=0.95, range = c(-0.001, 0.001)))

# F_d_4_b) FDiv: Composition Only Model -----------------------------------------------
FDiv_chapter1_composition_b <- brm(FDiv ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                 # Total_veg_amt.s
                                 # Climate
                                 decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                 
                                  (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Composition_FDiv_b",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition_b,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FDiv_chapter1_composition_b
describe_posterior(FDiv_chapter1_composition_b, rope_ci = 1)
pp_check(FDiv_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# F_d_5) FDiv: Horizontal Only Model ------------------------------------------------
FDiv_chapter1_horizontal <- brm(FDiv ~  
                                  np_und.s +
                                  np_mid.s +
                                  np_subcan.s + 
                                  np_can.s +
                                  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Horizontal_FDiv",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FDiv_chapter1_horizontal
describe_posterior(FDiv_chapter1_horizontal, rope_ci = 1)
pp_check(FDiv_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# F_d_5_b) FDiv: Horizontal Only Model ------------------------------------------------
FDiv_chapter1_horizontal_b <- brm(FDiv ~  
                                  np_und.s +
                                  np_mid.s +
                                  np_subcan.s + 
                                  np_can.s +
                                  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                    # Climate
                                    decimalLatitude.s +
                                    elevation.s +
                                    temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Horizontal_FDiv_b",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal_b,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FDiv_chapter1_horizontal_b
describe_posterior(FDiv_chapter1_horizontal_b, rope_ci = 1)
pp_check(FDiv_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# F_d_6) FDiv: Amount Only Model ------------------------------------------------
FDiv_chapter1_amount <- brm(FDiv ~  
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                              (1|siteID.factor),
                            
                            data = test,    
                            family = 'gaussian',
                            file="Amount_FDiv",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Amount,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
FDiv_chapter1_amount
describe_posterior(FDiv_chapter1_amount, rope_ci = 1)
pp_check(FDiv_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# F_d_7) FDiv: Vertical Structure Model ------------------------------------------------
FDiv_chapter1_Vertical <- brm(FDiv ~  
                              # Vert Configuration
                              Variance.s + 
                              
                              # Total amount 
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                                
                                (1|siteID.factor),
                            
                            data = test,    
                            family = 'gaussian',
                            file="Amount_FDiv_Vertical",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Vertical,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
FDiv_chapter1_Vertical 
describe_posterior(FDiv_chapter1_Vertical, rope_ci = 1)
pp_check(FDiv_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# F_d_7) FDiv: Climate Only Model ------------------------------------------------
FDiv_chapter1_Climate  <- brm(FDiv ~  
                                # Climate
                                decimalLatitude.s +
                                elevation.s +
                                temp.range_daymet.s +
                                
                                (1|siteID.factor),
                              
                              data = test,    
                              family = 'gaussian',
                              file="Amount_FDiv_Climate",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Climate,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
FDiv_chapter1_Climate 
describe_posterior(FDiv_chapter1_Climate, rope_ci = 1)
pp_check(FDiv_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(FDiv_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

FDiv_chapter1_full
FDiv_chapter1_full_minusTotVol
FDiv_chapter1_structure
FDiv_chapter1_configuration
FDiv_chapter1_configuration_b
FDiv_chapter1_composition
FDiv_chapter1_composition_b
FDiv_chapter1_horizontal
FDiv_chapter1_horizontal_b
FDiv_chapter1_amount
FDiv_chapter1_Vertical
FDiv_chapter1_Climate

loo_FDiv_full <- loo(FDiv_chapter1_full,moment_match = TRUE)
loo_FDiv_full_minusTotVol <- loo(FDiv_chapter1_full_minusTotVol, moment_match = TRUE)
loo_FDiv_structure <- loo(FDiv_chapter1_structure, moment_match = TRUE)
loo_FDiv_configuration <- loo(FDiv_chapter1_configuration, moment_match = TRUE)
loo_FDiv_configuration_b <- loo(FDiv_chapter1_configuration_b, moment_match = TRUE)
loo_FDiv_composition <- loo(FDiv_chapter1_composition, moment_match = TRUE)
loo_FDiv_composition_b <- loo(FDiv_chapter1_composition_b, moment_match = TRUE)
loo_FDiv_horizontal <- loo(FDiv_chapter1_horizontal, moment_match = TRUE)
loo_FDiv_horizontal_b <- loo(FDiv_chapter1_horizontal_b, moment_match = TRUE)
loo_FDiv_amount <- loo(FDiv_chapter1_amount, moment_match = TRUE)
loo_FDiv_Vertical <- loo(FDiv_chapter1_Vertical, moment_match = TRUE)
loo_FDiv_Climate <- loo(FDiv_chapter1_Climate, moment_match = TRUE)

# Loo Compare
FDiv_loo_compare <- loo_compare(loo_FDiv_full, loo_FDiv_full_minusTotVol, loo_FDiv_structure, loo_FDiv_configuration, loo_FDiv_configuration_b, loo_FDiv_composition, loo_FDiv_composition_b, loo_FDiv_horizontal, loo_FDiv_horizontal_b, loo_FDiv_amount, loo_FDiv_Vertical, loo_FDiv_Climate)

FDiv_loo_compare

# Output
#                               elpd_diff se_diff
# FDiv_chapter1_composition        0.0       0.0  
# FDiv_chapter1_composition_b     -1.3       1.8  
# FDiv_chapter1_horizontal_b      -1.3       3.0  
# FDiv_chapter1_structure         -1.4       2.9  
# FDiv_chapter1_full_minusTotVol  -1.4       3.5  
# FDiv_chapter1_full              -1.6       3.5  
# FDiv_chapter1_horizontal        -1.7       2.6  
# FDiv_chapter1_amount            -6.1       4.7  
# FDiv_chapter1_Vertical          -6.3       5.0  
# FDiv_chapter1_Climate           -8.3       6.1  
# FDiv_chapter1_configuration    -10.2       6.8  
# FDiv_chapter1_configuration_b  -10.9       6.7  

# Functional Richness (SES) -------------------------------------------------------
# F_r_1) FRic: Full Model------------------------------------------------------------
FRic_chapter1_full <- brm( SES.FRic~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_FRic",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
FRic_chapter1_full
describe_posterior(FRic_chapter1_full, rope_ci = 1)
pp_check(FRic_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# F_r_1.5) FRic: Full Model------------------------------------------------------------
FRic_chapter1_full_minusTotVol <- brm( SES.FRic~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             # Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_FRic_minusTotVol",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full_minusTotVol,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
FRic_chapter1_full_minusTotVol
describe_posterior(FRic_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(FRic_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_full_minusTotVol, ci=0.95, range = c(-0.01, 0.01)))

# F_r_2) FRic: Structure Only Model ------------------------------------------------
FRic_chapter1_structure <- brm(SES.FRic ~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_FRic",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
FRic_chapter1_structure
describe_posterior(FRic_chapter1_structure, rope_ci = 1)
pp_check(FRic_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_structure, ci=0.95, range = c(-0.01, 0.01)))

# F_r_3) FRic: Configuration Only Model --------------------------------------------
FRic_chapter1_configuration <- brm(SES.FRic ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     # Total Configuration
                                     np_TotVol.s +
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Configuration_FRic",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
FRic_chapter1_configuration
describe_posterior(FRic_chapter1_configuration, rope_ci = 1)
pp_check(FRic_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# F_r_3_b) FRic: Configuration Only Model (with climate)--------------------------------------------
FRic_chapter1_configuration_b <- brm(SES.FRic ~  
                                     # Vert Configuration
                                     Variance.s + 
                                     # Horiz Configuration
                                     np_und.s +
                                     np_mid.s + 
                                     np_subcan.s +
                                     np_can.s +
                                     # Total Configuration
                                     np_TotVol.s +
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Configuration_FRic_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
FRic_chapter1_configuration_b
describe_posterior(FRic_chapter1_configuration_b, rope_ci = 1)
pp_check(FRic_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# F_r_4) FRic: Composition Only Model -----------------------------------------------
FRic_chapter1_composition <- brm(SES.FRic ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                   # Total_veg_amt.s
                                   
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Composition_FRic",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FRic_chapter1_composition
describe_posterior(FRic_chapter1_composition, rope_ci = 1)
pp_check(FRic_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# F_r_4_b) FRic: Composition Only Model -----------------------------------------------
FRic_chapter1_composition_b <- brm(SES.FRic ~  
                                   Understory.s +
                                   Midstory.s +
                                   SubCanopy.s +
                                   Canopy.s +
                                   # Total_veg_amt.s
                                     # Climate
                                     decimalLatitude.s +
                                     elevation.s +
                                     temp.range_daymet.s +
                                   
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Composition_FRic_b",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition_b,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
FRic_chapter1_composition_b
describe_posterior(FRic_chapter1_composition_b, rope_ci = 1)
pp_check(FRic_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_composition_b, ci=0.95, range = c(-0.01, 0.01)))

# F_r_5) FRic: Horizontal Only Model ------------------------------------------------
FRic_chapter1_horizontal <- brm(SES.FRic ~  
                                  np_und.s +
                                  np_mid.s +
                                  np_subcan.s + 
                                  np_can.s +
                                  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Horizontal_FRic",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FRic_chapter1_horizontal
describe_posterior(FRic_chapter1_horizontal, rope_ci = 1)
pp_check(FRic_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# F_r_5_b) FRic: Horizontal Only Model ------------------------------------------------
FRic_chapter1_horizontal_b <- brm(SES.FRic ~  
                                  np_und.s +
                                  np_mid.s +
                                  np_subcan.s + 
                                  np_can.s +
                                  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                    # Climate
                                    decimalLatitude.s +
                                    elevation.s +
                                    temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Horizontal_FRic_b",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal_b,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
FRic_chapter1_horizontal_b
describe_posterior(FRic_chapter1_horizontal_b, rope_ci = 1)
pp_check(FRic_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# F_r_6) FRic: Amount Only Model ------------------------------------------------
FRic_chapter1_amount <- brm(SES.FRic ~  
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                              (1|siteID.factor),
                            
                            data = test,    
                            family = 'gaussian',
                            file="Amount_FRic",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Amount,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
FRic_chapter1_amount
describe_posterior(FRic_chapter1_amount, rope_ci = 1)
pp_check(FRic_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# F_r_7) FRic: Vertical Structure Model ------------------------------------------------
FRic_chapter1_Vertical <- brm(SES.FRic ~  
                              # Vert Configuration
                              Variance.s + 
                              
                              # Total amount 
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                              (1|siteID.factor),
                            
                            data = test,    
                            family = 'gaussian',
                            file="Vertical_FRic",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Vertical,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
FRic_chapter1_Vertical
describe_posterior(FRic_chapter1_Vertical, rope_ci = 1)
pp_check(FRic_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# F_r_8) FRic: Climate Model ------------------------------------------------
FRic_chapter1_Climate  <- brm(SES.FRic ~  
                                # Climate
                                decimalLatitude.s +
                                elevation.s +
                                temp.range_daymet.s +
                                
                                (1|siteID.factor),
                              
                              data = test,    
                              family = 'gaussian',
                              file="Climate_FRic",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Climate,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
FRic_chapter1_Climate
describe_posterior(FRic_chapter1_Climate, rope_ci = 1)
pp_check(FRic_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(FRic_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

FRic_chapter1_full
FRic_chapter1_full_minusTotVol
FRic_chapter1_structure
FRic_chapter1_configuration
FRic_chapter1_configuration_b
FRic_chapter1_composition
FRic_chapter1_composition_b
FRic_chapter1_horizontal
FRic_chapter1_horizontal_b
FRic_chapter1_amount
FRic_chapter1_Vertical
FRic_chapter1_Climate

loo_FRic_full <- loo(FRic_chapter1_full,moment_match = TRUE)
loo_FRic_full_minusTotVol <- loo(FRic_chapter1_full_minusTotVol, moment_match = TRUE)
loo_FRic_structure <- loo(FRic_chapter1_structure, moment_match = TRUE)
loo_FRic_configuration <- loo(FRic_chapter1_configuration, moment_match = TRUE)
loo_FRic_configuration_b <- loo(FRic_chapter1_configuration_b, moment_match = TRUE)
loo_FRic_composition <- loo(FRic_chapter1_composition, moment_match = TRUE)
loo_FRic_composition_b <- loo(FRic_chapter1_composition_b, moment_match = TRUE)
loo_FRic_horizontal <- loo(FRic_chapter1_horizontal, moment_match = TRUE)
loo_FRic_horizontal_b <- loo(FRic_chapter1_horizontal_b, moment_match = TRUE)
loo_FRic_amount <- loo(FRic_chapter1_amount, moment_match = TRUE)
loo_FRic_Vertical <- loo(FRic_chapter1_Vertical, moment_match = TRUE)
loo_FRic_Climate <- loo(FRic_chapter1_Climate, moment_match = TRUE)

# Loo Compare
FRic_loo_compare <- loo_compare(loo_FRic_full, loo_FRic_full_minusTotVol, loo_FRic_structure, loo_FRic_configuration, loo_FRic_configuration_b, loo_FRic_composition, loo_FRic_composition_b, loo_FRic_horizontal, loo_FRic_horizontal_b, loo_FRic_amount, loo_FRic_Vertical, loo_FRic_Climate)

FRic_loo_compare
# Output
#                               elpd_diff se_diff
# FRic_chapter1_Vertical          0.0       0.0   
# FRic_chapter1_amount            0.0       1.3   
# FRic_chapter1_composition      -0.6       2.6   
# FRic_chapter1_Climate          -0.7       2.3   
# FRic_chapter1_composition_b    -1.8       1.9   
# FRic_chapter1_configuration    -2.4       2.3   
# FRic_chapter1_horizontal       -2.5       3.2   
# FRic_chapter1_structure        -2.8       2.8   
# FRic_chapter1_horizontal_b     -3.8       2.7   
# FRic_chapter1_configuration_b  -4.3       2.0   
# FRic_chapter1_full_minusTotVol -4.8       2.4   
# FRic_chapter1_full             -5.0       2.4  

# PC1 (Functional Axis) -------------------------------------------------------
# PC1_1) PC1: Full Model------------------------------------------------------------
PC1_chapter1_full <- brm( PC1~  
                             # Vert Configuration
                             Variance.s + 
                             # Horiz Configuration
                             np_und.s +
                             np_mid.s + 
                             np_subcan.s +
                             np_can.s +
                             # Total Configuration
                             np_TotVol.s +
                             # Horiz Composition
                             Understory.s +
                             Midstory.s +
                             SubCanopy.s +
                             Canopy.s +
                             # Total Composition
                             Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             #temp.ave_daymet.s +
                             # precip.total_daymet.s +
                             # Random effects
                             (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_PC1",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
PC1_chapter1_full
describe_posterior(PC1_chapter1_full, rope_ci = 1)
pp_check(PC1_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# PC1_1.5) PC1: Full Model------------------------------------------------------------
PC1_chapter1_full_minusTotVol <- brm( PC1~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            # Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="FullModal_PC1_minusTotVol",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Full_minusTotVol,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')

# Check output
PC1_chapter1_full_minusTotVol
describe_posterior(PC1_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(PC1_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_full_minusTotVol, ci=0.95, range = c(-0.01, 0.01)))

# PC1_2) PC1: Strucutre Only Model------------------------------------------------------------
PC1_chapter1_structure <- brm( PC1~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Horiz Composition
                                        Understory.s +
                                        Midstory.s +
                                        SubCanopy.s +
                                        Canopy.s +
                                        # Total Composition
                                        # Total_veg_amt.s +
                                        
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="Structure_PC1",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Structure,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC1_chapter1_structure
describe_posterior(PC1_chapter1_structure, rope_ci = 1)
pp_check(PC1_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_structure, ci=0.95, range = c(-0.01, 0.01)))

# PC1_3) PC1: Configuration Only Model------------------------------------------------------------
PC1_chapter1_configuration  <- brm( PC1~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Configuration_PC1",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Configuration,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC1_chapter1_configuration
describe_posterior(PC1_chapter1_configuration, rope_ci = 1)
pp_check(PC1_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# PC1_3_b) PC1: Configuration Only Model (with climate)------------------------------------------------------------
PC1_chapter1_configuration_b  <- brm( PC1~  
                                      # Vert Configuration
                                      Variance.s + 
                                      # Horiz Configuration
                                      np_und.s +
                                      np_mid.s + 
                                      np_subcan.s +
                                      np_can.s +
                                      # Total Configuration
                                      np_TotVol.s +
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                      
                                      (1|siteID.factor),
                                    
                                    data = test,    
                                    family = 'gaussian',
                                    file="Configuration_PC1_b",
                                    iter= 4000,
                                    cores = 4,
                                    prior=prior_Configuration_b,
                                    control = list(adapt_delta = 0.98,
                                                   max_treedepth = 20),
                                    save_pars = save_pars(all = TRUE),
                                    file_refit = 'always')

# Check output
PC1_chapter1_configuration_b
describe_posterior(PC1_chapter1_configuration_b, rope_ci = 1)
pp_check(PC1_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# PC1_4) PC1: Composition Only Model------------------------------------------------------------
PC1_chapter1_composition   <- brm( PC1~  
                                        Understory.s +
                                        Midstory.s +
                                        SubCanopy.s +
                                        Canopy.s +
                                        # Total_veg_amt.s
                                        
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="Composition_PC1",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Composition,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC1_chapter1_composition
describe_posterior(PC1_chapter1_composition, rope_ci = 1)
pp_check(PC1_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# PC1_4_b) PC1: Composition Only Model (with climate) ------------------------------------------------------------
PC1_chapter1_composition_b   <- brm( PC1~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Composition_PC1_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC1_chapter1_composition_b
describe_posterior(PC1_chapter1_composition_b, rope_ci = 1)
pp_check(PC1_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_composition_b, ci=0.95, range = c(-0.01, 0.01)))

# PC1_5) PC1: Horizontal Only Model------------------------------------------------------------
PC1_chapter1_horizontal    <- brm( PC1~  
                                     np_und.s +
                                     np_mid.s +
                                     np_subcan.s + 
                                     np_can.s +
                                     
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Horizontal_PC1",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Horizontal,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC1_chapter1_horizontal
describe_posterior(PC1_chapter1_horizontal, rope_ci = 1)
pp_check(PC1_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_horizontal, ci=0.95, range = c(-0.01, 0.01)))

# PC1_5_b) PC1: Horizontal Only Model (with climate) ------------------------------------------------------------
PC1_chapter1_horizontal_b    <- brm( PC1~  
                                     np_und.s +
                                     np_mid.s +
                                     np_subcan.s + 
                                     np_can.s +
                                     
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Horizontal_PC1_b",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Horizontal_b,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC1_chapter1_horizontal_b
describe_posterior(PC1_chapter1_horizontal_b, rope_ci = 1)
pp_check(PC1_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_horizontal_b, ci=0.95, range = c(-0.01, 0.01)))

# PC1_6) PC1: Amount Only Model  ------------------------------------------------------------
PC1_chapter1_amount    <- brm( PC1~  
                                       Total_veg_amt.s +
                                 
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Amount_PC1",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Amount,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC1_chapter1_amount
describe_posterior(PC1_chapter1_amount, rope_ci = 1)
pp_check(PC1_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_amount, ci=0.95, range = c(-0.01, 0.01)))

# PC1_7) PC1: Vertical Structure Model  ------------------------------------------------------------
PC1_chapter1_Vertical    <- brm( PC1~  
                                 # Vert Configuration
                                 Variance.s + 
                                 
                                 # Total amount 
                                 Total_veg_amt.s +
                                 
                                 # Climate
                                 decimalLatitude.s +
                                 elevation.s +
                                 temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Vertical_PC1",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Vertical,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC1_chapter1_Vertical
describe_posterior(PC1_chapter1_Vertical, rope_ci = 1)
pp_check(PC1_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_Vertical, ci=0.95, range = c(-0.01, 0.01)))

# PC1_8) PC1: Climate Only Model  ------------------------------------------------------------
PC1_chapter1_Climate    <- brm( PC1~  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Climate_PC1",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Climate,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
PC1_chapter1_Climate
describe_posterior(PC1_chapter1_Climate, rope_ci = 1)
pp_check(PC1_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter1_Climate, ci=0.95, range = c(-0.01, 0.01)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

PC1_chapter1_full
PC1_chapter1_full_minusTotVol
PC1_chapter1_structure
PC1_chapter1_configuration
PC1_chapter1_configuration_b
PC1_chapter1_composition
PC1_chapter1_composition_b
PC1_chapter1_horizontal
PC1_chapter1_horizontal_b
PC1_chapter1_amount
PC1_chapter1_Vertical
PC1_chapter1_Climate

loo_PC1_full <- loo(PC1_chapter1_full,moment_match = TRUE)
loo_PC1_full_minusTotVol <- loo(PC1_chapter1_full_minusTotVol, moment_match = TRUE)
loo_PC1_structure <- loo(PC1_chapter1_structure, moment_match = TRUE)
loo_PC1_configuration <- loo(PC1_chapter1_configuration, moment_match = TRUE)
loo_PC1_configuration_b <- loo(PC1_chapter1_configuration_b, moment_match = TRUE)
loo_PC1_composition <- loo(PC1_chapter1_composition, moment_match = TRUE)
loo_PC1_composition_b <- loo(PC1_chapter1_composition_b, moment_match = TRUE)
loo_PC1_horizontal <- loo(PC1_chapter1_horizontal, moment_match = TRUE)
loo_PC1_horizontal_b <- loo(PC1_chapter1_horizontal_b, moment_match = TRUE)
loo_PC1_amount <- loo(PC1_chapter1_amount, moment_match = TRUE)
loo_PC1_Vertical <- loo(PC1_chapter1_Vertical, moment_match = TRUE)
loo_PC1_Climate <- loo(PC1_chapter1_Climate, moment_match = TRUE)

# Loo Compare
PC1_loo_compare <- loo_compare(loo_PC1_full, loo_PC1_full_minusTotVol, loo_PC1_structure, loo_PC1_configuration, loo_PC1_configuration_b, loo_PC1_composition, loo_PC1_composition_b, loo_PC1_horizontal, loo_PC1_horizontal_b, loo_PC1_amount, loo_PC1_Vertical, loo_PC1_Climate)

PC1_loo_compare
# Output
#                             elpd_diff se_diff
# PC1_chapter1_Climate           0.0       0.0   
# PC1_chapter1_amount            0.0       1.1   
# PC1_chapter1_Vertical         -0.6       2.6   
# PC1_chapter1_composition      -0.8       1.7   
# PC1_chapter1_structure        -1.0       4.5   
# PC1_chapter1_composition_b    -1.5       1.5   
# PC1_chapter1_horizontal_b     -1.6       2.7   
# PC1_chapter1_configuration    -2.1       3.0   
# PC1_chapter1_full             -2.3       4.6   
# PC1_chapter1_configuration_b  -2.6       2.8   
# PC1_chapter1_horizontal       -2.9       3.2   
# PC1_chapter1_full_minusTotVol -4.8       3.1  


# PC2 (Functional Axis) -------------------------------------------------------
# PC2_1) PC2: Full Model------------------------------------------------------------
PC2_chapter1_full <- brm( PC2~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="FullModal_PC2",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Full,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')

# Check output
PC2_chapter1_full
describe_posterior(PC2_chapter1_full, rope_ci = 1)
pp_check(PC2_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# PC2_1.5) PC2: Full Model------------------------------------------------------------
PC2_chapter1_full_minusTotVol <- brm( PC2~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Horiz Composition
                                        Understory.s +
                                        Midstory.s +
                                        SubCanopy.s +
                                        Canopy.s +
                                        # Total Composition
                                        # Total_veg_amt.s +
                                        
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        #temp.ave_daymet.s +
                                        # precip.total_daymet.s +
                                        # Random effects
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="FullModal_PC2_minusTotVol",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Full_minusTotVol,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC2_chapter1_full_minusTotVol
describe_posterior(PC2_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(PC2_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_full_minusTotVol, ci=0.95, range = c(-0.001, 0.001)))

# PC2_2) PC2: Structure Only Model------------------------------------------------------------
PC2_chapter1_structure <- brm( PC2~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_PC2",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC2_chapter1_structure
describe_posterior(PC2_chapter1_structure, rope_ci = 1)
pp_check(PC1_chapter2_structure, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter2_structure, ci=0.95, range = c(-0.01, 0.01)))

# PC2_3) PC2: Configuration Only Model------------------------------------------------------------
PC2_chapter1_configuration  <- brm( PC2~  
                                      # Vert Configuration
                                      Variance.s + 
                                      # Horiz Configuration
                                      np_und.s +
                                      np_mid.s + 
                                      np_subcan.s +
                                      np_can.s +
                                      # Total Configuration
                                      np_TotVol.s +
                                      
                                      (1|siteID.factor),
                                    
                                    data = test,    
                                    family = 'gaussian',
                                    file="Configuration_PC2",
                                    iter= 4000,
                                    cores = 4,
                                    prior=prior_Configuration,
                                    control = list(adapt_delta = 0.98,
                                                   max_treedepth = 20),
                                    save_pars = save_pars(all = TRUE),
                                    file_refit = 'always')

# Check output
PC2_chapter1_configuration
describe_posterior(PC2_chapter1_configuration, rope_ci = 1)
pp_check(PC2_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# PC2_3_b) PC2: Configuration Only Model (with climate)------------------------------------------------------------
PC2_chapter1_configuration_b  <- brm( PC2~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="Configuration_PC2_b",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Configuration_b,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC2_chapter1_configuration_b
describe_posterior(PC2_chapter1_configuration_b, rope_ci = 1)
pp_check(PC2_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# PC2_4) PC2: Composition Only Model------------------------------------------------------------
PC2_chapter1_composition   <- brm( PC2~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Composition_PC2",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC2_chapter1_composition
describe_posterior(PC2_chapter1_composition, rope_ci = 1)
pp_check(PC2_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# PC2_4_b) PC2: Composition Only Model (with climate) --------------------------------------------------------
PC2_chapter1_composition_b   <- brm( PC2~  
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Composition_PC2_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Composition_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC2_chapter1_composition_b
describe_posterior(PC2_chapter1_composition_b, rope_ci = 1)
pp_check(PC2_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# PC2_5) PC1: Horizontal Only Model------------------------------------------------------------
PC2_chapter1_horizontal    <- brm( PC2~  
                                     np_und.s +
                                     np_mid.s +
                                     np_subcan.s + 
                                     np_can.s +
                                     
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Horizontal_PC2",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Horizontal,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC2_chapter1_horizontal
describe_posterior(PC2_chapter1_horizontal, rope_ci = 1)
pp_check(PC1_chapter2_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(PC1_chapter2_horizontal, ci=0.95, range = c(-0.01, 0.01)))

# PC2_5_b) PC1: Horizontal Only Model (with climate) ------------------------------------------------------------
PC2_chapter1_horizontal_b    <- brm( PC2~  
                                       np_und.s +
                                       np_mid.s +
                                       np_subcan.s + 
                                       np_can.s +
                                       
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Horizontal_PC2_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Horizontal_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC2_chapter1_horizontal_b
describe_posterior(PC2_chapter1_horizontal_b, rope_ci = 1)
pp_check(PC2_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# PC2_6) PC1: Amount Only Model  ------------------------------------------------------------
PC2_chapter1_amount    <- brm( PC2~  
                                 Total_veg_amt.s +
                                 
                                 # Climate
                                 decimalLatitude.s +
                                 elevation.s +
                                 temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Amount_PC2",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Amount,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC2_chapter1_amount
describe_posterior(PC2_chapter1_amount, rope_ci = 1)
pp_check(PC2_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_amount, ci=0.95, range = c(-0.01, 0.01)))

# PC2_7) PC2: Vertical Structure Model  ------------------------------------------------------------
PC2_chapter1_Vertical    <- brm( PC2~  
                                   # Vert Configuration
                                   Variance.s + 
                                   
                                   # Total amount 
                                   Total_veg_amt.s +
                                   
                                   # Climate
                                   decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                   
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Vertical_PC2",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Vertical,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
PC2_chapter1_Vertical
describe_posterior(PC2_chapter1_Vertical, rope_ci = 1)
pp_check(PC2_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_Vertical, ci=0.95, range = c(-0.01, 0.01)))

# PC2_8) PC2: Climate Only Model  ------------------------------------------------------------
PC2_chapter1_Climate    <- brm( PC2~  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Climate_PC2",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Climate,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')

# Check output
PC2_chapter1_Climate
describe_posterior(PC2_chapter1_Climate, rope_ci = 1)
pp_check(PC2_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(PC2_chapter1_Climate, ci=0.95, range = c(-0.01, 0.01)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

PC2_chapter1_full
PC2_chapter1_full_minusTotVol
PC2_chapter1_structure
PC2_chapter1_configuration
PC2_chapter1_configuration_b
PC2_chapter1_composition
PC2_chapter1_composition_b
PC2_chapter1_horizontal
PC2_chapter1_horizontal_b
PC2_chapter1_amount
PC2_chapter1_Vertical
PC2_chapter1_Climate

loo_PC2_full <- loo(PC2_chapter1_full,moment_match = TRUE)
loo_PC2_full_minusTotVol <- loo(PC2_chapter1_full_minusTotVol, moment_match = TRUE)
loo_PC2_structure <- loo(PC2_chapter1_structure, moment_match = TRUE)
loo_PC2_configuration <- loo(PC2_chapter1_configuration, moment_match = TRUE)
loo_PC2_configuration_b <- loo(PC2_chapter1_configuration_b, moment_match = TRUE)
loo_PC2_composition <- loo(PC2_chapter1_composition, moment_match = TRUE)
loo_PC2_composition_b <- loo(PC2_chapter1_composition_b, moment_match = TRUE)
loo_PC2_horizontal <- loo(PC2_chapter1_horizontal, moment_match = TRUE)
loo_PC2_horizontal_b <- loo(PC2_chapter1_horizontal_b, moment_match = TRUE)
loo_PC2_amount <- loo(PC2_chapter1_amount, moment_match = TRUE)
loo_PC2_Vertical <- loo(PC2_chapter1_Vertical, moment_match = TRUE)
loo_PC2_Climate <- loo(PC2_chapter1_Climate, moment_match = TRUE)

# Loo Compare
PC2_loo_compare <- loo_compare(loo_PC2_full, loo_PC2_full_minusTotVol, loo_PC2_structure, loo_PC2_configuration, loo_PC2_configuration_b, loo_PC2_composition, loo_PC2_composition_b, loo_PC2_horizontal, loo_PC2_horizontal_b, loo_PC2_amount, loo_PC2_Vertical, loo_PC2_Climate)

PC2_loo_compare
# Output
#                               elpd_diff se_diff
# PC2_chapter1_composition_b      0.0       0.0  
# PC2_chapter1_composition       -0.4       1.3  
# PC2_chapter1_horizontal_b      -3.6       1.8  
# PC2_chapter1_full_minusTotVol  -3.6       3.0  
# PC2_chapter1_full              -4.0       2.9  
# PC2_chapter1_horizontal        -4.2       2.6  
# PC2_chapter1_amount            -4.4       4.6  
# PC2_chapter1_structure         -4.8       3.9  
# PC2_chapter1_Vertical          -5.1       4.6  
# PC2_chapter1_configuration_b  -14.9       8.3  
# PC2_chapter1_Climate          -15.3       8.9  
# PC2_chapter1_configuration    -16.7       8.9  

# PC3 (Functional Axis) -------------------------------------------------------
# PC3_1) PC3: Full Model------------------------------------------------------------
PC3_chapter1_full <- brm( PC3~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="FullModal_PC3",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Full,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')

# Check output
PC3_chapter1_full
describe_posterior(PC3_chapter1_full, rope_ci = 1)
pp_check(PC3_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# PC3_1.5) PC3: Full Model minus total amount ----------------------------------
PC3_chapter1_full_minusTotVol <- brm( PC3~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Horiz Composition
                                        Understory.s +
                                        Midstory.s +
                                        SubCanopy.s +
                                        Canopy.s +
                                        # Total Composition
                                        # Total_veg_amt.s +
                                        
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        #temp.ave_daymet.s +
                                        # precip.total_daymet.s +
                                        # Random effects
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="FullModal_PC3_minusTotVol",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Full_minusTotVol,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC3_chapter1_full_minusTotVol
describe_posterior(PC3_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(PC3_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_full_minusTotVol, ci=0.95, range = c(-0.001, 0.001)))

# PC3_2) PC3: Structure Only Model------------------------------------------------------------
PC3_chapter1_structure <- brm( PC3~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_PC3",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC3_chapter1_structure
describe_posterior(PC3_chapter1_structure, rope_ci = 1)
pp_check(PC3_chapter2_structure, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter2_structure, ci=0.95, range = c(-0.01, 0.01)))

# PC3_3) PC2: Configuration Only Model------------------------------------------------------------
PC3_chapter1_configuration  <- brm( PC3~  
                                      # Vert Configuration
                                      Variance.s + 
                                      # Horiz Configuration
                                      np_und.s +
                                      np_mid.s + 
                                      np_subcan.s +
                                      np_can.s +
                                      # Total Configuration
                                      np_TotVol.s +
                                      
                                      (1|siteID.factor),
                                    
                                    data = test,    
                                    family = 'gaussian',
                                    file="Configuration_PC3",
                                    iter= 4000,
                                    cores = 4,
                                    prior=prior_Configuration,
                                    control = list(adapt_delta = 0.98,
                                                   max_treedepth = 20),
                                    save_pars = save_pars(all = TRUE),
                                    file_refit = 'always')

# Check output
PC3_chapter1_configuration
describe_posterior(PC3_chapter1_configuration, rope_ci = 1)
pp_check(PC3_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# PC3_3_b) PC3: Configuration Only Model (with climate)------------------------------------------------------------
PC3_chapter1_configuration_b  <- brm( PC3~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="Configuration_PC3_b",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Configuration_b,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC3_chapter1_configuration_b
describe_posterior(PC3_chapter1_configuration_b, rope_ci = 1)
pp_check(PC3_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# PC3_4) PC3: Composition Only Model------------------------------------------------------------
PC3_chapter1_composition   <- brm( PC3~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Composition_PC3",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC3_chapter1_composition
describe_posterior(PC3_chapter1_composition, rope_ci = 1)
pp_check(PC3_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# PC3_4_b) PC3: Composition Only Model (with climate) --------------------------------------------------------
PC3_chapter1_composition_b   <- brm( PC3~  
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Composition_PC3_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Composition_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC3_chapter1_composition_b
describe_posterior(PC3_chapter1_composition_b, rope_ci = 1)
pp_check(PC3_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# PC3_5) PC3: Horizontal Only Model------------------------------------------------------------
PC3_chapter1_horizontal    <- brm( PC3~  
                                     np_und.s +
                                     np_mid.s +
                                     np_subcan.s + 
                                     np_can.s +
                                     
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Horizontal_PC3",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Horizontal,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC3_chapter1_horizontal
describe_posterior(PC3_chapter1_horizontal, rope_ci = 1)
pp_check(PC3_chapter2_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter2_horizontal, ci=0.95, range = c(-0.01, 0.01)))

# PC3_5_b) PC3: Horizontal Only Model (with climate) ------------------------------------------------------------
PC3_chapter1_horizontal_b    <- brm( PC3~  
                                       np_und.s +
                                       np_mid.s +
                                       np_subcan.s + 
                                       np_can.s +
                                       
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Horizontal_PC3_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Horizontal_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC3_chapter1_horizontal_b
describe_posterior(PC3_chapter1_horizontal_b, rope_ci = 1)
pp_check(PC3_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# PC3_6) PC3: Amount Only Model  ------------------------------------------------------------
PC3_chapter1_amount    <- brm( PC3~  
                                 Total_veg_amt.s +
                                 
                                 # Climate
                                 decimalLatitude.s +
                                 elevation.s +
                                 temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Amount_PC3",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Amount,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC3_chapter1_amount
describe_posterior(PC3_chapter1_amount, rope_ci = 1)
pp_check(PC3_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_amount, ci=0.95, range = c(-0.01, 0.01)))

# PC3_7) PC3: Vertical Structure Model  ------------------------------------------------------------
PC3_chapter1_Vertical    <- brm( PC3~  
                                   # Vert Configuration
                                   Variance.s + 
                                   
                                   # Total amount 
                                   Total_veg_amt.s +
                                   
                                   # Climate
                                   decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                   
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Vertical_PC3",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Vertical,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
PC3_chapter1_Vertical
describe_posterior(PC3_chapter1_Vertical, rope_ci = 1)
pp_check(PC3_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_Vertical, ci=0.95, range = c(-0.01, 0.01)))

# PC3_8) PC3: Climate Only Model  ------------------------------------------------------------
PC3_chapter1_Climate    <- brm( PC3~  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Climate_PC3",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Climate,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')

# Check output
PC3_chapter1_Climate
describe_posterior(PC3_chapter1_Climate, rope_ci = 1)
pp_check(PC3_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(PC3_chapter1_Climate, ci=0.95, range = c(-0.01, 0.01)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

PC3_chapter1_full
PC3_chapter1_full_minusTotVol
PC3_chapter1_structure
PC3_chapter1_configuration
PC3_chapter1_configuration_b
PC3_chapter1_composition
PC3_chapter1_composition_b
PC3_chapter1_horizontal
PC3_chapter1_horizontal_b
PC3_chapter1_amount
PC3_chapter1_Vertical
PC3_chapter1_Climate

loo_PC3_full <- loo(PC3_chapter1_full,moment_match = TRUE)
loo_PC3_full_minusTotVol <- loo(PC3_chapter1_full_minusTotVol, moment_match = TRUE)
loo_PC3_structure <- loo(PC3_chapter1_structure, moment_match = TRUE)
loo_PC3_configuration <- loo(PC3_chapter1_configuration, moment_match = TRUE)
loo_PC3_configuration_b <- loo(PC3_chapter1_configuration_b, moment_match = TRUE)
loo_PC3_composition <- loo(PC3_chapter1_composition, moment_match = TRUE)
loo_PC3_composition_b <- loo(PC3_chapter1_composition_b, moment_match = TRUE)
loo_PC3_horizontal <- loo(PC3_chapter1_horizontal, moment_match = TRUE)
loo_PC3_horizontal_b <- loo(PC3_chapter1_horizontal_b, moment_match = TRUE)
loo_PC3_amount <- loo(PC3_chapter1_amount, moment_match = TRUE)
loo_PC3_Vertical <- loo(PC3_chapter1_Vertical, moment_match = TRUE)
loo_PC3_Climate <- loo(PC3_chapter1_Climate, moment_match = TRUE)

# Loo Compare
PC3_loo_compare <- loo_compare(loo_PC3_full, loo_PC3_full_minusTotVol, loo_PC3_structure, loo_PC3_configuration, loo_PC3_configuration_b, loo_PC3_composition, loo_PC3_composition_b, loo_PC3_horizontal, loo_PC3_horizontal_b, loo_PC3_amount, loo_PC3_Vertical, loo_PC3_Climate)

PC3_loo_compare

# PC4 (Functional Axis) -------------------------------------------------------
# PC4_1) PC4: Full Model------------------------------------------------------------
PC4_chapter1_full <- brm( PC4~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="FullModal_PC4",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Full,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')

# Check output
PC4_chapter1_full
describe_posterior(PC4_chapter1_full, rope_ci = 1)
pp_check(PC4_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# PC4_1.5) PC4: Full Model minus total amount ----------------------------------
PC4_chapter1_full_minusTotVol <- brm( PC4~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Horiz Composition
                                        Understory.s +
                                        Midstory.s +
                                        SubCanopy.s +
                                        Canopy.s +
                                        # Total Composition
                                        # Total_veg_amt.s +
                                        
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        #temp.ave_daymet.s +
                                        # precip.total_daymet.s +
                                        # Random effects
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="FullModal_PC4_minusTotVol",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Full_minusTotVol,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC4_chapter1_full_minusTotVol
describe_posterior(PC4_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(PC4_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_full_minusTotVol, ci=0.95, range = c(-0.001, 0.001)))

# PC4_2) PC4: Structure Only Model------------------------------------------------------------
PC4_chapter1_structure <- brm( PC4~  
                                 # Vert Configuration
                                 Variance.s + 
                                 # Horiz Configuration
                                 np_und.s +
                                 np_mid.s + 
                                 np_subcan.s +
                                 np_can.s +
                                 # Total Configuration
                                 np_TotVol.s +
                                 # Horiz Composition
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total Composition
                                 # Total_veg_amt.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_PC4",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC4_chapter1_structure
describe_posterior(PC4_chapter1_structure, rope_ci = 1)
pp_check(PC4_chapter2_structure, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter2_structure, ci=0.95, range = c(-0.01, 0.01)))

# PC4_3) PC4: Configuration Only Model------------------------------------------------------------
PC4_chapter1_configuration  <- brm( PC4~  
                                      # Vert Configuration
                                      Variance.s + 
                                      # Horiz Configuration
                                      np_und.s +
                                      np_mid.s + 
                                      np_subcan.s +
                                      np_can.s +
                                      # Total Configuration
                                      np_TotVol.s +
                                      
                                      (1|siteID.factor),
                                    
                                    data = test,    
                                    family = 'gaussian',
                                    file="Configuration_PC4",
                                    iter= 4000,
                                    cores = 4,
                                    prior=prior_Configuration,
                                    control = list(adapt_delta = 0.98,
                                                   max_treedepth = 20),
                                    save_pars = save_pars(all = TRUE),
                                    file_refit = 'always')

# Check output
PC4_chapter1_configuration
describe_posterior(PC4_chapter1_configuration, rope_ci = 1)
pp_check(PC4_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# PC4_3_b) PC4: Configuration Only Model (with climate)------------------------------------------------------------
PC4_chapter1_configuration_b  <- brm( PC4~  
                                        # Vert Configuration
                                        Variance.s + 
                                        # Horiz Configuration
                                        np_und.s +
                                        np_mid.s + 
                                        np_subcan.s +
                                        np_can.s +
                                        # Total Configuration
                                        np_TotVol.s +
                                        # Climate
                                        decimalLatitude.s +
                                        elevation.s +
                                        temp.range_daymet.s +
                                        
                                        (1|siteID.factor),
                                      
                                      data = test,    
                                      family = 'gaussian',
                                      file="Configuration_PC4_b",
                                      iter= 4000,
                                      cores = 4,
                                      prior=prior_Configuration_b,
                                      control = list(adapt_delta = 0.98,
                                                     max_treedepth = 20),
                                      save_pars = save_pars(all = TRUE),
                                      file_refit = 'always')

# Check output
PC4_chapter1_configuration_b
describe_posterior(PC4_chapter1_configuration_b, rope_ci = 1)
pp_check(PC4_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# PC4_4) PC4: Composition Only Model------------------------------------------------------------
PC4_chapter1_composition   <- brm( PC4~  
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Composition_PC4",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Composition,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC4_chapter1_composition
describe_posterior(PC4_chapter1_composition, rope_ci = 1)
pp_check(PC4_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# PC4_4_b) PC4: Composition Only Model (with climate) --------------------------------------------------------
PC4_chapter1_composition_b   <- brm( PC4~  
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Composition_PC4_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Composition_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC4_chapter1_composition_b
describe_posterior(PC4_chapter1_composition_b, rope_ci = 1)
pp_check(PC4_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# PC4_5) PC4: Horizontal Only Model------------------------------------------------------------
PC4_chapter1_horizontal    <- brm( PC4~  
                                     np_und.s +
                                     np_mid.s +
                                     np_subcan.s + 
                                     np_can.s +
                                     
                                     Understory.s +
                                     Midstory.s +
                                     SubCanopy.s +
                                     Canopy.s +
                                     # Total_veg_amt.s
                                     
                                     (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Horizontal_PC4",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Horizontal,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PC4_chapter1_horizontal
describe_posterior(PC4_chapter1_horizontal, rope_ci = 1)
pp_check(PC4_chapter2_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter2_horizontal, ci=0.95, range = c(-0.01, 0.01)))

# PC4_5_b) PC4: Horizontal Only Model (with climate) ------------------------------------------------------------
PC4_chapter1_horizontal_b    <- brm( PC4~  
                                       np_und.s +
                                       np_mid.s +
                                       np_subcan.s + 
                                       np_can.s +
                                       
                                       Understory.s +
                                       Midstory.s +
                                       SubCanopy.s +
                                       Canopy.s +
                                       # Total_veg_amt.s
                                       # Climate
                                       decimalLatitude.s +
                                       elevation.s +
                                       temp.range_daymet.s +
                                       
                                       (1|siteID.factor),
                                     
                                     data = test,    
                                     family = 'gaussian',
                                     file="Horizontal_PC4_b",
                                     iter= 4000,
                                     cores = 4,
                                     prior=prior_Horizontal_b,
                                     control = list(adapt_delta = 0.98,
                                                    max_treedepth = 20),
                                     save_pars = save_pars(all = TRUE),
                                     file_refit = 'always')

# Check output
PC4_chapter1_horizontal_b
describe_posterior(PC4_chapter1_horizontal_b, rope_ci = 1)
pp_check(PC4_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# PC4_6) PC4: Amount Only Model  ------------------------------------------------------------
PC4_chapter1_amount    <- brm( PC4~  
                                 Total_veg_amt.s +
                                 
                                 # Climate
                                 decimalLatitude.s +
                                 elevation.s +
                                 temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Amount_PC4",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Amount,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')

# Check output
PC4_chapter1_amount
describe_posterior(PC4_chapter1_amount, rope_ci = 1)
pp_check(PC4_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_amount, ci=0.95, range = c(-0.01, 0.01)))

# PC4_7) PC4: Vertical Structure Model  ------------------------------------------------------------
PC4_chapter1_Vertical    <- brm( PC4~  
                                   # Vert Configuration
                                   Variance.s + 
                                   
                                   # Total amount 
                                   Total_veg_amt.s +
                                   
                                   # Climate
                                   decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                   
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Vertical_PC4",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Vertical,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
PC4_chapter1_Vertical
describe_posterior(PC4_chapter1_Vertical, rope_ci = 1)
pp_check(PC4_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_Vertical, ci=0.95, range = c(-0.01, 0.01)))

# PC4_8) PC4: Climate Only Model  ------------------------------------------------------------
PC4_chapter1_Climate    <- brm( PC4~  
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Climate_PC4",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Climate,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')

# Check output
PC4_chapter1_Climate
describe_posterior(PC4_chapter1_Climate, rope_ci = 1)
pp_check(PC4_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(PC4_chapter1_Climate, ci=0.95, range = c(-0.01, 0.01)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

PC4_chapter1_full
PC4_chapter1_full_minusTotVol
PC4_chapter1_structure
PC4_chapter1_configuration
PC4_chapter1_configuration_b
PC4_chapter1_composition
PC4_chapter1_composition_b
PC4_chapter1_horizontal
PC4_chapter1_horizontal_b
PC4_chapter1_amount
PC4_chapter1_Vertical
PC4_chapter1_Climate

loo_PC4_full <- loo(PC4_chapter1_full,moment_match = TRUE)
loo_PC4_full_minusTotVol <- loo(PC4_chapter1_full_minusTotVol, moment_match = TRUE)
loo_PC4_structure <- loo(PC4_chapter1_structure, moment_match = TRUE)
loo_PC4_configuration <- loo(PC4_chapter1_configuration, moment_match = TRUE)
loo_PC4_configuration_b <- loo(PC4_chapter1_configuration_b, moment_match = TRUE)
loo_PC4_composition <- loo(PC4_chapter1_composition, moment_match = TRUE)
loo_PC4_composition_b <- loo(PC4_chapter1_composition_b, moment_match = TRUE)
loo_PC4_horizontal <- loo(PC4_chapter1_horizontal, moment_match = TRUE)
loo_PC4_horizontal_b <- loo(PC4_chapter1_horizontal_b, moment_match = TRUE)
loo_PC4_amount <- loo(PC4_chapter1_amount, moment_match = TRUE)
loo_PC4_Vertical <- loo(PC4_chapter1_Vertical, moment_match = TRUE)
loo_PC4_Climate <- loo(PC4_chapter1_Climate, moment_match = TRUE)

# Loo Compare
PC4_loo_compare <- loo_compare(loo_PC4_full, loo_PC4_full_minusTotVol, loo_PC4_structure, loo_PC4_configuration, loo_PC4_configuration_b, loo_PC4_composition, loo_PC4_composition_b, loo_PC4_horizontal, loo_PC4_horizontal_b, loo_PC4_amount, loo_PC4_Vertical, loo_PC4_Climate)

PC4_loo_compare

################################################################################
###### Phylogenetic Diversity #############################
# Faith's Pylogentic Diversity (SES)-------------------------------------------------------
# P_PD_1) PD: Full Model------------------------------------------------------------
PD_chapter1_full <- brm( SES.PD~  
                           # Vert Configuration
                           Variance.s + 
                           # Horiz Configuration
                           np_und.s +
                           np_mid.s + 
                           np_subcan.s +
                           np_can.s +
                           # Total Configuration
                           np_TotVol.s +
                           # Horiz Composition
                           Understory.s +
                           Midstory.s +
                           SubCanopy.s +
                           Canopy.s +
                           # Total Composition
                           Total_veg_amt.s +
                           
                           # Climate
                           decimalLatitude.s +
                           elevation.s +
                           temp.range_daymet.s +
                           #temp.ave_daymet.s +
                           # precip.total_daymet.s +
                           # Random effects
                           (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="FullModal_PD",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Full,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')

# Check output
PD_chapter1_full
describe_posterior(PD_chapter1_full, rope_ci = 1)
pp_check(PD_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# P_PD_1.5) PD: Full Model------------------------------------------------------------
PD_chapter1_full_minusTotVol <- brm( SES.PD~  
                           # Vert Configuration
                           Variance.s + 
                           # Horiz Configuration
                           np_und.s +
                           np_mid.s + 
                           np_subcan.s +
                           np_can.s +
                           # Total Configuration
                           np_TotVol.s +
                           # Horiz Composition
                           Understory.s +
                           Midstory.s +
                           SubCanopy.s +
                           Canopy.s +
                           # Total Composition
                           #Total_veg_amt.s +
                           
                           # Climate
                           decimalLatitude.s +
                           elevation.s +
                           temp.range_daymet.s +
                           #temp.ave_daymet.s +
                           # precip.total_daymet.s +
                           # Random effects
                           (1|siteID.factor),
                         
                         data = test,    
                         family = 'gaussian',
                         file="FullModal_PD_minusTotVol",
                         iter= 4000,
                         cores = 4,
                         prior=prior_Full_minusTotVol,
                         control = list(adapt_delta = 0.98,
                                        max_treedepth = 20),
                         save_pars = save_pars(all = TRUE),
                         file_refit = 'always')

# Check output
PD_chapter1_full_minusTotVol
describe_posterior(PD_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(PD_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_full_minusTotVol, ci=0.95, range = c(-0.01, 0.01)))


# P_PD_2) PD: Structure Only Model ------------------------------------------------
PD_chapter1_structure <- brm(SES.PD ~  
                               # Vert Configuration
                               Variance.s + 
                               # Horiz Configuration
                               np_und.s +
                               np_mid.s + 
                               np_subcan.s +
                               np_can.s +
                               # Total Configuration
                               np_TotVol.s +
                               # Horiz Composition
                               Understory.s +
                               Midstory.s +
                               SubCanopy.s +
                               Canopy.s +
                               # Total Composition
                               # Total_veg_amt.s +
                               
                               (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Structure_PD",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Structure,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
PD_chapter1_structure
describe_posterior(PD_chapter1_structure, rope_ci = 1)
pp_check(PD_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_structure, ci=0.95, range = c(-0.01, 0.01)))

# P_PD_3) PD: Configuration Only Model --------------------------------------------
PD_chapter1_configuration <- brm(SES.PD ~  
                                   # Vert Configuration
                                   Variance.s + 
                                   # Horiz Configuration
                                   np_und.s +
                                   np_mid.s + 
                                   np_subcan.s +
                                   np_can.s +
                                   # Total Configuration
                                   np_TotVol.s +
                                   
                                   (1|siteID.factor),
                                   
                                   data = test,    
                                   family = 'gaussian',
                                   file="Configuration_PD",
                                   iter= 4000,
                                   cores = 4,
                                   prior=prior_Configuration,
                                   control = list(adapt_delta = 0.98,
                                                  max_treedepth = 20),
                                   save_pars = save_pars(all = TRUE),
                                   file_refit = 'always')

# Check output
PD_chapter1_configuration
describe_posterior(PD_chapter1_configuration, rope_ci = 1)
pp_check(PD_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_configuration, ci=0.95, range = c(-0.01, 0.01)))

# P_PD_3_b) PD: Configuration Only Model --------------------------------------------
PD_chapter1_configuration_b <- brm(SES.PD ~  
                                   # Vert Configuration
                                   Variance.s + 
                                   # Horiz Configuration
                                   np_und.s +
                                   np_mid.s + 
                                   np_subcan.s +
                                   np_can.s +
                                   # Total Configuration
                                   np_TotVol.s +
                                     # Climate
                                     decimalLatitude.s +
                                     elevation.s +
                                     temp.range_daymet.s +
                                     
                                   (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Configuration_PD_b",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Configuration_b,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
PD_chapter1_configuration_b
describe_posterior(PD_chapter1_configuration_b, rope_ci = 1)
pp_check(PD_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_configuration_b, ci=0.95, range = c(-0.01, 0.01)))

# PD_4) PD: Composition Only Model -----------------------------------------------
PD_chapter1_composition <- brm(SES.PD ~  
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total_veg_amt.s
                                 
                                 (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Composition_PD",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Composition,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')
# Check output
PD_chapter1_composition
describe_posterior(PD_chapter1_composition, rope_ci = 1)
pp_check(PD_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_composition, ci=0.95, range = c(-0.01, 0.01)))

# PD_4_b) PD: Composition Only Model -----------------------------------------------
PD_chapter1_composition_b <- brm(SES.PD ~  
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total_veg_amt.s
                                   # Climate
                                   decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Composition_PD_b",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Composition_b,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
PD_chapter1_composition_b
describe_posterior(PD_chapter1_composition_b, rope_ci = 1)
pp_check(PD_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_composition_b, ci=0.95, range = c(-0.01, 0.01)))

# PD_5) PD: Horizontal Only Model ------------------------------------------------
PD_chapter1_horizontal <- brm(SES.PD ~  
                                np_und.s +
                                np_mid.s +
                                np_subcan.s + 
                                np_can.s +
                                
                                Understory.s +
                                Midstory.s +
                                SubCanopy.s +
                                Canopy.s +
                                # Total_veg_amt.s
                                
                                (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Horizontal_PD",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Horizontal,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
PD_chapter1_horizontal
describe_posterior(PD_chapter1_horizontal, rope_ci = 1)
pp_check(PD_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# PD_5_b) PD: Horizontal Only Model ------------------------------------------------
PD_chapter1_horizontal_b <- brm(SES.PD ~  
                                np_und.s +
                                np_mid.s +
                                np_subcan.s + 
                                np_can.s +
                                
                                Understory.s +
                                Midstory.s +
                                SubCanopy.s +
                                Canopy.s +
                                # Total_veg_amt.s
                                  # Climate
                                  decimalLatitude.s +
                                  elevation.s +
                                  temp.range_daymet.s +
                                  
                                
                                (1|siteID.factor),
                              
                              data = test,    
                              family = 'gaussian',
                              file="Horizontal_PD_b",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Horizontal_b,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
PD_chapter1_horizontal_b
describe_posterior(PD_chapter1_horizontal_b, rope_ci = 1)
pp_check(PD_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# PD_6) PD: Amount Only Model ------------------------------------------------
PD_chapter1_amount <- brm(SES.PD ~  
                            Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            
                            (1|siteID.factor),
                            
                            data = test,    
                            family = 'gaussian',
                            file="Amount_PD",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Amount,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
PD_chapter1_amount
describe_posterior(PD_chapter1_amount, rope_ci = 1)
pp_check(PD_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# PD_7) PD: Vertical Structure Model ------------------------------------------------
PD_chapter1_Vertical <- brm(SES.PD ~  
                              # Vert Configuration
                              Variance.s + 
                              
                              # Total amount 
                              Total_veg_amt.s +
                              
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                              (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="Vertical_PD",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Vertical,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')
# Check output
PD_chapter1_Vertical
describe_posterior(PD_chapter1_Vertical, rope_ci = 1)
pp_check(PD_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# PD_8) PD: Climate Only Model ------------------------------------------------
PD_chapter1_Climate <- brm(SES.PD ~  
                              # Climate
                              decimalLatitude.s +
                              elevation.s +
                              temp.range_daymet.s +
                              
                              (1|siteID.factor),

                            data = test,    
                            family = 'gaussian',
                            file="Climate_PD",
                            iter= 4000,
                            cores = 4,
                            prior=prior_Climate,
                            control = list(adapt_delta = 0.98,
                                           max_treedepth = 20),
                            save_pars = save_pars(all = TRUE),
                            file_refit = 'always')
# Check output
PD_chapter1_Climate
describe_posterior(PD_chapter1_Climate, rope_ci = 1)
pp_check(PD_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(PD_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

PD_chapter1_full
PD_chapter1_full_minusTotVol
PD_chapter1_structure
PD_chapter1_configuration
PD_chapter1_configuration_b
PD_chapter1_composition
PD_chapter1_composition_b
PD_chapter1_horizontal
PD_chapter1_horizontal_b
PD_chapter1_amount
PD_chapter1_Vertical
PD_chapter1_Climate

loo_PD_full <- loo(PD_chapter1_full,moment_match = TRUE)
loo_PD_full_minusTotVol <- loo(PD_chapter1_full_minusTotVol, moment_match = TRUE)
loo_PD_structure <- loo(PD_chapter1_structure, moment_match = TRUE)
loo_PD_configuration <- loo(PD_chapter1_configuration, moment_match = TRUE)
loo_PD_configuration_b <- loo(PD_chapter1_configuration_b, moment_match = TRUE)
loo_PD_composition <- loo(PD_chapter1_composition, moment_match = TRUE)
loo_PD_composition_b <- loo(PD_chapter1_composition_b, moment_match = TRUE)
loo_PD_horizontal <- loo(PD_chapter1_horizontal, moment_match = TRUE)
loo_PD_horizontal_b <- loo(PD_chapter1_horizontal_b, moment_match = TRUE)
loo_PD_amount <- loo(PD_chapter1_amount, moment_match = TRUE)
loo_PD_Vertical <- loo(PD_chapter1_Vertical, moment_match = TRUE)
loo_PD_Climate <- loo(PD_chapter1_Climate, moment_match = TRUE)

# Loo Compare
PD_loo_compare <- loo_compare(loo_PD_full, loo_PD_full_minusTotVol, loo_PD_structure, loo_PD_configuration, loo_PD_configuration_b, loo_PD_composition, loo_PD_composition_b, loo_PD_horizontal, loo_PD_horizontal_b, loo_PD_amount, loo_PD_Vertical, loo_PD_Climate)

PD_loo_compare
# Output
#                             elpd_diff se_diff
# PD_chapter1_Climate           0.0       0.0   
# PD_chapter1_amount           -0.9       0.3   
# PD_chapter1_Vertical         -1.4       0.8   
# PD_chapter1_configuration    -2.0       2.1   
# PD_chapter1_composition      -2.2       1.6   
# PD_chapter1_composition_b    -3.1       1.2   
# PD_chapter1_configuration_b  -3.1       1.6   
# PD_chapter1_horizontal       -4.6       2.1   
# PD_chapter1_horizontal_b     -5.4       1.8   
# PD_chapter1_structure        -6.0       2.2   
# PD_chapter1_full_minusTotVol -7.6       1.9   
# PD_chapter1_full             -7.7       2.0  

# Mean Pairwise Distance (SES)-------------------------------------------------------
# MPD_1) MPD: Full Model------------------------------------------------------------
MPD_chapter1_full <- brm( SES.MPD~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                         data = test,    
                         family = 'gaussian',
                         file="FullModal_MPD",
                         iter= 4000,
                         cores = 4,
                         prior=prior_Full,
                         control = list(adapt_delta = 0.98,
                                        max_treedepth = 20),
                         save_pars = save_pars(all = TRUE),
                         file_refit = 'always')

# Check output
MPD_chapter1_full
describe_posterior(MPD_chapter1_full, rope_ci = 1)
pp_check(MPD_chapter1_full, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_full, ci=0.95, range = c(-0.01, 0.01)))

# MPD_1.5) MPD: Full Model------------------------------------------------------------
MPD_chapter1_full_minusTotVol <- brm( SES.MPD~  
                            # Vert Configuration
                            Variance.s + 
                            # Horiz Configuration
                            np_und.s +
                            np_mid.s + 
                            np_subcan.s +
                            np_can.s +
                            # Total Configuration
                            np_TotVol.s +
                            # Horiz Composition
                            Understory.s +
                            Midstory.s +
                            SubCanopy.s +
                            Canopy.s +
                            # Total Composition
                            # Total_veg_amt.s +
                            
                            # Climate
                            decimalLatitude.s +
                            elevation.s +
                            temp.range_daymet.s +
                            #temp.ave_daymet.s +
                            # precip.total_daymet.s +
                            # Random effects
                            (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="FullModal_MPD_minusTotVol",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Full_minusTotVol,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')

# Check output
MPD_chapter1_full_minusTotVol
describe_posterior(MPD_chapter1_full_minusTotVol, rope_ci = 1)
pp_check(MPD_chapter1_full_minusTotVol, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_full_minusTotVol, ci=0.95, range = c(-0.01, 0.01)))

# MPD_2) MPD: Structure Only Model ------------------------------------------------
MPD_chapter1_structure <- brm(SES.MPD ~  
                                # Vert Configuration
                                Variance.s + 
                                # Horiz Configuration
                                np_und.s +
                                np_mid.s + 
                                np_subcan.s +
                                np_can.s +
                                # Total Configuration
                                np_TotVol.s +
                                # Horiz Composition
                                Understory.s +
                                Midstory.s +
                                SubCanopy.s +
                                Canopy.s +
                                # Total Composition
                                # Total_veg_amt.s +
                                
                                (1|siteID.factor),
                             
                             data = test,    
                             family = 'gaussian',
                             file="Structure_MPD",
                             iter= 4000,
                             cores = 4,
                             prior=prior_Structure,
                             control = list(adapt_delta = 0.98,
                                            max_treedepth = 20),
                             save_pars = save_pars(all = TRUE),
                             file_refit = 'always')
# Check output
MPD_chapter1_structure
describe_posterior(MPD_chapter1_structure, rope_ci = 1)
pp_check(MPD_chapter1_structure, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_structure, ci=0.95, range = c(-0.01, 0.01)))

# MPD_3) MPD: Configuration Only Model --------------------------------------------
MPD_chapter1_configuration <- brm(SES.MPD ~  
                                    # Vert Configuration
                                    Variance.s + 
                                    # Horiz Configuration
                                    np_und.s +
                                    np_mid.s + 
                                    np_subcan.s +
                                    np_can.s +
                                    # Total Configuration
                                    np_TotVol.s +
                                    
                                    (1|siteID.factor),
                                 
                                 data = test,    
                                 family = 'gaussian',
                                 file="Configuration_MPD",
                                 iter= 4000,
                                 cores = 4,
                                 prior=prior_Configuration,
                                 control = list(adapt_delta = 0.98,
                                                max_treedepth = 20),
                                 save_pars = save_pars(all = TRUE),
                                 file_refit = 'always')

# Check output
MPD_chapter1_configuration
describe_posterior(MPD_chapter1_configuration, rope_ci = 1)
pp_check(MPD_chapter1_configuration, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_configuration, ci=0.95, range = c(-0.001, 0.001)))

# MPD_3_b) MPD: Configuration Only Model --------------------------------------------
MPD_chapter1_configuration_b <- brm(SES.MPD ~  
                                    # Vert Configuration
                                    Variance.s + 
                                    # Horiz Configuration
                                    np_und.s +
                                    np_mid.s + 
                                    np_subcan.s +
                                    np_can.s +
                                    # Total Configuration
                                    np_TotVol.s +
                                      # Climate
                                      decimalLatitude.s +
                                      elevation.s +
                                      temp.range_daymet.s +
                                    
                                    (1|siteID.factor),
                                  
                                  data = test,    
                                  family = 'gaussian',
                                  file="Configuration_MPD_b",
                                  iter= 4000,
                                  cores = 4,
                                  prior=prior_Configuration_b,
                                  control = list(adapt_delta = 0.98,
                                                 max_treedepth = 20),
                                  save_pars = save_pars(all = TRUE),
                                  file_refit = 'always')

# Check output
MPD_chapter1_configuration_b
describe_posterior(MPD_chapter1_configuration_b, rope_ci = 1)
pp_check(MPD_chapter1_configuration_b, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_configuration_b, ci=0.95, range = c(-0.001, 0.001)))

# MPD_4) MPD: Composition Only Model -----------------------------------------------
MPD_chapter1_composition <- brm(SES.MPD ~  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                  
                                  (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Composition_MPD",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Composition,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
MPD_chapter1_composition
describe_posterior(MPD_chapter1_composition, rope_ci = 1)
pp_check(MPD_chapter1_composition, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_composition, ci=0.95, range = c(-0.001, 0.001)))

# MPD_4_b) MPD: Composition Only Model -----------------------------------------------
MPD_chapter1_composition_b <- brm(SES.MPD ~  
                                  Understory.s +
                                  Midstory.s +
                                  SubCanopy.s +
                                  Canopy.s +
                                  # Total_veg_amt.s
                                    # Climate
                                    decimalLatitude.s +
                                    elevation.s +
                                    temp.range_daymet.s +
                                  
                                  (1|siteID.factor),
                                
                                data = test,    
                                family = 'gaussian',
                                file="Composition_MPD_b",
                                iter= 4000,
                                cores = 4,
                                prior=prior_Composition_b,
                                control = list(adapt_delta = 0.98,
                                               max_treedepth = 20),
                                save_pars = save_pars(all = TRUE),
                                file_refit = 'always')
# Check output
MPD_chapter1_composition_b
describe_posterior(MPD_chapter1_composition_b, rope_ci = 1)
pp_check(MPD_chapter1_composition_b, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_composition_b, ci=0.95, range = c(-0.001, 0.001)))

# MPD_5) MPD: Horizontal Only Model ------------------------------------------------
MPD_chapter1_horizontal <- brm(SES.MPD ~  
                                 np_und.s +
                                 np_mid.s +
                                 np_subcan.s + 
                                 np_can.s +
                                 
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total_veg_amt.s
                                 
                                 (1|siteID.factor),
                              
                              data = test,    
                              family = 'gaussian',
                              file="Horizontal_MPD",
                              iter= 4000,
                              cores = 4,
                              prior=prior_Horizontal,
                              control = list(adapt_delta = 0.98,
                                             max_treedepth = 20),
                              save_pars = save_pars(all = TRUE),
                              file_refit = 'always')
# Check output
MPD_chapter1_horizontal
describe_posterior(MPD_chapter1_horizontal, rope_ci = 1)
pp_check(MPD_chapter1_horizontal, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_horizontal, ci=0.95, range = c(-0.001, 0.001)))

# MPD_5_b) MPD: Horizontal Only Model ------------------------------------------------
MPD_chapter1_horizontal_b <- brm(SES.MPD ~  
                                 np_und.s +
                                 np_mid.s +
                                 np_subcan.s + 
                                 np_can.s +
                                 
                                 Understory.s +
                                 Midstory.s +
                                 SubCanopy.s +
                                 Canopy.s +
                                 # Total_veg_amt.s
                                   # Climate
                                   decimalLatitude.s +
                                   elevation.s +
                                   temp.range_daymet.s +
                                 
                                 (1|siteID.factor),
                               
                               data = test,    
                               family = 'gaussian',
                               file="Horizontal_MPD_b",
                               iter= 4000,
                               cores = 4,
                               prior=prior_Horizontal_b,
                               control = list(adapt_delta = 0.98,
                                              max_treedepth = 20),
                               save_pars = save_pars(all = TRUE),
                               file_refit = 'always')
# Check output
MPD_chapter1_horizontal_b
describe_posterior(MPD_chapter1_horizontal_b, rope_ci = 1)
pp_check(MPD_chapter1_horizontal_b, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_horizontal_b, ci=0.95, range = c(-0.001, 0.001)))

# MPD_6) MPD: Amount Only Model ------------------------------------------------
MPD_chapter1_amount <- brm(SES.MPD ~  
                             Total_veg_amt.s +
                             
                             # Climate
                             decimalLatitude.s +
                             elevation.s +
                             temp.range_daymet.s +
                             
                             (1|siteID.factor),
                          
                          data = test,    
                          family = 'gaussian',
                          file="Amount_MPD",
                          iter= 4000,
                          cores = 4,
                          prior=prior_Amount,
                          control = list(adapt_delta = 0.98,
                                         max_treedepth = 20),
                          save_pars = save_pars(all = TRUE),
                          file_refit = 'always')
# Check output
MPD_chapter1_amount
describe_posterior(MPD_chapter1_amount, rope_ci = 1)
pp_check(MPD_chapter1_amount, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_amount, ci=0.95, range = c(-0.001, 0.001)))

# MPD_7) MPD: Vertical Structure  Model ------------------------------------------------
MPD_chapter1_Vertical <- brm(SES.MPD ~  
                               # Vert Configuration
                               Variance.s + 
                               
                               # Total amount 
                               Total_veg_amt.s +
                               
                               # Climate
                               decimalLatitude.s +
                               elevation.s +
                               temp.range_daymet.s +
                               
                               (1|siteID.factor),
                           
                           data = test,    
                           family = 'gaussian',
                           file="Vertical_MPD",
                           iter= 4000,
                           cores = 4,
                           prior=prior_Vertical,
                           control = list(adapt_delta = 0.98,
                                          max_treedepth = 20),
                           save_pars = save_pars(all = TRUE),
                           file_refit = 'always')
# Check output
MPD_chapter1_Vertical
describe_posterior(MPD_chapter1_Vertical, rope_ci = 1)
pp_check(MPD_chapter1_Vertical, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_Vertical, ci=0.95, range = c(-0.001, 0.001)))

# MPD_8) MPD: Climate Only Model ------------------------------------------------
MPD_chapter1_Climate <- brm(SES.MPD ~  
                               # Climate
                               decimalLatitude.s +
                               elevation.s +
                               temp.range_daymet.s +
                               
                               (1|siteID.factor),
                             
                             data = test,    
                             family = 'gaussian',
                             file="Climate_MPD",
                             iter= 4000,
                             cores = 4,
                             prior=prior_Climate,
                             control = list(adapt_delta = 0.98,
                                            max_treedepth = 20),
                             save_pars = save_pars(all = TRUE),
                             file_refit = 'always')
# Check output
MPD_chapter1_Climate
describe_posterior(MPD_chapter1_Climate, rope_ci = 1)
pp_check(MPD_chapter1_Climate, type = 'stat_2d', ndraws = 500)
plot(rope(MPD_chapter1_Climate, ci=0.95, range = c(-0.001, 0.001)))

################################################################################
# Model comparison: Leave one out (loo compare) --------------------------------
library(loo)

MPD_chapter1_full
MPD_chapter1_full_minusTotVol
MPD_chapter1_structure
MPD_chapter1_configuration
MPD_chapter1_configuration_b
MPD_chapter1_composition
MPD_chapter1_composition_b
MPD_chapter1_horizontal
MPD_chapter1_horizontal_b
MPD_chapter1_amount
MPD_chapter1_Vertical
MPD_chapter1_Climate

loo_MPD_full <- loo(MPD_chapter1_full,moment_match = TRUE)
loo_MPD_full_minusTotVol <- loo(MPD_chapter1_full_minusTotVol, moment_match = TRUE)
loo_MPD_structure <- loo(MPD_chapter1_structure, moment_match = TRUE)
loo_MPD_configuration <- loo(MPD_chapter1_configuration, moment_match = TRUE)
loo_MPD_configuration_b <- loo(MPD_chapter1_configuration_b, moment_match = TRUE)
loo_MPD_composition <- loo(MPD_chapter1_composition, moment_match = TRUE)
loo_MPD_composition_b <- loo(MPD_chapter1_composition_b, moment_match = TRUE)
loo_MPD_horizontal <- loo(MPD_chapter1_horizontal, moment_match = TRUE)
loo_MPD_horizontal_b <- loo(MPD_chapter1_horizontal_b, moment_match = TRUE)
loo_MPD_amount <- loo(MPD_chapter1_amount, moment_match = TRUE)
loo_MPD_Vertical <- loo(MPD_chapter1_Vertical, moment_match = TRUE)
loo_MPD_Climate <- loo(MPD_chapter1_Climate, moment_match = TRUE)

# Loo Compare
MPD_loo_compare <- loo_compare(loo_MPD_full, loo_MPD_full_minusTotVol, loo_MPD_structure, loo_MPD_configuration, loo_MPD_configuration_b, loo_MPD_composition, loo_MPD_composition_b, loo_MPD_horizontal, loo_MPD_horizontal_b, loo_MPD_amount, loo_MPD_Vertical, loo_MPD_Climate)

MPD_loo_compare
# Output
#                             elpd_diff se_diff
# MPD_chapter1_Climate           0.0       0.0   
# MPD_chapter1_amount           -0.7       0.3   
# MPD_chapter1_Vertical         -1.4       0.6   
# MPD_chapter1_composition      -2.4       1.7   
# MPD_chapter1_configuration    -2.6       2.5   
# MPD_chapter1_horizontal       -2.9       3.1   
# MPD_chapter1_composition_b    -3.1       1.1   
# MPD_chapter1_configuration_b  -4.0       2.1   
# MPD_chapter1_horizontal_b     -4.1       3.0   
# MPD_chapter1_structure        -4.5       3.3   
# MPD_chapter1_full             -5.6       3.1   
# MPD_chapter1_full_minusTotVol -5.8       3.1  

################################################################################
################################################################################
################################################################################
pp_check(Chicago_FDiv_test2, type = 'stat_2d', ndraws = 500)
plot(rope(Chicago_FDiv_test2, ci=0.95, range = c(-0.001, 0.001)))

rope_test <- rope(Chicago_FDiv_test2, ci=0.95, range = c(-0.001, 0.001))

plot(rope_test, geom.colors = c("cadetblue", "coral"))
?rope
###


describe_posterior(Chicago_FRich_test2)
pp_check(Chicago_FRich_test2, type = 'stat_2d', ndraws = 500)
plot(rope(Chicago_FRich_test2, ci=0.95, range = c(-0.001, 0.001)))


###############################################################################
# Plotting --------------------------------------------------------------------
library(viridis)

#################### -----------------------------------------------------------
# Plot the parameter values ----------------------------------------------------
# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(spRich_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(spRich_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(spRich_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(spRich_chapter1_structure )),
    nrow(mcmc_intervals_data(spRich_chapter1_configuration )),
    nrow(mcmc_intervals_data(spRich_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(spRich_chapter1_composition )),
    nrow(mcmc_intervals_data(spRich_chapter1_composition_b )),
    nrow(mcmc_intervals_data(spRich_chapter1_horizontal)),
    nrow(mcmc_intervals_data(spRich_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(spRich_chapter1_amount)) ,
    nrow(mcmc_intervals_data(spRich_chapter1_Vertical)),
    nrow(mcmc_intervals_data(spRich_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
colors_test <- c("#332288", "#117733", "#44AA99", "#44AA99", "#88CCEE", "#DDCC77", "#DDCC77", "#CC6677", "#008000", "#AA4499", "#882255")
colors_test <- colors_test[as.numeric(factor(combined_test$model))]
#1, 2, 3, 3, 4,4,5,6,6,7,8

Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             #color=colors_test, 
             color="black",
             size=1.75) +
  # Shapes: square(0,15), circle (1,16), triangle(2, 17), 
  scale_shape_manual(values = c(  5, # Amount 
                                 18, # Climate
                                  2, # Composition
                                 17, # Composition b
                                  0, # Configuration
                                 15, # Configuration b
                                  9, # Full_minus TotVol
                                  1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic()+
  ggtitle("Taxonomic Diversity: Species Richness") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

#
Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
    )
  ) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
        )
  
ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/SpRich.png",
  plot = last_plot(),
  width = 6, height = 6
)

###############################################
library(sjPlot)

plot_models(
  spRich_chapter1_full, spRich_chapter1_full_minusTotVol, spRich_chapter1_structure, spRich_chapter1_configuration, spRich_chapter1_configuration_b, spRich_chapter1_composition, spRich_chapter1_composition_b, spRich_chapter1_horizontal, spRich_chapter1_horizontal_b, spRich_chapter1_amount, spRich_chapter1_Vertical, spRich_chapter1_Climate,
  # axis.labels = c(
  #   "Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"
  # ),
  m.labels = c("Model: Full", "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  show.values = FALSE, show.p = FALSE, p.shape = FALSE
)


########################

# Functional Diversity
################################################################################
# FEven --------------
# Figure <- mcmc_intervals(as.array(FEve_chapter1_Climate), 
#                          pars=c("b_decimalLatitude.s",
#                                 "b_elevation.s",
#                                 "b_temp.range_daymet.s"),
#                          prob = 0.8, prob_outer = 0.95)+
#   ggplot2::theme_classic()
# 
# 
# Figure + scale_y_discrete(
#   labels = c("b_decimalLatitude.s" = "Latitude",
#              "b_elevation.s" = "Elevation",
#              "b_temp.range_daymet.s" = "Temperature Range"))

mcmc_intervals(as.array(FEve_chapter1_full), 
               pars=c("b_decimalLatitude.s",
                      "b_elevation.s",
                      "b_temp.range_daymet.s",
                      "b_Variance.s",
                      "b_np_und.s",
                      "b_np_mid.s",
                      "b_np_subcan.s",
                      "b_np_can.s",
                      "b_np_TotVol.s",
                      "b_Understory.s",
                      "b_Midstory.s",
                      "b_SubCanopy.s",
                      "b_Canopy.s",
                      "b_Total_veg_amt.s"),
                              prob = 0.8, prob_outer = 0.95)+
       ggplot2::theme_classic()

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FEve_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(FEve_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(FEve_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(FEve_chapter1_structure )),
    nrow(mcmc_intervals_data(FEve_chapter1_configuration )),
    nrow(mcmc_intervals_data(FEve_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(FEve_chapter1_composition )),
    nrow(mcmc_intervals_data(FEve_chapter1_composition_b )),
    nrow(mcmc_intervals_data(FEve_chapter1_horizontal)),
    nrow(mcmc_intervals_data(FEve_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(FEve_chapter1_amount)) ,
    nrow(mcmc_intervals_data(FEve_chapter1_Vertical)),
    nrow(mcmc_intervals_data(FEve_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic()+
  ggtitle("Functional Diversity: Functional Evenness") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

# 
Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/FEven.png",
  plot = last_plot(),
  width = 6, height = 6
)
################################################################################
# FDiv --------------
Figure <- mcmc_intervals(as.array(FDiv_chapter1_composition), 
                         pars=c("b_Understory.s",
                                "b_Midstory.s" ,
                                "b_SubCanopy.s",
                                "b_Canopy.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_Understory.s" = "Understory",
             "b_Midstory.s" = "Midstory" ,
             "b_SubCanopy.s" = "SubCanopy",
             "b_Canopy.s" = "Canopy"))
#
Figure <- mcmc_intervals(as.array(FDiv_chapter1_composition_b), 
                         pars=c("b_Understory.s",
                                "b_Midstory.s" ,
                                "b_SubCanopy.s",
                                "b_Canopy.s",
                                "b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_Understory.s" = "Understory",
             "b_Midstory.s" = "Midstory" ,
             "b_SubCanopy.s" = "SubCanopy",
             "b_Canopy.s" = "Canopy",
             "b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))
# Horizontal Model_b
FDiv_chapter1_horizontal_b$formula
Figure <- mcmc_intervals(as.array(FDiv_chapter1_horizontal_b), 
                         pars=c("b_np_und.s",
                                "b_np_mid.s" ,
                                "b_np_subcan.s",
                                "b_np_can.s",
                                "b_Understory.s",
                                "b_Midstory.s",
                                "b_SubCanopy.s",
                                "b_Canopy.s",
                                "b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_np_und.s"="Understory Fragmentation",
             "b_np_mid.s"="Midstory Fragmentation" ,
             "b_np_subcan.s"="Subcanopy Fragmentation",
             "b_np_can.s"="Canopy Fragmentation",
             "b_Understory.s" = "Understory",
             "b_Midstory.s" = "Midstory" ,
             "b_SubCanopy.s" = "SubCanopy",
             "b_Canopy.s" = "Canopy",
             "b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Vertical
FDiv_chapter1_Vertical
Figure <- mcmc_intervals(as.array(FDiv_chapter1_Vertical), 
                         pars=c("b_Variance.s",
                                "b_Total_veg_amt.s" ,
                                "b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_Variance.s" = "Variance",
             "b_Total_veg_amt.s" = "Total Vegitation Amount" ,
             "b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FDiv_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(FDiv_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(FDiv_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(FDiv_chapter1_structure )),
    nrow(mcmc_intervals_data(FDiv_chapter1_configuration )),
    nrow(mcmc_intervals_data(FDiv_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(FDiv_chapter1_composition )),
    nrow(mcmc_intervals_data(FDiv_chapter1_composition_b )),
    nrow(mcmc_intervals_data(FDiv_chapter1_horizontal)),
    nrow(mcmc_intervals_data(FDiv_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(FDiv_chapter1_amount)) ,
    nrow(mcmc_intervals_data(FDiv_chapter1_Vertical)),
    nrow(mcmc_intervals_data(FDiv_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic()+
  ggtitle("Functional Diversity: Functional Divergence") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

# 
Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/FDiv.png",
  plot = last_plot(),
  width = 6, height = 6
)


################################################################################
# FRich --------------
Figure <- mcmc_intervals(as.array(FRic_chapter1_Vertical), 
                         pars=c("b_Variance.s",          
                                "b_Total_veg_amt.s",     
                                "b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_Variance.s" = "Variance",
             "b_Total_veg_amt.s" = "Total Veg Amount",
             "b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))


# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(FRic_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(FRic_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(FRic_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(FRic_chapter1_structure )),
    nrow(mcmc_intervals_data(FRic_chapter1_configuration )),
    nrow(mcmc_intervals_data(FRic_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(FRic_chapter1_composition )),
    nrow(mcmc_intervals_data(FRic_chapter1_composition_b )),
    nrow(mcmc_intervals_data(FRic_chapter1_horizontal)),
    nrow(mcmc_intervals_data(FRic_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(FRic_chapter1_amount)) ,
    nrow(mcmc_intervals_data(FRic_chapter1_Vertical)),
    nrow(mcmc_intervals_data(FRic_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
                                ) )+

  ggplot2::theme_classic()+
  ggtitle("Functional Diversity: Functional Richness") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/FRich.png",
  plot = last_plot(),
  width = 6, height = 6
)

################################################################################
# PC1 --------------
Figure <- mcmc_intervals(as.array(PC1_chapter1_Climate), 
                         pars=c("b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC1_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(PC1_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(PC1_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(PC1_chapter1_structure )),
    nrow(mcmc_intervals_data(PC1_chapter1_configuration )),
    nrow(mcmc_intervals_data(PC1_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(PC1_chapter1_composition )),
    nrow(mcmc_intervals_data(PC1_chapter1_composition_b )),
    nrow(mcmc_intervals_data(PC1_chapter1_horizontal)),
    nrow(mcmc_intervals_data(PC1_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(PC1_chapter1_amount)) ,
    nrow(mcmc_intervals_data(PC1_chapter1_Vertical)),
    nrow(mcmc_intervals_data(PC1_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic()+
  ggtitle("Functional Diversity: PC1") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1.png",
  plot = last_plot(),
  width = 6, height = 6
)

################################################################################
# PC2 --------------
Figure <- mcmc_intervals(as.array(PC2_chapter1_composition_b), 
                         pars=c("b_Understory.s",
                                "b_Midstory.s" ,
                                "b_SubCanopy.s",
                                "b_Canopy.s",
                                "b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s" ),
                          prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_Understory.s" = "Understory",
             "b_Midstory.s" = "Midstory" ,
             "b_SubCanopy.s" = "SubCanopy",
             "b_Canopy.s" = "Canopy",
             "b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC2_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(PC2_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(PC2_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(PC2_chapter1_structure )),
    nrow(mcmc_intervals_data(PC2_chapter1_configuration )),
    nrow(mcmc_intervals_data(PC2_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(PC2_chapter1_composition )),
    nrow(mcmc_intervals_data(PC2_chapter1_composition_b )),
    nrow(mcmc_intervals_data(PC2_chapter1_horizontal)),
    nrow(mcmc_intervals_data(PC2_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(PC2_chapter1_amount)) ,
    nrow(mcmc_intervals_data(PC2_chapter1_Vertical)),
    nrow(mcmc_intervals_data(PC2_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic() +
  ggtitle("Functional Diversity: PC2") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC2.png",
  plot = last_plot(),
  width = 6, height = 6
)

################################################################################
# PC3 --------------
# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC3_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(PC3_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(PC3_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(PC3_chapter1_structure )),
    nrow(mcmc_intervals_data(PC3_chapter1_configuration )),
    nrow(mcmc_intervals_data(PC3_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(PC3_chapter1_composition )),
    nrow(mcmc_intervals_data(PC3_chapter1_composition_b )),
    nrow(mcmc_intervals_data(PC3_chapter1_horizontal)),
    nrow(mcmc_intervals_data(PC3_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(PC3_chapter1_amount)) ,
    nrow(mcmc_intervals_data(PC3_chapter1_Vertical)),
    nrow(mcmc_intervals_data(PC3_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic() +
  ggtitle("Functional Diversity: PC3") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3.png",
  plot = last_plot(),
  width = 6, height = 6
)

################################################################################
# PC4 --------------
# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PC4_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(PC4_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(PC4_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(PC4_chapter1_structure )),
    nrow(mcmc_intervals_data(PC4_chapter1_configuration )),
    nrow(mcmc_intervals_data(PC4_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(PC4_chapter1_composition )),
    nrow(mcmc_intervals_data(PC4_chapter1_composition_b )),
    nrow(mcmc_intervals_data(PC4_chapter1_horizontal)),
    nrow(mcmc_intervals_data(PC4_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(PC4_chapter1_amount)) ,
    nrow(mcmc_intervals_data(PC4_chapter1_Vertical)),
    nrow(mcmc_intervals_data(PC4_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic() +
  ggtitle("Functional Diversity: PC4") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC4.png",
  plot = last_plot(),
  width = 6, height = 6
)

# Phylogenetic Diversity 
################################################################################
# PD --------------
Figure <- mcmc_intervals(as.array(PD_chapter1_Climate), 
                         pars=c("b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(PD_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(PD_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(PD_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(PD_chapter1_structure )),
    nrow(mcmc_intervals_data(PD_chapter1_configuration )),
    nrow(mcmc_intervals_data(PD_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(PD_chapter1_composition )),
    nrow(mcmc_intervals_data(PD_chapter1_composition_b )),
    nrow(mcmc_intervals_data(PD_chapter1_horizontal)),
    nrow(mcmc_intervals_data(PD_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(PD_chapter1_amount)) ,
    nrow(mcmc_intervals_data(PD_chapter1_Vertical)),
    nrow(mcmc_intervals_data(PD_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic()+
  ggtitle("Phylogentic Diversity: Faith's PD") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PD.png",
  plot = last_plot(),
  width = 6, height = 6
)

################################################################################
# MPD --------------
Figure <- mcmc_intervals(as.array(MPD_chapter1_Climate), 
                         pars=c("b_decimalLatitude.s",
                                "b_elevation.s",
                                "b_temp.range_daymet.s"),
                         prob = 0.8, prob_outer = 0.95)+
  ggplot2::theme_classic()

Figure + scale_y_discrete(
  labels = c("b_decimalLatitude.s" = "Latitude",
             "b_elevation.s" = "Elevation",
             "b_temp.range_daymet.s" = "Temperature Range"))

# Combined plots
combined <- rbind(#mcmc_intervals_data(FRic_chapter1_full,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_full_minusTotVol,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_structure,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_configuration,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_configuration_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_composition,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_composition_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_horizontal,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_horizontal_b,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_amount,prob=0.8,prob_outer = 0.95),
  mcmc_intervals_data(MPD_chapter1_Vertical,prob=0.8,prob_outer = 0.95), 
  mcmc_intervals_data(MPD_chapter1_Climate,prob=0.8,prob_outer = 0.95)
)

combined$model <- rep(c(#"Model: Full", 
  "Model: Full_minusTotVol", "Model: Structure", "Model: Configuration", "Model: Configuration_b", "Model: Composition", "Model: Composition_b", "Model: Horizontal", "Model: Horizontal_b", "Model: Amount", "Model: Vertical", "Model: Climate"), 
  c(
    #nrow(mcmc_intervals_data(FRic_chapter1_full)),
    nrow(mcmc_intervals_data(MPD_chapter1_full_minusTotVol)),
    nrow(mcmc_intervals_data(MPD_chapter1_structure )),
    nrow(mcmc_intervals_data(MPD_chapter1_configuration )),
    nrow(mcmc_intervals_data(MPD_chapter1_configuration_b)),
    nrow(mcmc_intervals_data(MPD_chapter1_composition )),
    nrow(mcmc_intervals_data(MPD_chapter1_composition_b )),
    nrow(mcmc_intervals_data(MPD_chapter1_horizontal)),
    nrow(mcmc_intervals_data(MPD_chapter1_horizontal_b)),
    nrow(mcmc_intervals_data(MPD_chapter1_amount)) ,
    nrow(mcmc_intervals_data(MPD_chapter1_Vertical)),
    nrow(mcmc_intervals_data(MPD_chapter1_Climate))
  ) 
)

combined_test <- combined%>%filter(
  combined$parameter=="b_Variance.s"|
    combined$parameter=="b_np_und.s"|
    combined$parameter=="b_np_mid.s"|
    combined$parameter=="b_np_subcan.s"|
    combined$parameter=="b_np_can.s"|
    combined$parameter=="b_np_TotVol.s"|
    combined$parameter=="b_Understory.s"|
    combined$parameter=="b_Midstory.s"|
    combined$parameter=="b_SubCanopy.s"|
    combined$parameter=="b_Canopy.s"|
    combined$parameter=="b_Total_veg_amt.s"|
    combined$parameter=="b_decimalLatitude.s"|
    combined$parameter=="b_elevation.s"|
    combined$parameter=="b_temp.range_daymet.s"
)

pos <- position_nudge(y = ifelse(combined_test$model == "Model: Full", 0, 
                                 ifelse(combined_test$model == "Model: Full_minusTotVol", 0.075,
                                        ifelse(combined_test$model == "Model: Structure", 0.15,
                                               ifelse(combined_test$model == "Model: Configuration", 0.225,    
                                                      ifelse(combined_test$model == "Model: Configuration_b", 0.3,
                                                             ifelse(combined_test$model == "Model: Composition", 0.375,
                                                                    ifelse(combined_test$model == "Model: Composition_b", 0.45,
                                                                           ifelse(combined_test$model == "Model: Horizontal", 0.525,
                                                                                  ifelse(combined_test$model == "Model: Horizontal_b", 0.6,
                                                                                         ifelse(combined_test$model == "Model: Amount", 0.675,
                                                                                                ifelse(combined_test$model == "Model: Vertical", 0.75,
                                                                                                       ifelse(combined_test$model == "Model: Climate", 0.825,
                                                                                                              1.2))))))))))))
)

############## Plots -------------
Figure <- ggplot(combined_test, 
                 aes(x = m, y = parameter, color = ifelse(ll < 0 & hh < 0, '#CC6666',
                                                          ifelse(ll > 0 & hh > 0, '#66CC99', 'grey'
                                                          )   ) )) +scale_color_identity()+
  
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+ # adds internal confidence
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+ # adds outer confidence 
  #geom_point(position = pos, color="black", shape=combined_test$parameter) +
  
  geom_point(data=combined_test, 
             aes(shape=model), 
             position = pos,
             color="black", size=2) +
  scale_shape_manual(values = c( 5, # Amount 
                                 18, # Climate
                                 2, # Composition
                                 17, # Composition b
                                 0, # Configuration
                                 15, # Configuration b
                                 9, # Full_minus TotVol
                                 1, # Horizontal
                                 16,  # Horizontal b
                                 14, # Structure
                                 25   # Vertical
  ) )+
  
  ggplot2::theme_classic() +
  ggtitle("Phylogentic Diversity: MPD") +
  xlab("Parameter Estimate") + ylab("Parameter Name")

Figure + scale_y_discrete(
  limits = c(
    "b_Variance.s",
    "b_np_und.s",
    "b_np_mid.s",
    "b_np_subcan.s",
    "b_np_can.s",
    "b_np_TotVol.s",
    "b_Understory.s",
    "b_Midstory.s",
    "b_SubCanopy.s",
    "b_Canopy.s", 
    "b_Total_veg_amt.s",
    "b_decimalLatitude.s",
    "b_elevation.s",
    "b_temp.range_daymet.s"),
  labels=c( 
    "Vertical Heterogenity",
    "Understory Fragmentation",
    "Midstory Fragmentation",
    "SubCanopy Fragmentation",
    "Canopy Fragmentation",
    "Total Horizontal Fragmentation",
    "Understory Amount",
    "Midstory Amount",
    "SubCanopy Amount",
    "Canopy Amount", 
    "Total Vegitation Amount" ,
    "Latitude", 
    "Elevation",
    "Temperature Range"
  )
) +
  geom_vline(xintercept = 0)+
  theme(panel.grid.major.y = element_line(color = "grey23",
                                          size = 0.5,
                                          linetype = 3),
        axis.text = element_text(face=1.5,vjust=-2.5)
  )

ggsave(
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/MPD.png",
  plot = last_plot(),
  width = 6, height = 6
)

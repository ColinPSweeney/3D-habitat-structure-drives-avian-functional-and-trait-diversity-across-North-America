# 
# Final LiDAR Metrics calculations
# Understory, Midstory, Subcanopy, Canopy, Vertical configuration, Horizontal 

# Load R Packages---------------------------------------------------------------
library(lidR)
library(raster)
library(rgdal)
library(landscapemetrics)
library(tidyverse)
library(terra)
library(sf)

# Load in Data from separate R file---------------------------------------------
# 3DFragmentationMetrics_DataLoad_chapter1.R
list_laz # List of all laz file names
list_plots # List of all plots
directory # List of wd file paths

# OR
df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_datarepair.csv")
# df <- df%>%drop_na(te_und) # Data cleaning. Remove NA data. 
# df <- df%>%drop_na(Understory) # Data cleaning. Remove NA data. 

df <- df%>%filter(rangefilt_95_SpRich>=5) # Remove site with less than 5 species (functional metrics)
df$plotID
length(unique(df$plotID)) #390
View(df)
list_plots<- c(unique(df$plotID))
list_laz <-c(paste0(list_plots,"_rec_clip_normalized.laz"))
directory <- c(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/", df$domainID,"_", df$siteID ,"/HtNorm_250_radius/"))

length(directory)
length(list_laz)

# Create Functions for forest volume -------------------------------------------
###############################################################
# Canopy Volume Function - 3 heights (0-5, 5-15, 15-higher)
ForestCount <- function(n) {
  m <- list(
    Understory =sum((dplyr::filter(n, x>0 & x<= 5))$freq),
    Midstory=sum((dplyr::filter(n, x> 5 & x<=15))$freq),
    SubCanopy=sum((dplyr::filter(n, x> 15 & x<=25))$freq),
    Canopy =sum((dplyr::filter(n, x> 25))$freq))
  return(m)
}

# Intermediate Function to get from voxels to inventory function
TempList <- function(p) {
  n <- plyr::count(p@data$Z) 
  return(n)}

# Combines intermediate function and Forest count function
ForestInventory <- function(p){
  q <- TempList(p)%>%ForestCount()
  return(q)}

volume_4bins <- ForestInventory(vox) # completed function.
volume_4bins$Understory

#################################################################################
# RESET For Each For Loop Run ----------------------------------------------------
output_frag_und<- c("plotID","ca_und","clumpy_und","np_und","para_mn_und","tca_und","te_und","volume_und")
output_frag_mid<- c("plotID","ca_mid","clumpy_mid","np_mid","para_mn_mid","tca_mid","te_mid", "volume_mid")
output_frag_subcan<- c("plotID","ca_subcan","clumpy_subcan","np_subcan","para_mn_subcan","tca_subcan","te_subcan", "volume_subcan")
output_frag_can<- c("plotID","ca_can","clumpy_can","np_can","para_mn_can","tca_can","te_can", "volume_can")


###################################################################################
# Calculate Horizontal Habitat Fragmentation at 4 Height Bins
for(k in 1:length(list_laz)){ 
  setwd(directory[k])
  ########-------------------------------------------------------------------------
  laz <- readLAS(list_laz[k])
  ########-------------------------------------------------------------------------
  vox <- voxelize_points(laz, 0.5) # create voxels with 0.5m resolution
  
  ########-------------------------------------------------------------------------
  Understory <- filter_poi(vox, Z>0 & Z<= 5) 
  Midstory<- filter_poi(vox, Z > 5 & Z<= 15)  
  SubCanopy<- filter_poi(vox, Z > 15 & Z<=25)
  Canopy<- filter_poi(vox, Z > 25)
  
  ########-------------------------------------------------------------------------
  # Calculate Volume of Voxels at Height Strata (4 bins)
  volume_4bins <- ForestInventory(vox) # Use premade custom function 
  
  und_vol    <- volume_4bins$Understory
  mid_vol    <- volume_4bins$Midstory
  subcan_vol <- volume_4bins$SubCanopy
  can_vol    <- volume_4bins$Canopy
  
  ########-------------------------------------------------------------------------
  # Understory
  if (sum(Understory$Z)>0){
    Und<- raster(rasterize_canopy(Understory, res=1)) # Continuous raster
    reclass_df_Und <- c(0, 6, 1,
                        NA,NA, 0)
    reclass_m_Und <- matrix(reclass_df_Und,
                            ncol = 3,
                            byrow = TRUE)
    las_Und_raster_classified <- raster::reclassify(Und, # Continuous raster
                                                    reclass_m_Und,
                                                    include.lowest=T)
    las_Und_raster_classified <-terra::rast(las_Und_raster_classified)
    #
    und_ext <- ext(Und)
    und_cent <- c(mean(und_ext[c(1,2)]), mean(und_ext[c(3,4)]))
    pt <- st_point(und_cent)
    buff <- st_buffer(pt, 250)
    buff <- vect(buff)
    und_mask <- mask(las_Und_raster_classified, buff) # input for landscape metrics
    ls_metrics_und <- sample_lsm(und_mask,
                                 y = matrix(und_cent, ncol = 2), shape = 'circle',
                                 size = 250,
                                 what = c('lsm_c_tca',
                                          'lsm_c_np',
                                          'lsm_c_ca',
                                          'lsm_c_te',
                                          'lsm_c_para_mn',
                                          'lsm_c_clumpy'),
                                 return_raster = T)
    ls_und <- ls_metrics_und$raster_sample_plots[[1]]
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters/1m")
    writeRaster(ls_und, paste(list_laz[k],'Understory_1m', 'tif', sep = '.'), overwrite=TRUE)
    #
    ls_metrics_und_test <- dplyr::select(as.data.frame(ls_metrics_und),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_und <- ls_metrics_und_test[2,] # list of values
    maybe_und<- c(list_plots[k],frag_und, und_vol) # NEW: Combine volume with rest
    output_frag_und<- rbind(output_frag_und,maybe_und)
  }
  # }else{
  #output_frag_und<- rbind(output_frag_und, c(list_plots[k],NA,NA,NA,NA,NA,NA)}
  
  ########-------------------------------------------------------------------------
  # Midstory
  if (sum(Midstory$Z) > 0){
    Mid<- raster(rasterize_canopy(Midstory, res=1))   # Continuous raster
    
    reclass_df_Mid <- c(5, 16, 1,
                        NA,NA, 0)
    reclass_m_Mid <- matrix(reclass_df_Mid,
                            ncol = 3,
                            byrow = TRUE)
    las_Mid_raster_classified <- raster::reclassify(Mid, # Continuous raster
                                                    reclass_m_Mid,
                                                    include.lowest=T)
    las_Mid_raster_classified <-terra::rast(las_Mid_raster_classified)
    ########
    # Midstory
    mid_ext <- ext(Mid)
    mid_cent <- c(mean(mid_ext[c(1,2)]), mean(mid_ext[c(3,4)]))
    pt <- st_point(mid_cent)
    buff <- st_buffer(pt, 250)
    buff <- vect(buff)
    
    mid_mask <- mask(las_Mid_raster_classified, buff) # input for landscape metrics
    
    ls_metrics_mid <- sample_lsm(mid_mask,
                                 y = matrix(mid_cent, ncol = 2), shape = 'circle',
                                 size = 250,
                                 what = c('lsm_c_tca',
                                          'lsm_c_np',
                                          'lsm_c_ca',
                                          'lsm_c_te',
                                          'lsm_c_para_mn',
                                          'lsm_c_clumpy'),
                                 return_raster = T)
    ls_mid <- ls_metrics_mid$raster_sample_plots[[1]]
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters/1m")
    writeRaster(ls_mid, paste(list_laz[k],'Midstory_1m', 'tif', sep = '.'), overwrite=TRUE)
    # Midstory
    ls_metrics_mid_test <- dplyr::select(as.data.frame(ls_metrics_mid),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_mid <- ls_metrics_mid_test[2,] # list of values
    maybe_mid<-  c(list_plots[k],frag_mid, mid_vol) # NEW: Add volume to the rest
    
    output_frag_mid<- rbind(output_frag_mid,maybe_mid)}
  #else{output_frag_mid<- rbind(output_frag_mid, c(list_plots[k],NA,NA,NA,NA,NA,NA)}
  
  ########-------------------------------------------------------------------------
  # SubCanopy
  if (sum(SubCanopy$Z) > 0){
    SubCan<- raster(rasterize_canopy(SubCanopy, res=1))     # Continuous raster # load tiff
    
    reclass_df_SubCan <- c(15, 26, 1,
                           NA,NA, 0)
    
    reclass_m_SubCan <- matrix(reclass_df_SubCan,
                            ncol = 3,
                            byrow = TRUE)
    las_SubCan_raster_classified <- raster::reclassify(SubCan, # Continuous raster
                                                    reclass_m_SubCan,
                                                    include.lowest=T)
    las_SubCan_raster_classified <-terra::rast(las_SubCan_raster_classified)
    ########
    # SubCanopy
    subcan_ext <- ext(SubCan)
    subcan_cent <- c(mean(subcan_ext[c(1,2)]), mean(subcan_ext[c(3,4)]))
    pt <- st_point(subcan_cent)
    buff <- st_buffer(pt, 250)
    buff <- vect(buff)
    
    subcan_mask <- mask(las_SubCan_raster_classified, buff) # input for landscape metrics
    
    ls_metrics_subcan <- sample_lsm(subcan_mask,
                                 y = matrix(subcan_cent, ncol = 2), shape = 'circle',
                                 size = 250,
                                 what = c('lsm_c_tca',
                                          'lsm_c_np',
                                          'lsm_c_ca',
                                          'lsm_c_te',
                                          'lsm_c_para_mn',
                                          'lsm_c_clumpy'),
                                 return_raster = T)
    ls_subcan <- ls_metrics_subcan$raster_sample_plots[[1]]
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters/1m")
    writeRaster(ls_subcan, paste(list_laz[k],'SubCanopy_1m', 'tif', sep = '.'), overwrite=TRUE)
    # SubCanopy
    ls_metrics_subcan_test <- dplyr::select(as.data.frame(ls_metrics_subcan),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_subcan <- ls_metrics_subcan_test[2,] # list of values
    maybe_subcan<-  c(list_plots[k],frag_subcan, subcan_vol) # NEW: add volume to the rest
    
    output_frag_subcan<- rbind(output_frag_subcan,maybe_subcan)
  } 
  
  ########-------------------------------------------------------------------------
  # Canopy 
  if (sum(Canopy$Z) > 0){
    Can<- raster(rasterize_canopy(Canopy, res=1))     # Continuous raster # load tiff
    
    reclass_df_Can <- c(25, 1000, 1,
                        0,25, NA,
                        NA,NA, 0)
    reclass_m_Can <- matrix(reclass_df_Can,
                            ncol = 3,
                            byrow = TRUE)
    las_Can_raster_classified <- raster::reclassify(Can, # Continuous raster
                                                    reclass_m_Can,
                                                    include.lowest=T)
    las_Can_raster_classified <-terra::rast(las_Can_raster_classified)
    ########
    # Canopy
    can_ext <- ext(Can)
    can_cent <- c(mean(can_ext[c(1,2)]), mean(can_ext[c(3,4)]))
    pt <- st_point(can_cent)
    buff <- st_buffer(pt, 250)
    buff <- vect(buff)
    
    can_mask <- mask(las_Can_raster_classified, buff) # input for landscape metrics
    
    ls_metrics_can <- sample_lsm(can_mask,
                                 y = matrix(can_cent, ncol = 2), shape = 'circle',
                                 size = 250,
                                 what = c('lsm_c_tca',
                                          'lsm_c_np',
                                          'lsm_c_ca',
                                          'lsm_c_te',
                                          'lsm_c_para_mn',
                                          'lsm_c_clumpy'),
                                 return_raster = T)
    ls_can <- ls_metrics_can$raster_sample_plots[[1]]
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters/1m")
    writeRaster(ls_can, paste(list_laz[k],'Canopy_1m', 'tif', sep = '.'), overwrite=TRUE)
    
    # Canopy
    ls_metrics_can_test <- dplyr::select(as.data.frame(ls_metrics_can),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_can <- ls_metrics_can_test[2,] # list of values
    maybe_can<-  c(list_plots[k],frag_can, can_vol) # NEW: add volume to the rest
    
    output_frag_can<- rbind(output_frag_can,maybe_can)
  } 
  #else{
  #output_frag_und<- rbind(output_frag_und, c(list_plots[k],NA,NA,NA,NA,NA,NA)
  #output_frag_mid<- rbind(output_frag_mid, c(list_plots[k],NA,NA,NA,NA,NA,NA)
  #output_frag_can<- rbind(output_frag_can, c(list_plots[k],NA,NA,NA,NA,NA,NA)
}
}

#####################################################################################
# Clean up data and arrange for final output ----------------------------------------
#####################################################################################
output_frag_und2<- as.data.frame(output_frag_und) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

output_frag_mid2<- as.data.frame(output_frag_mid) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

output_frag_subcan2 <- as.data.frame(output_frag_subcan) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

output_frag_can2 <- as.data.frame(output_frag_can) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

rownames(output_frag_und2)<-NULL
rownames(output_frag_mid2)<-NULL
rownames(output_frag_subcan2)<-NULL
rownames(output_frag_can2)<-NULL

### Final Output--------------------------------------------------------------------
output <- left_join(output_frag_und2, output_frag_mid2,by = "plotID",copy = FALSE)%>%left_join(output_frag_subcan2, by="plotID", copy=FALSE)%>%left_join(output_frag_can2, by="plotID", copy=FALSE)

View(output)
length(output_frag_und2$plotID)
length(output$plotID)
output$plotID

# Original Output: 
# write.csv(output, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output.csv")

# Missing Data Output:
# write.csv(output, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output_missing.csv")

# 1m Data Output:
write.csv(output, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DnMetrics_UndMidSubcanCan_2017_250m_output_1m.csv")

#################################################################################
# REDO VERTICAL CONFIGURATION 
###############################################################
###### Shannon Diversity, Simpson, Variance
vert_configuration <- function(z) {
  list(
    shannon = vegan::diversity(z, index = "shannon"), # Shannon Diversity
    simpson = vegan::diversity(z, index = "simpson"), # Simpson Diversity
    variance = var(z) # Variance
  )
}

#################################################################################
# RESET For Each For Loop Run ----------------------------------------------------
output_vert_config<- c("plotID", "shannon_plot", "simpson_plot", "variance_plot", 1:70)

#################################################################################
for(i in 1:length(list_laz)){
  laz <- readLAS(paste0(directory[i], list_laz[i]))
  vox <- voxelize_points(laz, 0.5)

################################################################################
vox1 <- filter_poi(vox, Z>0 & Z<= 1)# %>%voxelize_points(1)
vox2 <- filter_poi(vox, Z > 1 & Z<= 2)#%>%voxelize_points(1)
vox3 <- filter_poi(vox, Z > 2 & Z<=3)#%>%voxelize_points(1)
vox4 <- filter_poi(vox, Z > 3 & Z<=4)#%>%voxelize_points(1)
vox5 <- filter_poi(vox, Z > 4 & Z<=5)#%>%voxelize_points(1)
vox6 <- filter_poi(vox, Z > 5 & Z<=6)#%>%voxelize_points(1)
vox7 <- filter_poi(vox, Z > 6 & Z<=7)#%>%voxelize_points(1)
vox8 <- filter_poi(vox, Z > 7 & Z<=8)#%>%voxelize_points(1)
vox9 <- filter_poi(vox, Z > 8 & Z<=9)#%>%voxelize_points(1)
vox10 <- filter_poi(vox, Z > 9 & Z<=10)#%>%voxelize_points(1)
vox11 <- filter_poi(vox, Z > 10 & Z<=11)#%>%voxelize_points(1)
vox12 <- filter_poi(vox, Z > 11 & Z<=12)#%>%voxelize_points(1)
vox13 <- filter_poi(vox, Z > 12 & Z<=13)#%>%voxelize_points(1)
vox14 <- filter_poi(vox, Z > 13 & Z<=14)#%>%voxelize_points(1)
vox15 <- filter_poi(vox, Z > 14 & Z<=15)#%>%voxelize_points(1)
vox16 <- filter_poi(vox, Z > 15 & Z<=16)#%>%voxelize_points(1)
vox17 <- filter_poi(vox, Z > 16 & Z<=17)#%>%voxelize_points(1)
vox18 <- filter_poi(vox, Z > 17 & Z<=18)#%>%voxelize_points(1)
vox19 <- filter_poi(vox, Z > 18 & Z<=19)#%>%voxelize_points(1)
vox20 <- filter_poi(vox, Z > 19 & Z<=20)#%>%voxelize_points(1)
vox21 <- filter_poi(vox, Z > 20 & Z<=21)#%>%voxelize_points(1)
vox22 <- filter_poi(vox, Z > 21 & Z<=22)#%>%voxelize_points(1)
vox23 <- filter_poi(vox, Z > 22 & Z<=23)#%>%voxelize_points(1)
vox24 <- filter_poi(vox, Z > 23 & Z<=24)#%>%voxelize_points(1)
vox25 <- filter_poi(vox, Z > 24 & Z<=25)#%>%voxelize_points(1)
vox26 <- filter_poi(vox, Z > 25 & Z<=26)#%>%voxelize_points(1)
vox27 <- filter_poi(vox, Z > 26 & Z<=27)#%>%voxelize_points(1)
vox28 <- filter_poi(vox, Z > 27 & Z<=28)#%>%voxelize_points(1)
vox29 <- filter_poi(vox, Z > 28 & Z<=29)#%>%voxelize_points(1)
vox30 <- filter_poi(vox, Z > 29 & Z<=30)#%>%voxelize_points(1)
vox31 <- filter_poi(vox, Z > 30 & Z<=31)#%>%voxelize_points(1)
vox32 <- filter_poi(vox, Z > 31 & Z<=32)#%>%voxelize_points(1)
vox33 <- filter_poi(vox, Z > 32 & Z<=33)#%>%voxelize_points(1)
vox34 <- filter_poi(vox, Z > 33 & Z<=34)#%>%voxelize_points(1)
vox35 <- filter_poi(vox, Z > 34 & Z<=35)#%>%voxelize_points(1)
vox36 <- filter_poi(vox, Z > 35 & Z<=36)#%>%voxelize_points(1)
vox36 <- filter_poi(vox, Z > 35 & Z<=36)#%>%voxelize_points(1)
vox37<- filter_poi(vox, Z > 36& Z<=37)# %>%voxelize_points(1)
vox38<- filter_poi(vox, Z > 37& Z<=38)# %>%voxelize_points(1)
vox39<- filter_poi(vox, Z > 38& Z<=39)# %>%voxelize_points(1)
vox40<- filter_poi(vox, Z > 39& Z<=40)# %>%voxelize_points(1)
vox41<- filter_poi(vox, Z > 40& Z<=41)# %>%voxelize_points(1)
vox42<- filter_poi(vox, Z > 41& Z<=42)# %>%voxelize_points(1)
vox43<- filter_poi(vox, Z > 42& Z<=43)# %>%voxelize_points(1)
vox44<- filter_poi(vox, Z > 43& Z<=44)# %>%voxelize_points(1)
vox45<- filter_poi(vox, Z > 44& Z<=45)# %>%voxelize_points(1)
vox46<- filter_poi(vox, Z > 45& Z<=46)# %>%voxelize_points(1)
vox47<- filter_poi(vox, Z > 46& Z<=47)# %>%voxelize_points(1)
vox48<- filter_poi(vox, Z > 47& Z<=48)# %>%voxelize_points(1)
vox49<- filter_poi(vox, Z > 48& Z<=49)# %>%voxelize_points(1)
vox50<- filter_poi(vox, Z > 49& Z<=50)# %>%voxelize_points(1)
vox51<- filter_poi(vox, Z > 50& Z<=51)# %>%voxelize_points(1)
vox52<- filter_poi(vox, Z > 51& Z<=52)# %>%voxelize_points(1)
vox53<- filter_poi(vox, Z > 52& Z<=53)# %>%voxelize_points(1)
vox54<- filter_poi(vox, Z > 53& Z<=54)# %>%voxelize_points(1)
vox55<- filter_poi(vox, Z > 54& Z<=55)# %>%voxelize_points(1)
vox56<- filter_poi(vox, Z > 55& Z<=56)# %>%voxelize_points(1)
vox57<- filter_poi(vox, Z > 56& Z<=57)# %>%voxelize_points(1)
vox58<- filter_poi(vox, Z > 57& Z<=58)# %>%voxelize_points(1)
vox59<- filter_poi(vox, Z > 58& Z<=59)# %>%voxelize_points(1)
vox60<- filter_poi(vox, Z > 59& Z<=60)# %>%voxelize_points(1)
vox61<- filter_poi(vox, Z > 60& Z<=61)# %>%voxelize_points(1)
vox62<- filter_poi(vox, Z > 61& Z<=62)# %>%voxelize_points(1)
vox63<- filter_poi(vox, Z > 62& Z<=63)# %>%voxelize_points(1)
vox64<- filter_poi(vox, Z > 63& Z<=64)# %>%voxelize_points(1)
vox65<- filter_poi(vox, Z > 64& Z<=65)# %>%voxelize_points(1)
vox66<- filter_poi(vox, Z > 65& Z<=66)# %>%voxelize_points(1)
vox67<- filter_poi(vox, Z > 66& Z<=67)# %>%voxelize_points(1)
vox68<- filter_poi(vox, Z > 67& Z<=68)# %>%voxelize_points(1)
vox69<- filter_poi(vox, Z > 68& Z<=69)# %>%voxelize_points(1)
vox70<- filter_poi(vox, Z > 69& Z<=70)# %>%voxelize_points(1)
################################################################################
# Create a list of all height volumes
test <- c(length(vox1$Z),length(vox2$Z),length(vox3$Z), length(vox4$Z), length(vox5$Z), length(vox6$Z), length(vox7$Z), length(vox8$Z), length(vox9$Z), length(vox10$Z), length(vox11$Z), length(vox12$Z), length(vox13$Z), length(vox14$Z), length(vox15$Z), length(vox16$Z), length(vox17$Z), length(vox18$Z), length(vox19$Z), length(vox20$Z), length(vox21$Z), length(vox22$Z), length(vox23$Z), length(vox24$Z), length(vox25$Z), length(vox26$Z), length(vox27$Z), length(vox28$Z), length(vox29$Z), length(vox30$Z), length(vox31$Z), length(vox32$Z), length(vox33$Z), length(vox34$Z), length(vox35$Z), length(vox36$Z) , length(vox37$Z), length(vox38$Z), length(vox39$Z), length(vox40$Z), length(vox41$Z), length(vox42$Z), length(vox43$Z), length(vox44$Z), length(vox45$Z), length(vox46$Z), length(vox47$Z), length(vox48$Z), length(vox49$Z), length(vox50$Z), length(vox51$Z), length(vox52$Z), length(vox53$Z), length(vox54$Z), length(vox55$Z), length(vox56$Z), length(vox57$Z), length(vox58$Z), length(vox59$Z), length(vox60$Z), length(vox61$Z), length(vox62$Z), length(vox63$Z), length(vox64$Z), length(vox65$Z), length(vox66$Z), length(vox67$Z), length(vox68$Z), length(vox69$Z), length(vox70$Z) )

################################################################################
# Apply metrics to list of all height volumes 
values <- vert_configuration(test)

shannon_plot <- values$shannon
simpson_plot <- values$simpson
variance_plot <- values$variance
################################################################################
# Summarize output by plotID
plot_by_row_vert <-  c(list_plots[i], shannon_plot, simpson_plot, variance_plot, test) # prep row to add 

# Add new row to the rest of the rows 
output_vert_config <- rbind(output_vert_config, plot_by_row_vert)

}
################################################################################
# Clean up Data output
output_vert_config2 <- output_vert_config

output_vert_config2 <- as.data.frame(output_vert_config) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

rownames(output_vert_config2)<-NULL
View(output_vert_config2)

length(output_vert_config2$plotID)
################################################################################
# Export Data
write.csv(output_vert_config2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_vert_config2.csv")

################################################################################

# cloud_metrics(vox, func = ~ShannonDiv(Z))
# cloud_metrics(laz, func = ~ShannonDiv(Z))

try <- output_vert_config2%>%dplyr::select(plotID, 6:75)

output_vert_configB<- c("plotID", "shannon_plot", "simpson_plot", "variance_plot", "range_min", "range_max", "range_abs", "dist_traveled")

# TEST: NEW metrics
for(k in 1:length(list_plots)){
  temp <- try[k,]
  temp2 <- temp[,2:71]
  temp3 <- as.list(as.data.frame(t(temp2)))
  names(temp3) <- c("row") 
  temp4 <- temp3$row
  temp5 <- temp4[temp4 > 0] # eliminate zero value height bins 
  temp6 <- sum(temp5)
  temp7<- (temp5/temp6)*100 # Convert to a percentage 
  
  temp8 <- c()
  # Loop to create new metrics 
  for(n in 1:(length(temp7)-1)){
    temp8[n] <- (temp7[n]-temp7[n+1])
  }
  
  # "Range"
  range_plotB <- range(temp8)
  range_plotB_min <-  range_plotB[1]
  range_plotB_max <-  range_plotB[2]
  
  range_plotB_abs <-  sum(abs(range_plotB_min - range_plotB_max))
 
  # "Distance Traveled" 
  dist_plotB <- sum(abs(temp8))
  
  # Shannon's, Simpson, Variance
  valuesB <- vert_configuration(temp7)
  
  shannon_plotB <- valuesB$shannon
  simpson_plotB <- valuesB$simpson
  variance_plotB <- valuesB$variance
  ################################################################################
  # Summarize output by plotID
  plot_by_row_vertB <-  c(list_plots[k], shannon_plotB, simpson_plotB, variance_plotB, range_plotB_min, range_plotB_max, range_plotB_abs, dist_plotB) # prep row to add 
  
  # Add new row to the rest of the rows 
  output_vert_configB <- rbind(output_vert_configB, plot_by_row_vertB)
  
}

# Clean up output
output_vert_configB2 <- as.data.frame(output_vert_configB) %>% purrr::set_names(as.character(slice(., 1))) %>% slice(-1)
rownames(output_vert_configB2)<-NULL
# Clean up NA values 
output_vert_configB2["range_abs"][is.na(output_vert_configB2["range_abs"])] <- 0
output_vert_configB2["dist_traveled"][is.na(output_vert_configB2["dist_traveled"])] <- 0


# Examine Data
head(output_vert_configB2)
View(output_vert_configB2 )
# head(output_vert_config2)

# Old version to % version 
cor(as.numeric(output_vert_configB2$shannon_plot), as.numeric(output_vert_config2$shannon_plot))
cor(as.numeric(output_vert_configB2$simpson_plot), as.numeric(output_vert_config2$simpson_plot))
cor(as.numeric(output_vert_configB2$variance_plot), as.numeric(output_vert_config2$variance_plot))

cor(as.numeric(output_vert_config2$shannon_plot), as.numeric(df$Total_veg_amt_NEW))

# new metrics compared to shannon and total vegetation amount 
cor(as.numeric(output_vert_configB2$range_abs), as.numeric(output_vert_config2$shannon_plot))
cor(as.numeric(output_vert_configB2$range_abs), as.numeric(df$Total_veg_amt_NEW))

cor(as.numeric(output_vert_configB2$dist_traveled), as.numeric(output_vert_config2$shannon_plot))
cor(as.numeric(output_vert_configB2$dist_traveled), as.numeric(df$Total_veg_amt_NEW))


write.csv(output_vert_configB2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_vert_configB2.csv")

###################################################################################
# Create overall horizontal habitat heterogeneity metrics

#################################################################################
# RESET For Each For Loop Run ----------------------------------------------------
output_frag_TotVol<- c("plotID","ca_TotVol","clumpy_TotVol","np_TotVol","para_mn_TotVol","tca_TotVol","te_TotVol")

###################################################################################
for(k in 1:length(list_laz)){ 
  setwd(directory[k])
  ########-------------------------------------------------------------------------
  laz <- readLAS(list_laz[k])
  ########-------------------------------------------------------------------------
  vox <- voxelize_points(laz, 0.5) # create voxels with 0.5m resolution
  
  ########-------------------------------------------------------------------------
  TotalVolume <- filter_poi(vox, Z>0) 
  
  ########-------------------------------------------------------------------------
  # Total Volume
  if (sum(TotalVolume$Z)>0){
    TotVol<- raster(rasterize_canopy(TotalVolume, res=1)) # Continuous raster
    
    reclass_df_TotVol <- c(0, 1000, 1, # From 0 to 75, turn into 1
                        NA,NA, 0) # From NA to NA, turn into 0
    reclass_m_TotVol <- matrix(reclass_df_TotVol,
                            ncol = 3,
                            byrow = TRUE)
    las_TotVol_raster_classified <- raster::reclassify(TotVol, # Continuous raster
                                                    reclass_m_TotVol,
                                                    include.lowest=T)
    las_TotVol_raster_classified <-terra::rast(las_TotVol_raster_classified)
    #
    TotVol_ext <- ext(TotVol)
    TotVol_cent <- c(mean(TotVol_ext[c(1,2)]), mean(TotVol_ext[c(3,4)]))
    pt <- st_point(TotVol_cent)
    buff <- st_buffer(pt, 250)
    buff <- vect(buff)
    TotVol_mask <- mask(las_TotVol_raster_classified, buff) # input for landscape metrics
    ls_metrics_TotVol <- sample_lsm(TotVol_mask,
                                 y = matrix(TotVol_cent, ncol = 2), shape = 'circle',
                                 size = 250,
                                 what = c('lsm_c_tca',
                                          'lsm_c_np',
                                          'lsm_c_ca',
                                          'lsm_c_te',
                                          'lsm_c_para_mn',
                                          'lsm_c_clumpy'),
                                 return_raster = T)
    ls_TotVol <- ls_metrics_TotVol$raster_sample_plots[[1]]
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters/TotalVolume_1m")
    writeRaster(ls_TotVol, paste(list_laz[k],'TotalVolume_1m', 'tif', sep = '.'), overwrite=TRUE)
    #
    ls_metrics_TotVol_test <- dplyr::select(as.data.frame(ls_metrics_TotVol),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_TotVol <- ls_metrics_TotVol_test[2,] # list of values
    maybe_TotVol<- c(list_plots[k],frag_TotVol)
    output_frag_TotVol<- rbind(output_frag_TotVol,maybe_TotVol)
  }
}

################################################################################
output_frag_TotVol2 <- as.data.frame(output_frag_TotVol) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

rownames(output_frag_TotVol2)<-NULL

summary(as.numeric(output_frag_TotVol2$clumpy_TotVol))

### Final Output--------------------------------------------------------------------
write.csv(output_frag_TotVol2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/output_frag_TotVol2.csv")


# Load R Packages
library(lidR)
library(raster)
library(rgdal)
library(landscapemetrics)
library(tidyverse)
library(terra)
library(sf)

# Load in Data from separate R file
list_laz # List of all laz file names
list_plots # List of all plots
directory # List of wd file paths

# OR
list_plots<- c(unique(subset(final_DATA_withoutPhylo_D,is.na(te_und))$plotID) )
list_plots <- list_plots[-c(43)] # remove missing files
list_plots <- list_plots[-c(105)] # remove missing files
list_plots <- list_plots[-c(105,106)] # remove missing files

list_laz <-c(paste0(list_plots,"_rec_clip_normalized.laz"))

directory <- c(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/", subset(final_DATA_withoutPhylo_D,is.na(te_und))$domainID,"_",subset(final_DATA_withoutPhylo_D,is.na(te_und))$siteID ,"/HtNorm_250_radius/"))
directory <- directory[-c(43)]
directory <- directory[-c(105)]
directory <- directory[-c(105,106)]

length(directory)
length(list_laz)

# RESET For Each For Loop Run ----------------------------------------------------
output_frag_und<- c("plotID","ca_und","clumpy_und","np_und","para_mn_und","tca_und","te_und")
output_frag_mid<- c("plotID","ca_mid","clumpy_mid","np_mid","para_mn_mid","tca_mid","te_mid")
output_frag_can<- c("plotID","ca_can","clumpy_can","np_can","para_mn_can","tca_can","te_can")

###################################################################################
for(k in 1:length(list_laz)){ 
  setwd(directory[k])
  ########-------------------------------------------------------------------------
  laz <- readLAS(list_laz[k])
  ########-------------------------------------------------------------------------
  vox <- voxelize_points(laz, 5)
  
  ########-------------------------------------------------------------------------
  Understory <- filter_poi(vox, Z>0 & Z<= 5) # LiDAR
  Midstory<- filter_poi(vox, Z > 5, Z<= 15)  # LiDAR
  Canopy<- filter_poi(vox, Z >= 15)
  ########-------------------------------------------------------------------------
  # Understory
  if (sum(Understory$Z)>0){
    Und<- raster(rasterize_canopy(Understory, res=5)) # Continuous raster
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
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters")
    writeRaster(ls_und, paste(list_laz[k],'Understory', 'tif', sep = '.'), overwrite=TRUE)
    #
    ls_metrics_und_test <- dplyr::select(as.data.frame(ls_metrics_und),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_und <- ls_metrics_und_test[2,] # list of values
    maybe_und<- c(list_plots[k],frag_und)
    output_frag_und<- rbind(output_frag_und,maybe_und)
  }
  # }else{
  #output_frag_und<- rbind(output_frag_und, c(list_plots[k],NA,NA,NA,NA,NA,NA)}
  ########-------------------------------------------------------------------------
  # Midstory
  if (sum(Midstory$Z) > 0){
    Mid<- raster(rasterize_canopy(Midstory, res=5))   # Continuous raster
    
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
    
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters")
    writeRaster(ls_mid, paste(list_laz[k],'Midstory', 'tif', sep = '.'), overwrite=TRUE)
    #
    ls_metrics_mid_test <- dplyr::select(as.data.frame(ls_metrics_mid),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_mid <- ls_metrics_mid_test[2,] # list of values
    maybe_mid<-  c(list_plots[k],frag_mid)
    
    output_frag_mid<- rbind(output_frag_mid,maybe_mid)}
  #else{output_frag_mid<- rbind(output_frag_mid, c(list_plots[k],NA,NA,NA,NA,NA,NA)}
  ########-------------------------------------------------------------------------
  # Canopy 
  if (sum(Canopy$Z) > 0){
    Can<- raster(rasterize_canopy(Canopy, res=5))     # Continuous raster # load tiff
    
    reclass_df_Can <- c(14, 1000, 1,
                        0,14, NA,
                        NA,NA, 0)
    reclass_m_Can <- matrix(reclass_df_Can,
                            ncol = 3,
                            byrow = TRUE)
    las_Can_raster_classified <- raster::reclassify(Can, # Continuous raster
                                                    reclass_m_Can,
                                                    include.lowest=T)
    las_Can_raster_classified <-terra::rast(las_Can_raster_classified)
    ########
    setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Rasters")
    # Cannopy
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
    
    writeRaster(ls_can, paste(list_laz[k],'Canopy', 'tif', sep = '.'), overwrite=TRUE)
    
    # Canopy
    ls_metrics_can_test <- dplyr::select(as.data.frame(ls_metrics_can),class,metric,value)%>%filter(class==1)%>%dplyr::select(metric,value)%>%t()
    frag_can <- ls_metrics_can_test[2,] # list of values
    maybe_can<-  c(list_plots[k],frag_can)
    
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

output_frag_can2 <- as.data.frame(output_frag_can) %>%
  purrr::set_names(as.character(slice(., 1))) %>%
  slice(-1)

rownames(output_frag_und2)<-NULL
rownames(output_frag_mid2)<-NULL
rownames(output_frag_can2)<-NULL

### Final Output--------------------------------------------------------------------
output <- left_join(output_frag_und2, output_frag_mid2,by = "plotID",copy = FALSE)%>%left_join(output_frag_can2, by="plotID", copy=FALSE)

View(output)
length(output_frag_und2$plotID)
length(output$plotID)
output$plotID

# Original Output: 
# write.csv(output, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output.csv")

# Missing Data Output:
# write.csv(output, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/Metrics/3DFragmentationMetrics_UndMidCan_2017_250m_output_missing.csv")


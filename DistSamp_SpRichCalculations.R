########################
# Examine Data output from distance modeling. 
########################


###########################################################
# Load Packages --------------------------------------------------------------
library(tidyverse)
library(jagsUI)
###########################################################
# Set WD --------------------------------------------------------------
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/Models")

###########################################################
# Load Data --------------------------------------------------------------
df<- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap_covariateReconstruct.csv", header = T) # Code ready 2017 count data with covariates (filtered)

df<- df%>% dplyr::filter(taxonRank == "species") # Filter out TOOL_007 (which has had 1 species but no distance information)

df <- df%>%dplyr::mutate(domainID = domainID.y, siteID= siteID.y, pointID= pointID.y)

####### Filtering For TEST Purposes ONLY ###############
#df <- df%>%distinct(plotID, .keep_all = T) # filter it down for the sake of testing
#df <- tail(df) # filter it down EVEN MORE!!!

# Clean up NA Data
df <- df%>%dplyr::filter(!is.na(startCloudCoverPercentage)) # filter out cloud cover NAs 
df <- df%>%dplyr::filter(!is.na(kmPerHourObservedWindSpeed))
df <- df%>%dplyr::filter(!is.na(observedAirTemp))
df <- df%>%dplyr::filter(!is.na(elevation)) # filter out elevation NAs 

#### TESTING ONLY ######################################
# Filter out Domains
#  "D16" "D18" "D01" "D02" "D19" "D11" "D10" "D09" "D08" "D03" "D07" "D14" "D06" "D13" "D15" "D17" "D05"
# df <- df%>%dplyr::filter(domainID.x=="D01") # only domain 1
########################################################

df$plotNum <- as.numeric(as.factor(df$plotID)) #create new column to hold numeric site IDs: needed for Kery and Royle code (can't take non-numeric reference data)

df$spec <- as.numeric(as.factor(df$scientificName)) #create new column to hold numeric site IDs: needed for Kery and Royle code (can't take non-numeric reference data)

df <- dplyr::select(df, plotNum, domainID, siteID, plotID, year, month, day, pointCountMinute, taxonRank, taxonID, scientificName, vernacularName, spec, observerDistance, detectionMethod, visualConfirmation, sexOrAge, clusterSize, identifiedBy, nlcdClass, decimalLatitude, decimalLongitude, elevation, startCloudCoverPercentage, endCloudCoverPercentage, startRH, endRH, observedHabitat, observedAirTemp, kmPerHourObservedWindSpeed) # filter out extra columns. 

df$observerDistance <- df$observerDistance/1000 # convert to km

summary(df$plotNum) 
sum(df$clusterSize)

######################
#### IN PROGRESS #########
# Repeat cluster size rows so that they can sum up normally. 
cs_2 <-df %>% filter(clusterSize==2)
cs_3 <-df %>% filter(clusterSize==3)
cs_4 <-df %>% filter(clusterSize==4)
cs_5 <-df %>% filter(clusterSize==5)
cs_6 <-df %>% filter(clusterSize==6)
cs_7 <-df %>% filter(clusterSize==7)
cs_9 <-df %>% filter(clusterSize==9)
cs_12 <-df %>% filter(clusterSize==12)
cs_16 <-df %>% filter(clusterSize==16)
cs_20 <-df %>% filter(clusterSize==20)

# df <- rbind(df,cs_2, cs_3, cs_3) # now there are repeat rows for cluster size of 2 and 3
df <- rbind(df,cs_2, cs_3, cs_3, cs_4, cs_4, cs_4, cs_5, cs_5, cs_5, cs_5, cs_6, cs_6, cs_6, cs_6, cs_6, cs_7, cs_7, cs_7, cs_7, cs_7, cs_7, cs_9, cs_9, cs_9, cs_9, cs_9, cs_9, cs_9, cs_9, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_12, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_16, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20, cs_20)

df$clusterSize <- 1 # asign all clusterSize to 1 since rows are now repeated. 
table(df$clusterSize)

df <- df%>%arrange(plotNum, spec, observerDistance) # arrange table so it's in a standard order

#### IN PROGRESS #########
############ create data matrix in R directly since there are now apparently issues with excel pivot tables. 

# subset needed columns
# blah <- df[,c("plotID","scientificName","clusterSize")]
blah <- df[,c("plotNum","spec","clusterSize")] 

Key <- df[,c("spec","scientificName", "vernacularName")]%>% arrange(spec) %>%unique()

# create pivot table of plotID vs scientificName, summarizing clusterSize
# blah2 <- blah %>% group_by(plotID, scientificName)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% pivot_wider(names_from = scientificName, values_from = clusterSize)

# blah2 <- blah %>% group_by(plotNum, spec)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% pivot_wider(names_from = spec, values_from = clusterSize)

blah2 <- blah %>% group_by(plotNum, spec)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% arrange(spec) %>% pivot_wider(names_from = spec, values_from = clusterSize)

blah2[is.na(blah2)] = 0 # asign zeros to all "NA" 
blah2 # check 
blah2<- blah2%>%arrange(plotNum) # put the plotNum in order

blah2<-as.data.frame(blah2)
row.names(blah2)<- blah2$plotID

colnames(blah2) <- c("plotNum", Key$scientificName) # rename column names to be scientific names

View(Key) # key for spec and scientific names 

####################
model_output <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_output_20000_super.csv")

df_update <-read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_2017_df_update_DistCorrAbund20000_SpRangeNEONDomainOverlap.csv")

library(plyr)
# Calculate species Richness
# conditionally create TRUE False based on numeric threshold and sum up TRUE values 
spRich_original <- ddply(blah2,~plotNum,function(x) {data.frame(Original_SpRich=sum(x[-1]>0))})
spRich_discorr <- ddply(model_output,~plotNum,function(x) {data.frame(distcorr_1_SpRich=sum(x[-1]>1))})

df_update2 <- dplyr::select(df_update, plotNum,c(7:266))
df_update2[is.na(df_update2)] <- 0
df_update2

spRich_discorr_SpRangeFiltered_1 <- ddply(df_update2,~plotNum,function(x) {data.frame(rangefilt_1_SpRich=sum(x[-1]>1))})

spRich_discorr_SpRangeFiltered_95 <- ddply(df_update2,~plotNum,function(x) {data.frame(rangefilt_95_SpRich=sum(x[-1]>0.95))})

####################
df_update3<-df_update2[-1]

df_update3[df_update3>0.95]<-0 # Keep only values for 

df_update4 <-cbind(plotNum=df_update2$plotNum,df_update3)

spRich_discorr_SpRangeFiltered_less95 <- ddply(df_update4,~plotNum,function(x) {data.frame(rangefilt_less95_SpRich=sum(x[-1]))})

spRich_discorr_SpRangeFiltered_95 # sum of .95 and higher as integers

combined_temp <- cbind(spRich_discorr_SpRangeFiltered_95, rangefilt_less95_SpRich=spRich_discorr_SpRangeFiltered_less95$rangefilt_less95_SpRich)

#
spRich_discorr_SpRangeFiltered_lower95 <- cbind(plotNum=df_update2$plotNum,data.frame(rangefilt_95lower_SpRich=rowSums(combined_temp[,c("rangefilt_95_SpRich", "rangefilt_less95_SpRich")])))

####################

spRich_combined <-spRich_original%>% right_join(spRich_discorr, by = "plotNum")

spRich_combined <-spRich_combined%>% right_join(spRich_discorr_SpRangeFiltered_1, by = "plotNum")

spRich_combined <-spRich_combined%>% right_join(spRich_discorr_SpRangeFiltered_95, by = "plotNum")

spRich_combined <-spRich_combined%>% right_join(spRich_discorr_SpRangeFiltered_lower95, by = "plotNum")


# Now this contains the original, dist corrented, and dist corrected with rangemap filtering for species richness values. 
head(spRich_combined)

write_csv(spRich_combined, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/2017_20000_supercomputer_distcorrected_spRich_combined.csv")

# Old Plot
plot(spRich_combined$Original_SpRich, spRich_combined$distcorr_1_SpRich, xlim =c(0,50), ylim=c(0,50), ylab="Dist Corrected spRich", xlab="Original spRich")
abline(coef=c(0,1))

# New Plot
plot(spRich_combined$Original_SpRich, spRich_combined$rangefilt_1_SpRich, xlim =c(0,50), ylim=c(0,50), ylab="Dist Corrected spRich Range Filtered", xlab="Original spRich")
abline(coef=c(0,1))

# 95% Confidence
plot(spRich_combined$Original_SpRich, spRich_combined$rangefilt_95_SpRich, xlim =c(0,50), ylim=c(0,50), ylab="Dist Corrected spRich Range Filtered 95%", xlab="Original spRich")
abline(coef=c(0,1))


# Lower than 95% Confidence as Fractional, Above as numeric
plot(spRich_combined$Original_SpRich, spRich_combined$rangefilt_95lower_SpRich, xlim =c(0,50), ylim=c(0,70), ylab="Dist Corrected spRich Range Filtered Including below 95%", xlab="Original spRich")
abline(coef=c(0,1))

#####################################
hist(spRich_combined$distcorr_RICHNESS,col='red')
hist(spRich_combined$Original_RICHNESS,col='green',add=TRUE)

summary(spRich_combined$Original_RICHNESS)
summary(spRich_combined$df_update2)

#####################################
df_update

FullData <- df_update%>%right_join(spRich_combined, by="plotNum")
write_csv(FullData,"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter.csv")

#####################################
library(ggplot2)
require(viridis)
library(rgdal)

NEON_Domains <- readOGR("/Users/colinsweeney/Documents/Documents/PhD/NEON/Sites/GIS_Files/NEONDomains_0/NEON_Domains.shp")

p1 <- ggplot(FullData, aes(x=decimalLongitude, y=decimalLatitude, colour =rangefilt_95lower_SpRich)) + 
  geom_polygon(data=NEON_Domains, aes(x=long, y=lat, group=group), fill="white", colour="grey10", alpha=1) + 
  geom_point(size=4) + 
  scale_colour_viridis() +
  theme(panel.background = element_rect(fill = 'white', colour = 'grey85'),
        panel.border = element_rect(fill=NA, colour = "white", size=1),
        axis.line = element_line(color = 'black', size=1.5),
        plot.title = element_text(size=15, vjust=2, family="sans"),
        axis.text.x = element_text(colour='black',size=22),
        axis.text.y = element_text(colour='black',size=22),
        axis.title.x = element_text(colour='black',size=22),
        axis.title.y = element_text(colour='black',size=22),
        axis.ticks = element_line(color = 'black', size=1.5),
        axis.ticks.length=unit(0.3,"cm"),
        legend.position="right",
        legend.text=element_text(size=20),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  coord_map("conic", lat0 = 40, xlim = c(-123,-72), ylim = c(25, 50))#+
#coord_map("conic", lat0 = 30, xlim = c(-72.5,-72), ylim = c(42.2,42.7))
p1


p2 <- ggplot(FullData, aes(x=decimalLongitude, y=decimalLatitude, colour = rangefilt_95lower_SpRich)) + 
  geom_polygon(data=NEON_Domains, aes(x=long, y=lat, group=group), fill="grey", colour="grey10", alpha=1) + 
  geom_point(size=3) + 
  ggtitle("Species Richness: rangefilt_95lower_SpRich")+
  xlab("Longitude") + 
  ylab("Latitude") + 
  #scale_fill_binned(guide = guide_coloursteps(show.limits = TRUE, even.steps = FALSE))+
  #scale_size(guide = "legend")+
  scale_color_gradientn(colours = rainbow(6),
                        limits=c(0, 75), na.value = "transparent")+ # Check range: range(FullData$rangefilt_95lower_SpRich)
  #scale_colour_viridis() +
  theme(panel.background = element_rect(fill = 'white', colour = 'grey85'),
        panel.border = element_rect(fill=NA, colour = "white", size=1),
        axis.line = element_line(color = 'black', size=1.5),
        plot.title = element_text(size=24, vjust=0, family="sans", hjust=0.5),
        axis.text.x = element_text(colour='black',size=12),
        axis.text.y = element_text(colour='black',size=12),
        axis.title.x = element_text(colour='black',size=12),
        axis.title.y = element_text(colour='black',size=15),
        #axis.ticks = element_line(color = 'black', size=1.5),
        axis.ticks.length=unit(0.25,"cm"),
        legend.position="right",
        legend.text=element_text(size=13),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #coord_map("conic", lat0 = 40, xlim = c(-123,-72), ylim = c(25, 50)) # Continental US
  #coord_map("conic", lat0 = 65, xlim = c(-170,-130), ylim = c(52,75)) # Alaska
  coord_map("conic", lat0 = 40, xlim = c(-150,-72), ylim = c(25, 75))
p2

summary(FullData$update_95)


ggsave("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Plots/rangefilt_95lower_SpRich.jpeg", p2, width=10,height=10, dpi=600)


p2
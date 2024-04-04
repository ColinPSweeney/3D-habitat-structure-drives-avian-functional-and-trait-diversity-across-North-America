###########################################################
# Code to first load dataframes and then loop through vector of points with the NEON domains based on each species' rangemap overlap to create a final output of Dist Corrected species abundances for each spieces, with NAs filled in where that species does not have a rangemap present. 
# Load Packages --------------------------------------------------------------
library(tidyverse)
library(jagsUI)
library(sp)
library(rgdal)
library(raster)

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
########################################################

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

#############################################################################

df <- dplyr::select(df, plotNum, domainID, siteID, plotID, decimalLatitude, decimalLongitude)

df_test <- df[!duplicated(df$plotID), ]
df_test

############
N_output_20000_super<- read_csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_output_20000_super.csv")

data <- right_join(df_test, N_output_20000_super, by='plotNum')

data # combined ID information with supercomputer distance model N output

############
write.csv(data, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_output_20000_super_withID.csv")

######################################################################


#################################################################
# Only run this line at the start of the for loop process
df_update <- df_test # df_update will be the final exported file 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Load points
DistCorr_SpAbund_original<- st_read("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesAbundance_DistCorr20000_Shapefiles/20000_DistCorr_SpAbund_original.shp")
# reproject
DistCorr_SpAbund_original <- st_transform(DistCorr_SpAbund_original, crs="+proj=longlat +ellps=WGS84 +no_defs ")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap")

 # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# TEST
# NEON_spRang_fileName <-c("Acanthis_flammeaNEONDomain_DistCorr_Super2017_20000_SpRangeOverlap","Accipiter_cooperiiNEONDomain_DistCorr_Super2017_20000_SpRangeOverlap")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# list of names too long, have to split up over two lines. 
NEON_spRang_spName <-c("Acanthis_flammea","Accipiter_cooperii","Agelaius_phoeniceus","Aimophila_ruficeps","Aix_sponsa","Passerculus_henslowii","Ammodramus_savannarum","Amphispiza_bilineata","Mareca_americana","Spatula_discors","Anas_platyrhynchos","Anthus_rubescens","Aphelocoma_californica","Archilochus_alexandri","Archilochus_colubris","Ardea_alba","Ardea_herodias","Auriparus_flaviceps","Aythya_affinis","Aythya_americana","Baeolophus_bicolor","Baeolophus_inornatus","Baeolophus_ridgwayi","Bartramia_longicauda","Bombycilla_cedrorum","Bonasa_umbellus","Branta_canadensis","Bubulcus_ibis","Buteo_jamaicensis","Buteo_lineatus","Buteo_platypterus","Calamospiza_melanocorys","Calcarius_lapponicus","Calcarius_pictus","Calidris_alpina","Calidris_melanotos","Calidris_pusilla","Callipepla_californica","Callipepla_gambelii","Callipepla_squamata","Calypte_anna","Campylorhynchus_brunneicapillus","Cardellina_pusilla","Cardinalis_cardinalis","Cardinalis_sinuatus","Cathartes_aura","Catharus_fuscescens","Catharus_guttatus","Catharus_minimus","Catharus_ustulatus","Catherpes_mexicanus","Certhia_americana","Chaetura_pelagica","Chamaea_fasciata","Charadrius_vociferus","Chlidonias_niger","Chondestes_grammacus","Circus_hudsonius","Cistothorus_palustris","Cistothorus_platensis","Clangula_hyemalis","Coccyzus_americanus","Colaptes_auratus","Colaptes_chrysoides","Colinus_virginianus","Columbina_passerina","Contopus_cooperi","Contopus_sordidulus","Contopus_virens","Corvus_brachyrhynchos","Corvus_corax","Corvus_cryptoleucus","Corvus_ossifragus","Cyanocitta_cristata","Cyanocitta_stelleri","Dolichonyx_oryzivorus","Hylatomus_pileatus","Dumetella_carolinensis","Empidonax_alnorum","Empidonax_difficilis","Empidonax_hammondii","Empidonax_minimus","Empidonax_oberholseri","Empidonax_occidentalis","Empidonax_traillii","Empidonax_virescens","Empidonax_wrightii","Eremophila_alpestris","Eudocimus_albus","Euphagus_cyanocephalus","Falco_sparverius","Gavia_pacifica","Geococcyx_californianus","Geothlypis_formosa","Geothlypis_philadelphia","Geothlypis_tolmiei","Geothlypis_trichas","Antigone_canadensis","Haemorhous_cassinii")
                       
NEON_spRang_spName<- c(NEON_spRang_spName,"Haemorhous_mexicanus","Haemorhous_purpureus","Helmitheros_vermivorum","Hirundo_rustica","Hylocichla_mustelina","Icteria_virens","Icterus_bullockiorum","Icterus_galbula","Icterus_parisorum","Icterus_spurius","Ixoreus_naevius","Junco_hyemalis","Lanius_ludovicianus","Larus_delawarensis","Larus_hyperboreus","Limnothlypis_swainsonii","Loxia_curvirostra","Loxia_leucoptera","Melanerpes_carolinus","Melanerpes_erythrocephalus","Melanerpes_formicivorus","Melanerpes_uropygialis","Meleagris_gallopavo","Melospiza_georgiana","Melospiza_lincolnii","Melospiza_melodia","Melozone_crissalis","Mimus_polyglottos","Mniotilta_varia","Molothrus_ater","Myadestes_townsendi","Myiarchus_cinerascens","Myiarchus_crinitus","Nucifraga_columbiana","Numenius_phaeopus","Nycticorax_nycticorax","Oreoscoptes_montanus","Parkesia_motacilla","Passer_domesticus","Passerculus_sandwichensis","Passerella_iliaca","Passerina_amoena","Passerina_caerulea","Passerina_ciris","Passerina_cyanea","Pelecanus_erythrorhynchos","Perisoreus_canadensis","Petrochelidon_pyrrhonota","Peucaea_aestivalis","Peucaea_botterii","Peucaea_carpalis","Peucaea_cassinii","Phainopepla_nitens","Nannopterum_auritus","Phalaropus_fulicarius","Steganopus_tricolor","Phasianus_colchicus","Pheucticus_ludovicianus","Pheucticus_melanocephalus","Pica_hudsonia","Leuconotopicus_albolarvatus","Dryobates_nuttallii","Dryobates_scalaris","Leuconotopicus_villosus","Pinicola_enucleator","Pipilo_chlorurus","Pipilo_erythrophthalmus","Pipilo_maculatus","Piranga_ludoviciana","Piranga_olivacea","Piranga_rubra","Plectrophenax_nivalis","Pluvialis_dominica","Poecile_atricapillus","Poecile_gambeli","Poecile_hudsonicus","Polioptila_caerulea","Polioptila_melanura","Porzana_carolina","Progne_subis","Protonotaria_citrea","Psaltriparus_minimus","Pyrocephalus_rubinus","Quiscalus_major","Quiscalus_quiscula","Rallus_limicola","Regulus_calendula","Regulus_satrapa","Sayornis_nigricans","Sayornis_phoebe","Seiurus_aurocapilla","Selasphorus_platycercus","Selasphorus_rufus","Setophaga_caerulescens","Setophaga_citrina","Setophaga_coronata","Setophaga_discolor","Setophaga_dominica","Setophaga_fusca","Setophaga_magnolia","Setophaga_nigrescens","Setophaga_occidentalis","Setophaga_pensylvanica","Setophaga_petechia","Setophaga_pinus","Setophaga_ruticilla","Setophaga_striata","Setophaga_virens","Sialia_currucoides","Sialia_mexicana","Sialia_sialis","Sitta_canadensis","Sitta_carolinensis","Sitta_pusilla","Sitta_pygmaea","Sphyrapicus_ruber","Sphyrapicus_varius","Spinus_lawrencei","Spinus_pinus","Spinus_psaltria","Spinus_tristis","Spiza_americana","Spizella_breweri","Spizella_pallida","Spizella_passerina","Spizella_pusilla","Passerella_arborea","Stercorarius_longicaudus","Stercorarius_parasiticus","Sturnella_magna","Sturnella_neglecta","Sturnus_vulgaris","Tachycineta_bicolor","Tachycineta_thalassina","Thryomanes_bewickii","Thryothorus_ludovicianus","Toxostoma_crissale","Toxostoma_curvirostre","Toxostoma_rufum","Troglodytes_aedon","Turdus_migratorius","Tyrannus_forficatus","Tyrannus_tyrannus","Tyrannus_verticalis","Tyrannus_vociferans","Vireo_bellii","Vireo_cassinii","Vireo_flavifrons","Vireo_gilvus","Vireo_griseus","Vireo_huttoni","Vireo_olivaceus","Vireo_plumbeus","Vireo_solitarius","Vireo_vicinior","Xanthocephalus_xanthocephalus","Zenaida_asiatica","Zenaida_macroura","Zonotrichia_albicollis","Zonotrichia_atricapilla","Zonotrichia_leucophrys")

# Prepare the names of the shapefiles in object NEON_spRang_fileName
NEON_spRang_fileName <-c()

for(k in 1:length(NEON_spRang_spName)){
  NEON_spRang_fileName[k] <- paste0(NEON_spRang_spName[k],"NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp")
}

length(NEON_spRang_fileName) # 260 species names

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
for(i in 1:length(NEON_spRang_fileName)){
  # Load files
  NEON_spRange <- st_read(NEON_spRang_fileName[i])
  # Set crs
  NEON_spRange <- st_transform(NEON_spRange, crs="+proj=longlat +ellps=WGS84 +no_defs ")
  # Overlap and save output as Sdash
  Sdash = DistCorr_SpAbund_original[lengths(st_intersects(DistCorr_SpAbund_original,NEON_spRange))>0,]
  # Transform Sdash into Dataframe temp
  temp <- Sdash %>% st_set_geometry(NULL)
  # Select only needed columns
  temp2 <- temp%>%dplyr::select(plotNum, c(7+i))  # plotNum and Only the filtered abundances
  # join temp2 abundance data to df_update file
  df_update <- left_join(df_update, temp2, by="plotNum")
}

head(df_update)
df_update
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

write_csv(df_update, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/outputs/N_2017_df_update_DistCorrAbund20000_SpRangeNEONDomainOverlap.csv")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OLD --------------------------------------------------------------------------

#DistCorr_SpAbund_original<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesAbundance_DistCorr20000_Shapefiles/20000_DistCorr_SpAbund_original.shp")

#plot(DistCorr_SpAbund_original)
#crs(DistCorr_SpAbund_original)
## +proj=longlat +ellps=WGS84 +no_defs 

#setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap")

# Load an example NEON species subsetted shapefile
#NEON_spRange<- readOGR("Acanthis_flammeaNEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp")

# Test to see if they plot well
#plot(NEON_spRange)
#points(DistCorr_SpAbund_original)

##################################################################
#setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap")

#NEON_spRange <- st_read("Acanthis_flammeaNEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp")

#DistCorr_SpAbund_original<- st_read("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesAbundance_DistCorr20000_Shapefiles/20000_DistCorr_SpAbund_original.shp")

#NEON_spRange <- st_transform(NEON_spRange, crs="+proj=longlat +ellps=WGS84 +no_defs ")
#DistCorr_SpAbund_original <- st_transform(DistCorr_SpAbund_original, crs="+proj=longlat +ellps=WGS84 +no_defs ")

#sf::sf_use_s2(FALSE) # used to eliminate error message about spherical geometry.

#Sdash = DistCorr_SpAbund_original[lengths(st_intersects(DistCorr_SpAbund_original,NEON_spRange))>0,]

#st_crs(NEON_spRange)
#st_crs(DistCorr_SpAbund_original)

# st_crosses
#plot(Sdash)
#plot(DistCorr_SpAbund_original)
#############################################
#df_update <- df_test # df_update will be the final exported file

#temp <- Sdash %>% st_set_geometry(NULL)
#temp2 <- temp%>%dplyr::select(plotNum, V1) # plotNum and Only the filtered abundances

#length(temp2$V1)
#length(df_test$plotNum)
#length(df_update$plotNum)

#df_update <- left_join(df_update, temp2, by="plotNum")

#summary(df_update)
#View(df_update)

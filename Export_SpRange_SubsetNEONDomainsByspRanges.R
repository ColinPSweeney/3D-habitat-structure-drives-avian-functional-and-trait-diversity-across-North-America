################################################################################
# Code for exporting NEON domains with overlap with Specific Species Range maps
################################################################################
library(terra)
library(raster)
library(rgdal)
library(landscapemetrics)
library(tidyverse)
library(sf)

################################################################################
birds_behrmann <- readRDS("/Users/colinsweeney/Documents/Documents/GIS_General/Shapefiles/RangeMaps/BirdLifeRangeMaps/birds_behrmann.rds")
bird_global_maps <- readOGR("/Users/colinsweeney/Documents/Documents/GIS_General/Shapefiles/RangeMaps/BirdLifeRangeMaps/birds_global_birdlife.shp")

################################################################################
NEON_usa <- readOGR("/Users/colinsweeney/Documents/Documents/PhD/NEON/Sites/GIS_Files/NEONDomains_0/NEON_Domains.shp")
# print(NEON_usa)
# crs         : +proj=longlat +datum=WGS84 +no_defs 
NEON_usa<- spTransform(NEON_usa, CRS("+proj=longlat +datum=WGS84 +no_defs") )
# plot(NEON_usa)

################################################################################
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/")

################################################################################
# PART 1:
# Export individual species shape files
################################################################################
###### Start of List ########################################################
test <- bird_global_maps[bird_global_maps$SCINAME == "Acanthis flammea", ] 
test <- birds_behrmann[birds_behrmann$SCINAME == "Acanthis flammea",]
writeOGR(obj=test, dsn=".", layer="Acanthis flammea", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Accipiter cooperii", ] #
writeOGR(obj=test, dsn=".", layer="Accipiter cooperii", driver="ESRI Shapefile")#

test <- bird_global_maps[bird_global_maps$SCINAME == "Agelaius phoeniceus", ]
test <- birds_behrmann[birds_behrmann$SCINAME == "Agelaius phoeniceus",]
writeOGR(obj=test, dsn=".", layer="Agelaius phoeniceus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Aimophila ruficeps", ]
test <- birds_behrmann[birds_behrmann$SCINAME == "Aimophila ruficeps",]
writeOGR(obj=test, dsn=".", layer="Aimophila ruficeps", driver="ESRI Shapefile")
#################
test <- bird_global_maps[bird_global_maps$SCINAME == "Aix sponsa", ] #
writeOGR(obj=test, dsn=".", layer="Aix sponsa", driver="ESRI Shapefile")#

test <- bird_global_maps[bird_global_maps$SCINAME == "Ammodramus henslowii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Ammodramus savannarum", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Amphispiza bilineata", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Anas americana", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Anas discors", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Anas platyrhynchos", ] #
writeOGR(obj=test, dsn=".", layer="Anas platyrhynchos", driver="ESRI Shapefile")#

test <- bird_global_maps[bird_global_maps$SCINAME == "Anthus rubescens", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Aphelocoma californica", ]#
writeOGR(obj=test, dsn=".", layer="Aphelocoma californica", driver="ESRI Shapefile")#

test <- bird_global_maps[bird_global_maps$SCINAME == "Archilochus alexandri", ]
writeOGR(obj=test, dsn=".", layer="Archilochus alexandri", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Archilochus colubris", ]
writeOGR(obj=test, dsn=".", layer="Archilochus colubris", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Ardea alba", ]
writeOGR(obj=test, dsn=".", layer="Ardea alba", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Ardea herodias", ]
writeOGR(obj=test, dsn=".", layer="Ardea herodias", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Auriparus flaviceps", ]

test <- bird_global_maps[bird_global_maps$SCINAME == "Aythya affinis", ]
writeOGR(obj=test, dsn=".", layer="Aythya affinis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Aythya americana", ]
writeOGR(obj=test, dsn=".", layer="Aythya americana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Baeolophus bicolor", ]
writeOGR(obj=test, dsn=".", layer="Baeolophus bicolor", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Baeolophus inornatus", ]
writeOGR(obj=test, dsn=".", layer="Baeolophus inornatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Baeolophus ridgwayi", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Bartramia longicauda", ]
writeOGR(obj=test, dsn=".", layer="Bartramia longicauda", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Bombycilla cedrorum", ]
writeOGR(obj=test, dsn=".", layer="Bombycilla cedrorum", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Bonasa umbellus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Branta canadensis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Bubulcus ibis", ]
writeOGR(obj=test, dsn=".", layer="Bubulcus ibis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Buteo jamaicensis", ]
writeOGR(obj=test, dsn=".", layer="Buteo jamaicensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Buteo lineatus", ]
writeOGR(obj=test, dsn=".", layer="Buteo lineatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Buteo platypterus", ]
writeOGR(obj=test, dsn=".", layer="Buteo platypterus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calamospiza melanocorys", ]
writeOGR(obj=test, dsn=".", layer="Calamospiza melanocorys", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calcarius lapponicus", ]
writeOGR(obj=test, dsn=".", layer="Calcarius lapponicus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calcarius pictus", ]
writeOGR(obj=test, dsn=".", layer="Calcarius pictus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calidris alpina", ]
writeOGR(obj=test, dsn=".", layer="Calidris alpina", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calidris melanotos", ]
writeOGR(obj=test, dsn=".", layer="Calidris melanotos", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Calidris pusilla", ]
writeOGR(obj=test, dsn=".", layer="Calidris pusilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Callipepla californica", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Callipepla gambelii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Callipepla squamata", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Calypte anna", ]
writeOGR(obj=test, dsn=".", layer="Calypte anna", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Campylorhynchus brunneicapillus", ]
writeOGR(obj=test, dsn=".", layer="Campylorhynchus brunneicapillus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cardellina pusilla", ]
writeOGR(obj=test, dsn=".", layer="Cardellina pusilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cardinalis cardinalis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Cardinalis sinuatus", ]
writeOGR(obj=test, dsn=".", layer="Cardinalis sinuatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cathartes aura", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Catharus fuscescens", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Catharus guttatus", ]
writeOGR(obj=test, dsn=".", layer="Catharus guttatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Catharus minimus", ]
writeOGR(obj=test, dsn=".", layer="Catharus minimus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Catharus ustulatus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Catherpes mexicanus", ]
writeOGR(obj=test, dsn=".", layer="Catherpes mexicanus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Certhia americana", ]
writeOGR(obj=test, dsn=".", layer="Certhia americana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Chaetura pelagica", ]
writeOGR(obj=test, dsn=".", layer="Chaetura pelagica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Chamaea fasciata", ]
writeOGR(obj=test, dsn=".", layer="Chamaea fasciata", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Charadrius vociferus", ]
writeOGR(obj=test, dsn=".", layer="Charadrius vociferus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Chlidonias niger", ]
writeOGR(obj=test, dsn=".", layer="Chlidonias niger", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Chondestes grammacus", ]
writeOGR(obj=test, dsn=".", layer="Chondestes grammacus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Circus cyaneus", ]
test <- birds_behrmann[birds_behrmann$SCINAME == "Circus hudsonius",]
writeOGR(obj=test, dsn=".", layer="Circus cyaneus_Circus hudsonius", driver="ESRI Shapefile")
######
test <- bird_global_maps[bird_global_maps$SCINAME == "Cistothorus palustris", ]
writeOGR(obj=test, dsn=".", layer="Cistothorus palustris", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cistothorus platensis", ]
writeOGR(obj=test, dsn=".", layer="Cistothorus platensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Clangula hyemalis", ]
writeOGR(obj=test, dsn=".", layer="Clangula hyemalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Coccyzus americanus", ]
writeOGR(obj=test, dsn=".", layer="Coccyzus americanus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Colaptes auratus", ]
writeOGR(obj=test, dsn=".", layer="Colaptes auratus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Colaptes chrysoides", ]
writeOGR(obj=test, dsn=".", layer="Colaptes chrysoides", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Colinus virginianus", ]
writeOGR(obj=test, dsn=".", layer="Colinus virginianus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Columbina passerina", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Contopus cooperi", ]
writeOGR(obj=test, dsn=".", layer="Contopus cooperi", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Contopus sordidulus", ]
writeOGR(obj=test, dsn=".", layer="Contopus sordidulus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Contopus virens", ]
writeOGR(obj=test, dsn=".", layer="Contopus virens", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Corvus brachyrhynchos", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Corvus corax", ]
writeOGR(obj=test, dsn=".", layer="Corvus corax", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Corvus cryptoleucus", ]
writeOGR(obj=test, dsn=".", layer="Corvus cryptoleucus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Corvus ossifragus", ]
writeOGR(obj=test, dsn=".", layer="Corvus ossifragus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cyanocitta cristata", ]
writeOGR(obj=test, dsn=".", layer="Cyanocitta cristata", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Cyanocitta stelleri", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Dolichonyx oryzivorus", ]
writeOGR(obj=test, dsn=".", layer="Dolichonyx oryzivorus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Dryocopus pileatus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Dumetella carolinensis", ]
writeOGR(obj=test, dsn=".", layer="Dumetella carolinensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax alnorum", ]
writeOGR(obj=test, dsn=".", layer="Empidonax alnorum", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax difficilis", ]
writeOGR(obj=test, dsn=".", layer="Empidonax difficilis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax hammondii", ]
writeOGR(obj=test, dsn=".", layer="Empidonax hammondii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax minimus", ]
writeOGR(obj=test, dsn=".", layer="Empidonax minimus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax oberholseri", ]
writeOGR(obj=test, dsn=".", layer="Empidonax oberholseri", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax occidentalis", ]
writeOGR(obj=test, dsn=".", layer="Empidonax occidentalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax traillii", ]
writeOGR(obj=test, dsn=".", layer="Empidonax traillii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax virescens", ]
writeOGR(obj=test, dsn=".", layer="Empidonax virescens", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Empidonax wrightii", ]
writeOGR(obj=test, dsn=".", layer="Empidonax wrightii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Eremophila alpestris", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Eudocimus albus", ]
writeOGR(obj=test, dsn=".", layer="Eudocimus albus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Euphagus cyanocephalus", ]
writeOGR(obj=test, dsn=".", layer="Euphagus cyanocephalus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Falco sparverius", ]
writeOGR(obj=test, dsn=".", layer="Falco sparverius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Gavia pacifica", ]
writeOGR(obj=test, dsn=".", layer="Gavia pacifica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Geococcyx californianus", ]
writeOGR(obj=test, dsn=".", layer="Geococcyx californianus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Geothlypis formosa", ]
writeOGR(obj=test, dsn=".", layer="Geothlypis formosa", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Geothlypis philadelphia", ]
writeOGR(obj=test, dsn=".", layer="Geothlypis philadelphia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Geothlypis tolmiei", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Geothlypis trichas", ]
writeOGR(obj=test, dsn=".", layer="Geothlypis trichas", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Grus canadensis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Haemorhous cassinii", ]
writeOGR(obj=test, dsn=".", layer="Haemorhous cassinii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Haemorhous mexicanus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Haemorhous purpureus", ]
writeOGR(obj=test, dsn=".", layer="Haemorhous purpureus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Helmitheros vermivorum", ]
writeOGR(obj=test, dsn=".", layer="Helmitheros vermivorum", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Hirundo rustica", ]
writeOGR(obj=test, dsn=".", layer="Hirundo rustica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Hylocichla mustelina", ]
writeOGR(obj=test, dsn=".", layer="Hylocichla mustelina", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Icteria virens", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Icterus bullockii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Icterus galbula", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Icterus parisorum", ]
writeOGR(obj=test, dsn=".", layer="Icterus parisorum", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Icterus spurius", ]
writeOGR(obj=test, dsn=".", layer="Icterus spurius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Ixoreus naevius", ]
writeOGR(obj=test, dsn=".", layer="Ixoreus naevius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Junco hyemalis", ]
writeOGR(obj=test, dsn=".", layer="Junco hyemalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Lanius ludovicianus", ]
writeOGR(obj=test, dsn=".", layer="Lanius ludovicianus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Larus delawarensis", ]
writeOGR(obj=test, dsn=".", layer="Larus delawarensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Larus hyperboreus", ]
writeOGR(obj=test, dsn=".", layer="Larus hyperboreus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Limnothlypis swainsonii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Loxia curvirostra", ]
writeOGR(obj=test, dsn=".", layer="Loxia curvirostra", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Loxia leucoptera", ]
writeOGR(obj=test, dsn=".", layer="Loxia leucoptera", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melanerpes carolinus", ]
writeOGR(obj=test, dsn=".", layer="Melanerpes carolinus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melanerpes erythrocephalus", ]
writeOGR(obj=test, dsn=".", layer="Melanerpes erythrocephalus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melanerpes formicivorus", ]
writeOGR(obj=test, dsn=".", layer="Melanerpes formicivorus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melanerpes uropygialis", ]
writeOGR(obj=test, dsn=".", layer="Melanerpes uropygialis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Meleagris gallopavo", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Melospiza georgiana", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Melospiza lincolnii", ]
writeOGR(obj=test, dsn=".", layer="Melospiza lincolnii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melospiza melodia", ]
writeOGR(obj=test, dsn=".", layer="Melospiza melodia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Melozone crissalis", ]
writeOGR(obj=test, dsn=".", layer="Melozone crissalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Mimus polyglottos", ]
writeOGR(obj=test, dsn=".", layer="Mimus polyglottos", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Mniotilta varia", ]
writeOGR(obj=test, dsn=".", layer="Mniotilta varia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Molothrus ater", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Myadestes townsendi", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Myiarchus cinerascens", ]
writeOGR(obj=test, dsn=".", layer="Myiarchus cinerascens", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Myiarchus crinitus", ]
writeOGR(obj=test, dsn=".", layer="Myiarchus crinitus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Nucifraga columbiana", ]
writeOGR(obj=test, dsn=".", layer="Nucifraga columbiana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Numenius phaeopus", ]
writeOGR(obj=test, dsn=".", layer="Numenius phaeopus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Nycticorax nycticorax", ]
writeOGR(obj=test, dsn=".", layer="Nycticorax nycticorax", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Oreoscoptes montanus", ]
writeOGR(obj=test, dsn=".", layer="Oreoscoptes montanus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Parkesia motacilla", ]
writeOGR(obj=test, dsn=".", layer="Parkesia motacilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passer domesticus", ]
writeOGR(obj=test, dsn=".", layer="Passer domesticus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passerculus sandwichensis", ]
writeOGR(obj=test, dsn=".", layer="Passerculus sandwichensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passerella iliaca", ]
writeOGR(obj=test, dsn=".", layer="Passerella iliaca", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passerina amoena", ]
writeOGR(obj=test, dsn=".", layer="Passerina amoena", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passerina caerulea", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Passerina ciris", ]
writeOGR(obj=test, dsn=".", layer="Passerina ciris", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Passerina cyanea", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Pelecanus erythrorhynchos", ]
writeOGR(obj=test, dsn=".", layer="Pelecanus erythrorhynchos", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Perisoreus canadensis", ]
writeOGR(obj=test, dsn=".", layer="Perisoreus canadensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Petrochelidon pyrrhonota", ]
writeOGR(obj=test, dsn=".", layer="Petrochelidon pyrrhonota", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Peucaea aestivalis", ]
writeOGR(obj=test, dsn=".", layer="Peucaea aestivalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Peucaea botterii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Peucaea carpalis", ]
writeOGR(obj=test, dsn=".", layer="Peucaea carpalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Peucaea cassinii", ]
writeOGR(obj=test, dsn=".", layer="Peucaea cassinii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Phainopepla nitens", ]
writeOGR(obj=test, dsn=".", layer="Phainopepla nitens", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Phalacrocorax auritus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Phalaropus fulicarius", ]
writeOGR(obj=test, dsn=".", layer="Phalaropus fulicarius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Phalaropus tricolor", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Phasianus colchicus", ]
writeOGR(obj=test, dsn=".", layer="Phasianus colchicus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Pheucticus ludovicianus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Pheucticus melanocephalus", ]
writeOGR(obj=test, dsn=".", layer="Pheucticus melanocephalus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Pica hudsonia", ]
writeOGR(obj=test, dsn=".", layer="Pica hudsonia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Picoides albolarvatus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Picoides nuttallii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Picoides scalaris", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Picoides villosus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Pinicola enucleator", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Pipilo chlorurus", ]
writeOGR(obj=test, dsn=".", layer="Pipilo chlorurus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Pipilo erythrophthalmus", ]
writeOGR(obj=test, dsn=".", layer="Pipilo erythrophthalmus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Pipilo maculatus", ]
writeOGR(obj=test, dsn=".", layer="Pipilo maculatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Piranga ludoviciana", ]
writeOGR(obj=test, dsn=".", layer="Piranga ludoviciana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Piranga olivacea", ]
writeOGR(obj=test, dsn=".", layer="Piranga olivacea", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Piranga rubra", ]
writeOGR(obj=test, dsn=".", layer="Piranga rubra", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Plectrophenax nivalis", ]
writeOGR(obj=test, dsn=".", layer="Plectrophenax nivalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Pluvialis dominica", ]
writeOGR(obj=test, dsn=".", layer="Pluvialis dominica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Poecile atricapillus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Poecile gambeli", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Poecile hudsonicus", ]
writeOGR(obj=test, dsn=".", layer="Poecile hudsonicus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Polioptila caerulea", ]
writeOGR(obj=test, dsn=".", layer="Polioptila caerulea", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Polioptila melanura", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Porzana carolina", ]
writeOGR(obj=test, dsn=".", layer="Porzana carolina", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Progne subis", ]
writeOGR(obj=test, dsn=".", layer="Progne subis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Protonotaria citrea", ]
writeOGR(obj=test, dsn=".", layer="Protonotaria citrea", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Psaltriparus minimus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Pyrocephalus rubinus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Quiscalus major", ]
writeOGR(obj=test, dsn=".", layer="Quiscalus major", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Quiscalus quiscula", ]
writeOGR(obj=test, dsn=".", layer="Quiscalus quiscula", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Rallus limicola", ]
writeOGR(obj=test, dsn=".", layer="Rallus limicola", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Regulus calendula", ]
writeOGR(obj=test, dsn=".", layer="Regulus calendula", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Regulus satrapa", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Sayornis nigricans", ]
writeOGR(obj=test, dsn=".", layer="Sayornis nigricans", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sayornis phoebe", ]
writeOGR(obj=test, dsn=".", layer="Sayornis phoebe", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Seiurus aurocapilla", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Selasphorus platycercus", ]
writeOGR(obj=test, dsn=".", layer="Selasphorus platycercus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Selasphorus rufus", ]
writeOGR(obj=test, dsn=".", layer="Selasphorus rufus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga caerulescens", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga citrina", ]
writeOGR(obj=test, dsn=".", layer="Setophaga citrina", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga coronata", ]
writeOGR(obj=test, dsn=".", layer="Setophaga coronata", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga discolor", ]
writeOGR(obj=test, dsn=".", layer="Setophaga discolor", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga dominica", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga fusca", ]
writeOGR(obj=test, dsn=".", layer="Setophaga fusca", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga magnolia", ]
writeOGR(obj=test, dsn=".", layer="Setophaga magnolia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga nigrescens", ]
writeOGR(obj=test, dsn=".", layer="Setophaga nigrescens", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga occidentalis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga pensylvanica", ]
writeOGR(obj=test, dsn=".", layer="Setophaga pensylvanica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga petechia", ]
writeOGR(obj=test, dsn=".", layer="Setophaga petechia", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga pinus", ]
writeOGR(obj=test, dsn=".", layer="Setophaga pinus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga ruticilla", ]
writeOGR(obj=test, dsn=".", layer="Setophaga ruticilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga striata", ]
writeOGR(obj=test, dsn=".", layer="Setophaga striata", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Setophaga virens", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Sialia currucoides", ]
writeOGR(obj=test, dsn=".", layer="Sialia currucoides", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sialia mexicana", ]
writeOGR(obj=test, dsn=".", layer="Sialia mexicana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sialia sialis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Sitta canadensis", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Sitta carolinensis", ]
writeOGR(obj=test, dsn=".", layer="Sitta carolinensis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sitta pusilla", ]
writeOGR(obj=test, dsn=".", layer="Sitta pusilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sitta pygmaea", ]
writeOGR(obj=test, dsn=".", layer="Sitta pygmaea", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sphyrapicus ruber", ]
writeOGR(obj=test, dsn=".", layer="Sphyrapicus ruber", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sphyrapicus varius", ]
writeOGR(obj=test, dsn=".", layer="Sphyrapicus varius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spinus lawrencei", ]
writeOGR(obj=test, dsn=".", layer="Spinus lawrencei", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spinus pinus", ]
writeOGR(obj=test, dsn=".", layer="Spinus pinus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spinus psaltria", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Spinus tristis", ]
writeOGR(obj=test, dsn=".", layer="Spinus tristis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spiza americana", ]
writeOGR(obj=test, dsn=".", layer="Spiza americana", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spizella breweri", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Spizella pallida", ]
writeOGR(obj=test, dsn=".", layer="Spizella pallida", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spizella passerina", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Spizella pusilla", ]
writeOGR(obj=test, dsn=".", layer="Spizella pusilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Spizelloides arborea", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Stercorarius longicaudus", ]
writeOGR(obj=test, dsn=".", layer="Stercorarius longicaudus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Stercorarius parasiticus", ]
writeOGR(obj=test, dsn=".", layer="Stercorarius parasiticus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sturnella magna", ]
writeOGR(obj=test, dsn=".", layer="Sturnella magna", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Sturnella neglecta", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Sturnus vulgaris", ]
writeOGR(obj=test, dsn=".", layer="Sturnus vulgaris", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tachycineta bicolor", ]
writeOGR(obj=test, dsn=".", layer="Tachycineta bicolor", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tachycineta thalassina", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Thryomanes bewickii", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Thryothorus ludovicianus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Toxostoma crissale", ]
writeOGR(obj=test, dsn=".", layer="Toxostoma crissale", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Toxostoma curvirostre", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Toxostoma rufum", ]
writeOGR(obj=test, dsn=".", layer="Toxostoma rufum", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Troglodytes aedon", ]
writeOGR(obj=test, dsn=".", layer="Troglodytes aedon", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Turdus migratorius", ]
writeOGR(obj=test, dsn=".", layer="Turdus migratorius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tyrannus forficatus", ]
writeOGR(obj=test, dsn=".", layer="Tyrannus forficatus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tyrannus tyrannus", ]
writeOGR(obj=test, dsn=".", layer="Tyrannus tyrannus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tyrannus verticalis", ]
writeOGR(obj=test, dsn=".", layer="Tyrannus verticalis", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Tyrannus vociferans", ]
writeOGR(obj=test, dsn=".", layer="Tyrannus vociferans", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo bellii", ]
writeOGR(obj=test, dsn=".", layer="Vireo bellii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo cassinii", ]
writeOGR(obj=test, dsn=".", layer="Vireo cassinii", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo flavifrons", ]
writeOGR(obj=test, dsn=".", layer="Vireo flavifrons", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo gilvus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo griseus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo huttoni", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo olivaceus", ]
writeOGR(obj=test, dsn=".", layer="Vireo olivaceus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo plumbeus", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo solitarius", ]
writeOGR(obj=test, dsn=".", layer="Vireo solitarius", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Vireo vicinior", ]
writeOGR(obj=test, dsn=".", layer="Vireo vicinior", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Xanthocephalus xanthocephalus", ]
writeOGR(obj=test, dsn=".", layer="Xanthocephalus xanthocephalus", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Zenaida asiatica", ]
writeOGR(obj=test, dsn=".", layer="Zenaida asiatica", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Zenaida macroura", ]
writeOGR(obj=test, dsn=".", layer="Zenaida macroura", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Zonotrichia albicollis", ]
writeOGR(obj=test, dsn=".", layer="Zonotrichia atricapilla", driver="ESRI Shapefile")

test <- bird_global_maps[bird_global_maps$SCINAME == "Zonotrichia atricapilla", ]
test <- bird_global_maps[bird_global_maps$SCINAME == "Zonotrichia leucophrys", ]
writeOGR(obj=test, dsn=".", layer="Zonotrichia leucophrys", driver="ESRI Shapefile")



# Load individual species rangemaps
Accipiter_cooperii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Accipiter cooperii.shp")
Aix_sponsa<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Aix sponsa.shp")
Anas_platyrhynchos<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Anas platyrhynchos.shp")
Aphelocoma_californica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Aphelocoma californica.shp")
Archilochus_alexandri<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Archilochus alexandri.shp")
Archilochus_colubris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Archilochus colubris.shp")
Ardea_alba<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Ardea alba.shp")
Ardea_herodias<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Ardea herodias.shp")
Aythya_affinis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Aythya affinis.shp")
Aythya_americana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Aythya americana.shp")
Baeolophus_bicolor<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Baeolophus bicolor.shp")
Baeolophus_inornatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Baeolophus inornatus.shp")
Bartramia_longicauda<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Bartramia longicauda.shp")
Bombycilla_cedrorum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Bombycilla cedrorum.shp")
Bubulcus_ibis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Bubulcus ibis.shp")
Buteo_jamaicensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Buteo jamaicensis.shp")
Buteo_lineatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Buteo lineatus.shp")
Buteo_platypterus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Buteo platypterus.shp")
Calamospiza_melanocorys<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calamospiza melanocorys.shp")
Calcarius_lapponicus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calcarius lapponicus.shp")
Calcarius_pictus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calcarius pictus.shp")
Calidris_alpina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calidris alpina.shp")
Calidris_melanotos<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calidris melanotos.shp")
Calidris_pusilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calidris pusilla.shp")
Calypte_anna<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Calypte anna.shp")
Campylorhynchus_brunneicapillus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Campylorhynchus brunneicapillus.shp")
Cardellina_pusilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cardellina pusilla.shp")
Cardinalis_sinuatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cardinalis sinuatus.shp")
Catharus_guttatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Catharus guttatus.shp")
Catharus_minimus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Catharus minimus.shp")
Catherpes_mexicanus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Catherpes mexicanus.shp")
Certhia_americana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Certhia americana.shp")
Chaetura_pelagica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Chaetura pelagica.shp")
Chamaea_fasciata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Chamaea fasciata.shp")
Charadrius_vociferus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Charadrius vociferus.shp")
Chlidonias_niger<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Chlidonias niger.shp")
Chondestes_grammacus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Chondestes grammacus.shp")
Circus_cyaneus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Circus cyaneus.shp")
Circus_cyaneus_Circus_hudsonius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Circus cyaneus_Circus hudsonius.shp")

Cistothorus_palustris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cistothorus palustris.shp")
Cistothorus_platensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cistothorus platensis.shp")
Clangula_hyemalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Clangula hyemalis.shp")
Coccyzus_americanus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Coccyzus americanus.shp")
Colaptes_auratus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Colaptes auratus.shp")
Colaptes_chrysoides<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Colaptes chrysoides.shp")
Colinus_virginianus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Colinus virginianus.shp")
Contopus_cooperi<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Contopus cooperi.shp")
Contopus_sordidulus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Contopus sordidulus.shp")
Contopus_virens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Contopus virens.shp")
Corvus_corax<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Corvus corax.shp")
Corvus_cryptoleucus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Corvus cryptoleucus.shp")
Corvus_ossifragus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Corvus ossifragus.shp")
Cyanocitta_cristata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cyanocitta cristata.shp")
Dolichonyx_oryzivorus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Dolichonyx oryzivorus.shp")
Dumetella_carolinensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Dumetella carolinensis.shp")
Empidonax_alnorum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax alnorum.shp")
Empidonax_difficilis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax difficilis.shp")
Empidonax_hammondii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax hammondii.shp")
Empidonax_minimus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax minimus.shp")
Empidonax_oberholseri<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax oberholseri.shp")
Empidonax_occidentalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax occidentalis.shp")
Empidonax_traillii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax traillii.shp")
Empidonax_virescens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax virescens.shp")
Empidonax_wrightii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Empidonax wrightii.shp")
Eudocimus_albus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Eudocimus albus.shp")
Euphagus_cyanocephalus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Euphagus cyanocephalus.shp")
Falco_sparverius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Falco sparverius.shp")
Gavia_pacifica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Gavia pacifica.shp")
Geococcyx_californianus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Geococcyx californianus.shp")
Geothlypis_formosa<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Geothlypis formosa.shp")
Geothlypis_philadelphia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Geothlypis philadelphia.shp")
Geothlypis_trichas<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Geothlypis trichas.shp")
Haemorhous_cassinii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Haemorhous cassinii.shp")
Haemorhous_purpureus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Haemorhous purpureus.shp")
Helmitheros_vermivorum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Helmitheros vermivorum.shp")
Hirundo_rustica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Hirundo rustica.shp")
Hylocichla_mustelina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Hylocichla mustelina.shp")
Icterus_parisorum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icterus parisorum.shp")
Icterus_spurius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icterus spurius.shp")
Ixoreus_naevius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Ixoreus naevius.shp")
Junco_hyemalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Junco hyemalis.shp")
Lanius_ludovicianus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Lanius ludovicianus.shp")
Larus_delawarensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Larus delawarensis.shp")
Larus_hyperboreus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Larus hyperboreus.shp")
Loxia_curvirostra<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Loxia curvirostra.shp")
Loxia_leucoptera<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Loxia leucoptera.shp")
Melanerpes_carolinus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melanerpes carolinus.shp")
Melanerpes_erythrocephalus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melanerpes erythrocephalus.shp")
Melanerpes_formicivorus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melanerpes formicivorus.shp")
Melanerpes_uropygialis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melanerpes uropygialis.shp")
Melospiza_lincolnii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melospiza lincolnii.shp")
Melospiza_melodia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melospiza melodia.shp")
Melozone_crissalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melozone crissalis.shp")
Mimus_polyglottos<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Mimus polyglottos.shp")
Mniotilta_varia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Mniotilta varia.shp")
Myiarchus_cinerascens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Myiarchus cinerascens.shp")
Myiarchus_crinitus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Myiarchus crinitus.shp")
Nucifraga_columbiana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Nucifraga columbiana.shp")
Numenius_phaeopus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Numenius phaeopus.shp")
Nycticorax_nycticorax<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Nycticorax nycticorax.shp")
Oreoscoptes_montanus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Oreoscoptes montanus.shp")
Parkesia_motacilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Parkesia motacilla.shp")
Passer_domesticus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passer domesticus.shp")
Passerculus_sandwichensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerculus sandwichensis.shp")
Passerella_iliaca<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerella iliaca.shp")
Passerina_amoena<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerina amoena.shp")
Passerina_ciris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerina ciris.shp")
Pelecanus_erythrorhynchos<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pelecanus erythrorhynchos.shp")
Perisoreus_canadensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Perisoreus canadensis.shp")
Petrochelidon_pyrrhonota<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Petrochelidon pyrrhonota.shp")
Peucaea_aestivalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Peucaea aestivalis.shp")
Peucaea_carpalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Peucaea carpalis.shp")
Peucaea_cassinii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Peucaea cassinii.shp")
Phainopepla_nitens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Phainopepla nitens.shp")
Phalaropus_fulicarius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Phalaropus fulicarius.shp")
Phasianus_colchicus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Phasianus colchicus.shp")
Pheucticus_melanocephalus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pheucticus melanocephalus.shp")
Pica_hudsonia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pica hudsonia.shp")
Pipilo_chlorurus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pipilo chlorurus.shp")
Pipilo_erythrophthalmus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pipilo erythrophthalmus.shp")
Pipilo_maculatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pipilo maculatus.shp")
Piranga_ludoviciana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Piranga ludoviciana.shp")
Piranga_olivacea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Piranga olivacea.shp")
Piranga_rubra<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Piranga rubra.shp")
Plectrophenax_nivalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Plectrophenax nivalis.shp")
Pluvialis_dominica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pluvialis dominica.shp")
Poecile_hudsonicus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Poecile hudsonicus.shp")
Polioptila_caerulea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Polioptila caerulea.shp")
Porzana_carolina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Porzana carolina.shp")
Progne_subis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Progne subis.shp")
Protonotaria_citrea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Protonotaria citrea.shp")
Quiscalus_major<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Quiscalus major.shp")
Quiscalus_quiscula<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Quiscalus quiscula.shp")
Rallus_limicola<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Rallus limicola.shp")
Regulus_calendula<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Regulus calendula.shp")
Sayornis_nigricans<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sayornis nigricans.shp")
Sayornis_phoebe<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sayornis phoebe.shp")
Selasphorus_platycercus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Selasphorus platycercus.shp")
Selasphorus_rufus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Selasphorus rufus.shp")
Setophaga_citrina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga citrina.shp")
Setophaga_coronata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga coronata.shp")
Setophaga_discolor<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga discolor.shp")
Setophaga_fusca<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga fusca.shp")
Setophaga_magnolia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga magnolia.shp")
Setophaga_nigrescens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga nigrescens.shp")
Setophaga_pensylvanica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga pensylvanica.shp")
Setophaga_petechia<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga petechia.shp")
Setophaga_pinus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga pinus.shp")
Setophaga_ruticilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga ruticilla.shp")
Setophaga_striata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga striata.shp")
Sialia_currucoides<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sialia currucoides.shp")
Sialia_mexicana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sialia mexicana.shp")
Sitta_carolinensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sitta carolinensis.shp")
Sitta_pusilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sitta pusilla.shp")
Sitta_pygmaea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sitta pygmaea.shp")
Sphyrapicus_ruber<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sphyrapicus ruber.shp")
Sphyrapicus_varius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sphyrapicus varius.shp")
Spinus_lawrencei<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spinus lawrencei.shp")
Spinus_pinus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spinus pinus.shp")
Spinus_tristis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spinus tristis.shp")
Spiza_americana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spiza americana.shp")
Spizella_pallida<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spizella pallida.shp")
Spizella_pusilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spizella pusilla.shp")
Stercorarius_longicaudus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Stercorarius longicaudus.shp")
Stercorarius_parasiticus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Stercorarius parasiticus.shp")
Sturnella_magna<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sturnella magna.shp")
Sturnus_vulgaris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sturnus vulgaris.shp")
Tachycineta_bicolor<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tachycineta bicolor.shp")
Toxostoma_crissale<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Toxostoma crissale.shp")
Toxostoma_rufum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Toxostoma rufum.shp")
Troglodytes_aedon<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Troglodytes aedon.shp")
Turdus_migratorius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Turdus migratorius.shp")
Tyrannus_forficatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tyrannus forficatus.shp")
Tyrannus_tyrannus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tyrannus tyrannus.shp")
Tyrannus_verticalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tyrannus verticalis.shp")
Tyrannus_vociferans<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tyrannus vociferans.shp")
Vireo_bellii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo bellii.shp")
Vireo_cassinii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo cassinii.shp")
Vireo_flavifrons<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo flavifrons.shp")
Vireo_olivaceus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo olivaceus.shp")
Vireo_solitarius<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo solitarius.shp")
Vireo_vicinior<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo vicinior.shp")
Xanthocephalus_xanthocephalus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Xanthocephalus xanthocephalus.shp")
Zenaida_asiatica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Zenaida asiatica.shp")
Zenaida_macroura<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Zenaida macroura.shp")
Zonotrichia_atricapilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Zonotrichia atricapilla.shp")
Zonotrichia_leucophrys<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Zonotrichia leucophrys.shp")
#########################################################################################
# Reproject 
Accipiter_cooperii<-spTransform(Accipiter_cooperii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Aix_sponsa<-spTransform(Aix_sponsa,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Anas_platyrhynchos<-spTransform(Anas_platyrhynchos,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Aphelocoma_californica<-spTransform(Aphelocoma_californica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Archilochus_alexandri<-spTransform(Archilochus_alexandri,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Archilochus_colubris<-spTransform(Archilochus_colubris,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Ardea_alba<-spTransform(Ardea_alba,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Ardea_herodias<-spTransform(Ardea_herodias,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Aythya_affinis<-spTransform(Aythya_affinis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Aythya_americana<-spTransform(Aythya_americana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Baeolophus_bicolor<-spTransform(Baeolophus_bicolor,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Baeolophus_inornatus<-spTransform(Baeolophus_inornatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Bartramia_longicauda<-spTransform(Bartramia_longicauda,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Bombycilla_cedrorum<-spTransform(Bombycilla_cedrorum,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Bubulcus_ibis<-spTransform(Bubulcus_ibis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Buteo_jamaicensis<-spTransform(Buteo_jamaicensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Buteo_lineatus<-spTransform(Buteo_lineatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Buteo_platypterus<-spTransform(Buteo_platypterus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calamospiza_melanocorys<-spTransform(Calamospiza_melanocorys,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calcarius_lapponicus<-spTransform(Calcarius_lapponicus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calcarius_pictus<-spTransform(Calcarius_pictus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calidris_alpina<-spTransform(Calidris_alpina,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calidris_melanotos<-spTransform(Calidris_melanotos,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calidris_pusilla<-spTransform(Calidris_pusilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Calypte_anna<-spTransform(Calypte_anna,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Campylorhynchus_brunneicapillus<-spTransform(Campylorhynchus_brunneicapillus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Cardellina_pusilla<-spTransform(Cardellina_pusilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Cardinalis_sinuatus<-spTransform(Cardinalis_sinuatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Catharus_guttatus<-spTransform(Catharus_guttatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Catharus_minimus<-spTransform(Catharus_minimus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Catherpes_mexicanus<-spTransform(Catherpes_mexicanus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Certhia_americana<-spTransform(Certhia_americana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Chaetura_pelagica<-spTransform(Chaetura_pelagica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Chamaea_fasciata<-spTransform(Chamaea_fasciata,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Charadrius_vociferus<-spTransform(Charadrius_vociferus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Chlidonias_niger<-spTransform(Chlidonias_niger,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Chondestes_grammacus<-spTransform(Chondestes_grammacus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Circus_cyaneus<-spTransform(Circus_cyaneus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Circus_cyaneus_Circus_hudsonius<- spTransform(Circus_cyaneus_Circus_hudsonius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
####
Cistothorus_palustris<-spTransform(Cistothorus_palustris,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Cistothorus_platensis<-spTransform(Cistothorus_platensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Clangula_hyemalis<-spTransform(Clangula_hyemalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Coccyzus_americanus<-spTransform(Coccyzus_americanus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Colaptes_auratus<-spTransform(Colaptes_auratus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Colaptes_chrysoides<-spTransform(Colaptes_chrysoides,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Colinus_virginianus<-spTransform(Colinus_virginianus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Contopus_cooperi<-spTransform(Contopus_cooperi,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Contopus_sordidulus<-spTransform(Contopus_sordidulus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Contopus_virens<-spTransform(Contopus_virens,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Corvus_corax<-spTransform(Corvus_corax,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Corvus_cryptoleucus<-spTransform(Corvus_cryptoleucus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Corvus_ossifragus<-spTransform(Corvus_ossifragus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Cyanocitta_cristata<-spTransform(Cyanocitta_cristata,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Dolichonyx_oryzivorus<-spTransform(Dolichonyx_oryzivorus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Dumetella_carolinensis<-spTransform(Dumetella_carolinensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_alnorum<-spTransform(Empidonax_alnorum,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_difficilis<-spTransform(Empidonax_difficilis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_hammondii<-spTransform(Empidonax_hammondii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_minimus<-spTransform(Empidonax_minimus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_oberholseri<-spTransform(Empidonax_oberholseri,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_occidentalis<-spTransform(Empidonax_occidentalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_traillii<-spTransform(Empidonax_traillii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_virescens<-spTransform(Empidonax_virescens,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Empidonax_wrightii<-spTransform(Empidonax_wrightii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Eudocimus_albus<-spTransform(Eudocimus_albus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Euphagus_cyanocephalus<-spTransform(Euphagus_cyanocephalus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Falco_sparverius<-spTransform(Falco_sparverius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Gavia_pacifica<-spTransform(Gavia_pacifica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Geococcyx_californianus<-spTransform(Geococcyx_californianus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Geothlypis_formosa<-spTransform(Geothlypis_formosa,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Geothlypis_philadelphia<-spTransform(Geothlypis_philadelphia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Geothlypis_trichas<-spTransform(Geothlypis_trichas,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Haemorhous_cassinii<-spTransform(Haemorhous_cassinii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Haemorhous_purpureus<-spTransform(Haemorhous_purpureus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Helmitheros_vermivorum<-spTransform(Helmitheros_vermivorum,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Hirundo_rustica<-spTransform(Hirundo_rustica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Hylocichla_mustelina<-spTransform(Hylocichla_mustelina,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Icterus_parisorum<-spTransform(Icterus_parisorum,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Icterus_spurius<-spTransform(Icterus_spurius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Ixoreus_naevius<-spTransform(Ixoreus_naevius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Junco_hyemalis<-spTransform(Junco_hyemalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Lanius_ludovicianus<-spTransform(Lanius_ludovicianus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Larus_delawarensis<-spTransform(Larus_delawarensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Larus_hyperboreus<-spTransform(Larus_hyperboreus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Loxia_curvirostra<-spTransform(Loxia_curvirostra,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Loxia_leucoptera<-spTransform(Loxia_leucoptera,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melanerpes_carolinus<-spTransform(Melanerpes_carolinus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melanerpes_erythrocephalus<-spTransform(Melanerpes_erythrocephalus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melanerpes_formicivorus<-spTransform(Melanerpes_formicivorus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melanerpes_uropygialis<-spTransform(Melanerpes_uropygialis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melospiza_lincolnii<-spTransform(Melospiza_lincolnii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melospiza_melodia<-spTransform(Melospiza_melodia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Melozone_crissalis<-spTransform(Melozone_crissalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Mimus_polyglottos<-spTransform(Mimus_polyglottos,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Mniotilta_varia<-spTransform(Mniotilta_varia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Myiarchus_cinerascens<-spTransform(Myiarchus_cinerascens,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Myiarchus_crinitus<-spTransform(Myiarchus_crinitus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Nucifraga_columbiana<-spTransform(Nucifraga_columbiana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Numenius_phaeopus<-spTransform(Numenius_phaeopus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Nycticorax_nycticorax<-spTransform(Nycticorax_nycticorax,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Oreoscoptes_montanus<-spTransform(Oreoscoptes_montanus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Parkesia_motacilla<-spTransform(Parkesia_motacilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Passer_domesticus<-spTransform(Passer_domesticus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Passerculus_sandwichensis<-spTransform(Passerculus_sandwichensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Passerella_iliaca<-spTransform(Passerella_iliaca,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Passerina_amoena<-spTransform(Passerina_amoena,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Passerina_ciris<-spTransform(Passerina_ciris,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pelecanus_erythrorhynchos<-spTransform(Pelecanus_erythrorhynchos,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Perisoreus_canadensis<-spTransform(Perisoreus_canadensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Petrochelidon_pyrrhonota<-spTransform(Petrochelidon_pyrrhonota,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Peucaea_aestivalis<-spTransform(Peucaea_aestivalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Peucaea_carpalis<-spTransform(Peucaea_carpalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Peucaea_cassinii<-spTransform(Peucaea_cassinii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Phainopepla_nitens<-spTransform(Phainopepla_nitens,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Phalaropus_fulicarius<-spTransform(Phalaropus_fulicarius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Phasianus_colchicus<-spTransform(Phasianus_colchicus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pheucticus_melanocephalus<-spTransform(Pheucticus_melanocephalus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pica_hudsonia<-spTransform(Pica_hudsonia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pipilo_chlorurus<-spTransform(Pipilo_chlorurus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pipilo_erythrophthalmus<-spTransform(Pipilo_erythrophthalmus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pipilo_maculatus<-spTransform(Pipilo_maculatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Piranga_ludoviciana<-spTransform(Piranga_ludoviciana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Piranga_olivacea<-spTransform(Piranga_olivacea,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Piranga_rubra<-spTransform(Piranga_rubra,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Plectrophenax_nivalis<-spTransform(Plectrophenax_nivalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Pluvialis_dominica<-spTransform(Pluvialis_dominica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Poecile_hudsonicus<-spTransform(Poecile_hudsonicus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Polioptila_caerulea<-spTransform(Polioptila_caerulea,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Porzana_carolina<-spTransform(Porzana_carolina,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Progne_subis<-spTransform(Progne_subis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Protonotaria_citrea<-spTransform(Protonotaria_citrea,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Quiscalus_major<-spTransform(Quiscalus_major,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Quiscalus_quiscula<-spTransform(Quiscalus_quiscula,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Rallus_limicola<-spTransform(Rallus_limicola,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Regulus_calendula<-spTransform(Regulus_calendula,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sayornis_nigricans<-spTransform(Sayornis_nigricans,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sayornis_phoebe<-spTransform(Sayornis_phoebe,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Selasphorus_platycercus<-spTransform(Selasphorus_platycercus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Selasphorus_rufus<-spTransform(Selasphorus_rufus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_citrina<-spTransform(Setophaga_citrina,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_coronata<-spTransform(Setophaga_coronata,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_discolor<-spTransform(Setophaga_discolor,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_fusca<-spTransform(Setophaga_fusca,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_magnolia<-spTransform(Setophaga_magnolia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_nigrescens<-spTransform(Setophaga_nigrescens,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_pensylvanica<-spTransform(Setophaga_pensylvanica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_petechia<-spTransform(Setophaga_petechia,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_pinus<-spTransform(Setophaga_pinus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_ruticilla<-spTransform(Setophaga_ruticilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Setophaga_striata<-spTransform(Setophaga_striata,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sialia_currucoides<-spTransform(Sialia_currucoides,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sialia_mexicana<-spTransform(Sialia_mexicana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sitta_carolinensis<-spTransform(Sitta_carolinensis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sitta_pusilla<-spTransform(Sitta_pusilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sitta_pygmaea<-spTransform(Sitta_pygmaea,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sphyrapicus_ruber<-spTransform(Sphyrapicus_ruber,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sphyrapicus_varius<-spTransform(Sphyrapicus_varius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spinus_lawrencei<-spTransform(Spinus_lawrencei,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spinus_pinus<-spTransform(Spinus_pinus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spinus_tristis<-spTransform(Spinus_tristis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spiza_americana<-spTransform(Spiza_americana,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spizella_pallida<-spTransform(Spizella_pallida,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Spizella_pusilla<-spTransform(Spizella_pusilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Stercorarius_longicaudus<-spTransform(Stercorarius_longicaudus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Stercorarius_parasiticus<-spTransform(Stercorarius_parasiticus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sturnella_magna<-spTransform(Sturnella_magna,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Sturnus_vulgaris<-spTransform(Sturnus_vulgaris,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Tachycineta_bicolor<-spTransform(Tachycineta_bicolor,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Toxostoma_crissale<-spTransform(Toxostoma_crissale,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Toxostoma_rufum<-spTransform(Toxostoma_rufum,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Troglodytes_aedon<-spTransform(Troglodytes_aedon,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Turdus_migratorius<-spTransform(Turdus_migratorius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Tyrannus_forficatus<-spTransform(Tyrannus_forficatus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Tyrannus_tyrannus<-spTransform(Tyrannus_tyrannus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Tyrannus_verticalis<-spTransform(Tyrannus_verticalis,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Tyrannus_vociferans<-spTransform(Tyrannus_vociferans,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_bellii<-spTransform(Vireo_bellii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_cassinii<-spTransform(Vireo_cassinii,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_flavifrons<-spTransform(Vireo_flavifrons,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_olivaceus<-spTransform(Vireo_olivaceus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_solitarius<-spTransform(Vireo_solitarius,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Vireo_vicinior<-spTransform(Vireo_vicinior,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Xanthocephalus_xanthocephalus<-spTransform(Xanthocephalus_xanthocephalus,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Zenaida_asiatica<-spTransform(Zenaida_asiatica,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Zenaida_macroura<-spTransform(Zenaida_macroura,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Zonotrichia_atricapilla<-spTransform(Zonotrichia_atricapilla,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )
Zonotrichia_leucophrys<-spTransform(Zonotrichia_leucophrys,CRS("+proj=longlat +datum=WGS84 +no_defs"  ) )

##########################################################################################3
# Same thing for species that didn't export properly
test <- birds_behrmann[birds_behrmann$SCINAME=="Acanthis flammea",]
writeOGR(obj=test, dsn= "." , layer="Acanthis flammea" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Agelaius phoeniceus",]
writeOGR(obj=test, dsn= "." , layer="Agelaius phoeniceus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Aimophila ruficeps",]
writeOGR(obj=test, dsn= "." , layer="Aimophila ruficeps" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Ammodramus henslowii",]
writeOGR(obj=test, dsn= "." , layer="Ammodramus henslowii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Ammodramus savannarum",]
writeOGR(obj=test, dsn= "." , layer="Ammodramus savannarum" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Amphispiza bilineata",]
writeOGR(obj=test, dsn= "." , layer="Amphispiza bilineata" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Anas americana",]
writeOGR(obj=test, dsn= "." , layer="Anas americana" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Anas discors",]
writeOGR(obj=test, dsn= "." , layer="Anas discors" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Anthus rubescens",]
writeOGR(obj=test, dsn= "." , layer="Anthus rubescens" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Auriparus flaviceps",]
writeOGR(obj=test, dsn= "." , layer="Auriparus flaviceps" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Baeolophus ridgwayi",]
writeOGR(obj=test, dsn= "." , layer="Baeolophus ridgwayi" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Bonasa umbellus",]
writeOGR(obj=test, dsn= "." , layer="Bonasa umbellus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Branta canadensis",]
writeOGR(obj=test, dsn= "." , layer="Branta canadensis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Callipepla californica",]
writeOGR(obj=test, dsn= "." , layer="Callipepla californica" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Callipepla gambelii",]
writeOGR(obj=test, dsn= "." , layer="Callipepla gambelii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Callipepla squamata",]
writeOGR(obj=test, dsn= "." , layer="Callipepla squamata" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Cardinalis cardinalis",]
writeOGR(obj=test, dsn= "." , layer="Cardinalis cardinalis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Cathartes aura",]
writeOGR(obj=test, dsn= "." , layer="Cathartes aura" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Catharus fuscescens",]
writeOGR(obj=test, dsn= "." , layer="Catharus fuscescens" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Catharus ustulatus",]
writeOGR(obj=test, dsn= "." , layer="Catharus ustulatus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Columbina passerina",]
writeOGR(obj=test, dsn= "." , layer="Columbina passerina" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Corvus brachyrhynchos",]
writeOGR(obj=test, dsn= "." , layer="Corvus brachyrhynchos" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Cyanocitta stelleri",]
writeOGR(obj=test, dsn= "." , layer="Cyanocitta stelleri" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Dryocopus pileatus",]
writeOGR(obj=test, dsn= "." , layer="Dryocopus pileatus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Eremophila alpestris",]
writeOGR(obj=test, dsn= "." , layer="Eremophila alpestris" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Geothlypis tolmiei",]
writeOGR(obj=test, dsn= "." , layer="Geothlypis tolmiei" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Grus canadensis",]
writeOGR(obj=test, dsn= "." , layer="Grus canadensis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Haemorhous mexicanus",]
writeOGR(obj=test, dsn= "." , layer="Haemorhous mexicanus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Icteria virens",]
writeOGR(obj=test, dsn= "." , layer="Icteria virens" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Icterus bullockii",]
writeOGR(obj=test, dsn= "." , layer="Icterus bullockii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Icterus galbula",]
writeOGR(obj=test, dsn= "." , layer="Icterus galbula" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Limnothlypis swainsonii",]
writeOGR(obj=test, dsn= "." , layer="Limnothlypis swainsonii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Meleagris gallopavo",]
writeOGR(obj=test, dsn= "." , layer="Meleagris gallopavo" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Melospiza georgiana",]
writeOGR(obj=test, dsn= "." , layer="Melospiza georgiana" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Molothrus ater",]
writeOGR(obj=test, dsn= "." , layer="Molothrus ater" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Myadestes townsendi",]
writeOGR(obj=test, dsn= "." , layer="Myadestes townsendi" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Passerina caerulea",]
writeOGR(obj=test, dsn= "." , layer="Passerina caerulea" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Passerina cyanea",]
writeOGR(obj=test, dsn= "." , layer="Passerina cyanea" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Peucaea botterii",]
writeOGR(obj=test, dsn= "." , layer="Peucaea botterii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Phalacrocorax auritus",]
writeOGR(obj=test, dsn= "." , layer="Phalacrocorax auritus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Phalaropus tricolor",]
writeOGR(obj=test, dsn= "." , layer="Phalaropus tricolor" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Pheucticus ludovicianus",]
writeOGR(obj=test, dsn= "." , layer="Pheucticus ludovicianus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Picoides albolarvatus",]
writeOGR(obj=test, dsn= "." , layer="Picoides albolarvatus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Picoides nuttallii",]
writeOGR(obj=test, dsn= "." , layer="Picoides nuttallii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Picoides scalaris",]
writeOGR(obj=test, dsn= "." , layer="Picoides scalaris" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Picoides villosus",]
writeOGR(obj=test, dsn= "." , layer="Picoides villosus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Pinicola enucleator",]
writeOGR(obj=test, dsn= "." , layer="Pinicola enucleator" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Poecile atricapillus",]
writeOGR(obj=test, dsn= "." , layer="Poecile atricapillus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Poecile gambeli",]
writeOGR(obj=test, dsn= "." , layer="Poecile gambeli" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Polioptila melanura",]
writeOGR(obj=test, dsn= "." , layer="Polioptila melanura" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Psaltriparus minimus",]
writeOGR(obj=test, dsn= "." , layer="Psaltriparus minimus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Pyrocephalus rubinus",]
writeOGR(obj=test, dsn= "." , layer="Pyrocephalus rubinus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Regulus satrapa",]
writeOGR(obj=test, dsn= "." , layer="Regulus satrapa" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Seiurus aurocapilla",]
writeOGR(obj=test, dsn= "." , layer="Seiurus aurocapilla" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Setophaga caerulescens",]
writeOGR(obj=test, dsn= "." , layer="Setophaga caerulescens" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Setophaga dominica",]
writeOGR(obj=test, dsn= "." , layer="Setophaga dominica" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Setophaga occidentalis",]
writeOGR(obj=test, dsn= "." , layer="Setophaga occidentalis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Setophaga virens",]
writeOGR(obj=test, dsn= "." , layer="Setophaga virens" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Sialia sialis",]
writeOGR(obj=test, dsn= "." , layer="Sialia sialis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Sitta canadensis",]
writeOGR(obj=test, dsn= "." , layer="Sitta canadensis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Spinus psaltria",]
writeOGR(obj=test, dsn= "." , layer="Spinus psaltria" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Spizella breweri",]
writeOGR(obj=test, dsn= "." , layer="Spizella breweri" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Spizella passerina",]
writeOGR(obj=test, dsn= "." , layer="Spizella passerina" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Spizelloides arborea",]
writeOGR(obj=test, dsn= "." , layer="Spizelloides arborea" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Sturnella neglecta",]
writeOGR(obj=test, dsn= "." , layer="Sturnella neglecta" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Tachycineta thalassina",]
writeOGR(obj=test, dsn= "." , layer="Tachycineta thalassina" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Thryomanes bewickii",]
writeOGR(obj=test, dsn= "." , layer="Thryomanes bewickii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Thryothorus ludovicianus" ,]
writeOGR(obj=test, dsn= "." , layer="Thryothorus ludovicianus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Toxostoma curvirostre",]
writeOGR(obj=test, dsn= "." , layer="Toxostoma curvirostre" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Vireo gilvus",]
writeOGR(obj=test, dsn= "." , layer="Vireo gilvus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Vireo griseus",]
writeOGR(obj=test, dsn= "." , layer="Vireo griseus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Vireo huttoni",]
writeOGR(obj=test, dsn= "." , layer="Vireo huttoni" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Vireo plumbeus",]
writeOGR(obj=test, dsn= "." , layer="Vireo plumbeus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Zonotrichia albicollis",]
writeOGR(obj=test, dsn= "." , layer="Zonotrichia albicollis" , driver= "ESRI Shapefile" )

#

Acanthis_flammea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Acanthis flammea.shp")
Agelaius_phoeniceus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Agelaius phoeniceus.shp")
Aimophila_ruficeps<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Aimophila ruficeps.shp" )
Ammodramus_henslowii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Ammodramus henslowii.shp")
Ammodramus_savannarum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Ammodramus savannarum.shp")
Amphispiza_bilineata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Amphispiza bilineata.shp")
Anas_americana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Anas americana.shp")
Anas_discors<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Anas discors.shp")
Anthus_rubescens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Anthus rubescens.shp")
Auriparus_flaviceps<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Auriparus flaviceps.shp")
Baeolophus_ridgwayi<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Baeolophus ridgwayi.shp")
Bonasa_umbellus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Bonasa umbellus.shp")
Branta_canadensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Branta canadensis.shp")
Callipepla_californica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Callipepla californica.shp")
Callipepla_gambelii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Callipepla gambelii.shp")
Callipepla_squamata<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Callipepla squamata.shp")
Cardinalis_cardinalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cardinalis cardinalis.shp")
Cathartes_aura<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cathartes aura.shp")
Catharus_fuscescens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Catharus fuscescens.shp")
Catharus_ustulatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Catharus ustulatus.shp")
Columbina_passerina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Columbina passerina.shp")
Corvus_brachyrhynchos<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Corvus brachyrhynchos.shp")
Cyanocitta_stelleri<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Cyanocitta stelleri.shp")
Dryocopus_pileatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Dryocopus pileatus.shp")
Eremophila_alpestris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Eremophila alpestris.shp")
Geothlypis_tolmiei<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Geothlypis tolmiei.shp")
Grus_canadensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Grus canadensis.shp")
Haemorhous_mexicanus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Haemorhous mexicanus.shp")
Icteria_virens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icteria virens.shp")
Icterus_bullockii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icterus bullockii.shp")
Icterus_galbula<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icterus galbula.shp")
Limnothlypis_swainsonii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Limnothlypis swainsonii.shp")
Meleagris_gallopavo<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Meleagris gallopavo.shp")
Melospiza_georgiana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Melospiza georgiana.shp")
Molothrus_ater<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Molothrus ater.shp")
Myadestes_townsendi<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Myadestes townsendi.shp")
Passerina_caerulea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerina caerulea.shp")
Passerina_cyanea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerina cyanea.shp")
Peucaea_botterii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Peucaea botterii.shp")
Phalacrocorax_auritus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Phalacrocorax auritus.shp")
Phalaropus_tricolor<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Phalaropus tricolor.shp")
Pheucticus_ludovicianus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pheucticus ludovicianus.shp")
Picoides_albolarvatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Picoides albolarvatus.shp")
Picoides_nuttallii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Picoides nuttallii.shp")
Picoides_scalaris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Picoides scalaris.shp")
Picoides_villosus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Picoides villosus.shp")
Pinicola_enucleator<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pinicola enucleator.shp")
Poecile_atricapillus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Poecile atricapillus.shp")
Poecile_gambeli<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Poecile gambeli.shp")
Polioptila_melanura<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Polioptila melanura.shp")
Psaltriparus_minimus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Psaltriparus minimus.shp")
Pyrocephalus_rubinus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Pyrocephalus rubinus.shp")
Regulus_satrapa<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Regulus satrapa.shp")
Seiurus_aurocapilla<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Seiurus aurocapilla.shp")
Setophaga_caerulescens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga caerulescens.shp")
Setophaga_dominica<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga dominica.shp")
Setophaga_occidentalis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga occidentalis.shp")
Setophaga_virens<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Setophaga virens.shp")
Sialia_sialis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sialia sialis.shp")
Sitta_canadensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sitta canadensis.shp")
Spinus_psaltria<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spinus psaltria.shp")
Spizella_breweri<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spizella breweri.shp")
Spizella_passerina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spizella passerina.shp")
Spizelloides_arborea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spizelloides arborea.shp")
Sturnella_neglecta<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Sturnella neglecta.shp")
Tachycineta_thalassina<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Tachycineta thalassina.shp")
Thryomanes_bewickii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Thryomanes bewickii.shp")
Thryothorus_ludovicianus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Thryothorus ludovicianus.shp")
Toxostoma_curvirostre<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Toxostoma curvirostre.shp")
Vireo_gilvus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo gilvus.shp")
Vireo_griseus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo griseus.shp")
Vireo_huttoni<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo huttoni.shp")
Vireo_plumbeus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Vireo plumbeus.shp")
Zonotrichia_albicollis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Zonotrichia albicollis.shp")


###### Species Name mismatch
test <- birds_behrmann[birds_behrmann$SCINAME=="Carduelis flammea",]
writeOGR(obj=test, dsn= "." , layer="Carduelis flammea" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Passerculus henslowii",]
writeOGR(obj=test, dsn= "." , layer="Passerculus henslowii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Mareca americana",]
writeOGR(obj=test, dsn= "." , layer="Mareca americana" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Spatula discors",]
writeOGR(obj=test, dsn= "." , layer="Spatula discors" , driver= "ESRI Shapefile" )


test <- birds_behrmann[birds_behrmann$SCINAME=="Hylatomus pileatus",]
writeOGR(obj=test, dsn= "." , layer="Hylatomus pileatus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Grus canadensis",]
writeOGR(obj=test, dsn= "." , layer="Grus canadensis" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Icterus bullockiorum",]
writeOGR(obj=test, dsn= "." , layer="Icterus bullockiorum" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Nannopterum auritus",]
writeOGR(obj=test, dsn= "." , layer="Nannopterum auritus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Steganopus tricolor",]
writeOGR(obj=test, dsn= "." , layer="Steganopus tricolor" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Leuconotopicus albolarvatus",]
writeOGR(obj=test, dsn= "." , layer="Leuconotopicus albolarvatus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Dryobates nuttallii",]
writeOGR(obj=test, dsn= "." , layer="Dryobates nuttallii" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Dryobates scalaris",]
writeOGR(obj=test, dsn= "." , layer="Dryobates scalaris" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Leuconotopicus villosus",]
writeOGR(obj=test, dsn= "." , layer="Leuconotopicus villosus" , driver= "ESRI Shapefile" )

test <- birds_behrmann[birds_behrmann$SCINAME=="Passerella arborea",]
writeOGR(obj=test, dsn= "." , layer="Passerella arborea" , driver= "ESRI Shapefile" )


##################
Carduelis_flammea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Carduelis flammea.shp")
Acanthis_flammea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Acanthis flammea.shp")

Passerculus_henslowii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerculus henslowii.shp")

Mareca_americana<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Mareca americana.shp")

Spatula_discors<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Spatula discors.shp")

Hylatomus_pileatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Hylatomus pileatus.shp")

Grus_canadensis<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Grus canadensis.shp")

Icterus_bullockiorum<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Icterus bullockiorum.shp")

Nannopterum_auritus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Nannopterum auritus.shp")

Steganopus_tricolor<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Steganopus tricolor.shp")

Leuconotopicus_albolarvatus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Leuconotopicus albolarvatus.shp")

Dryobates_nuttallii<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Dryobates nuttallii.shp")

Dryobates_scalaris<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Dryobates scalaris.shp")

Leuconotopicus_villosus<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Leuconotopicus villosus.shp")

Passerella_arborea<- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/Passerella arborea.shp")

#################################
Carduelis_flammea<-spTransform(Carduelis_flammea, CRS( "+proj=longlat +datum=WGS84 +no_defs" )) # ??
Acanthis_flammea<-spTransform(Acanthis_flammea, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))

Agelaius_phoeniceus<-spTransform(Agelaius_phoeniceus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Aimophila_ruficeps<-spTransform(Aimophila_ruficeps, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Passerculus_henslowii<-spTransform(Passerculus_henslowii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Ammodramus_savannarum<-spTransform(Ammodramus_savannarum, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Amphispiza_bilineata<-spTransform(Amphispiza_bilineata, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Mareca_americana<-spTransform(Mareca_americana, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Spatula_discors<-spTransform(Spatula_discors, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Anthus_rubescens<-spTransform(Anthus_rubescens, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Auriparus_flaviceps<-spTransform(Auriparus_flaviceps, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Baeolophus_ridgwayi<-spTransform(Baeolophus_ridgwayi, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Bonasa_umbellus<-spTransform(Bonasa_umbellus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Branta_canadensis<-spTransform(Branta_canadensis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Callipepla_californica<-spTransform(Callipepla_californica, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Callipepla_gambelii<-spTransform(Callipepla_gambelii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Callipepla_squamata<-spTransform(Callipepla_squamata, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Cardinalis_cardinalis<-spTransform(Cardinalis_cardinalis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Cathartes_aura<-spTransform(Cathartes_aura, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Catharus_fuscescens<-spTransform(Catharus_fuscescens, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Catharus_ustulatus<-spTransform(Catharus_ustulatus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Columbina_passerina<-spTransform(Columbina_passerina, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Corvus_brachyrhynchos<-spTransform(Corvus_brachyrhynchos, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Cyanocitta_stelleri<-spTransform(Cyanocitta_stelleri, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Hylatomus_pileatus<-spTransform(Hylatomus_pileatus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Eremophila_alpestris<-spTransform(Eremophila_alpestris, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Geothlypis_tolmiei<-spTransform(Geothlypis_tolmiei, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Grus_canadensis<-spTransform(Grus_canadensis, CRS( "+proj=longlat +datum=WGS84 +no_defs" )) ###### Not found
Haemorhous_mexicanus<-spTransform(Haemorhous_mexicanus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Icteria_virens<-spTransform(Icteria_virens, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Icterus_bullockiorum<-spTransform(Icterus_bullockiorum, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Icterus_galbula<-spTransform(Icterus_galbula, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Limnothlypis_swainsonii<-spTransform(Limnothlypis_swainsonii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Meleagris_gallopavo<-spTransform(Meleagris_gallopavo, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Melospiza_georgiana<-spTransform(Melospiza_georgiana, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Molothrus_ater<-spTransform(Molothrus_ater, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Myadestes_townsendi<-spTransform(Myadestes_townsendi, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Passerina_caerulea<-spTransform(Passerina_caerulea, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Passerina_cyanea<-spTransform(Passerina_cyanea, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Peucaea_botterii<-spTransform(Peucaea_botterii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Nannopterum_auritus<-spTransform(Nannopterum_auritus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Steganopus_tricolor<-spTransform(Steganopus_tricolor, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Pheucticus_ludovicianus<-spTransform(Pheucticus_ludovicianus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Leuconotopicus_albolarvatus<-spTransform(Leuconotopicus_albolarvatus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Dryobates_nuttallii<-spTransform(Dryobates_nuttallii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Dryobates_scalaris<-spTransform(Dryobates_scalaris, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Leuconotopicus_villosus<-spTransform(Leuconotopicus_villosus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Pinicola_enucleator<-spTransform(Pinicola_enucleator, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Poecile_atricapillus<-spTransform(Poecile_atricapillus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Poecile_gambeli<-spTransform(Poecile_gambeli, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Polioptila_melanura<-spTransform(Polioptila_melanura, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Psaltriparus_minimus<-spTransform(Psaltriparus_minimus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Pyrocephalus_rubinus<-spTransform(Pyrocephalus_rubinus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Regulus_satrapa<-spTransform(Regulus_satrapa, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Seiurus_aurocapilla<-spTransform(Seiurus_aurocapilla, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Setophaga_caerulescens<-spTransform(Setophaga_caerulescens, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Setophaga_dominica<-spTransform(Setophaga_dominica, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Setophaga_occidentalis<-spTransform(Setophaga_occidentalis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Setophaga_virens<-spTransform(Setophaga_virens, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Sialia_sialis<-spTransform(Sialia_sialis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Sitta_canadensis<-spTransform(Sitta_canadensis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Spinus_psaltria<-spTransform(Spinus_psaltria, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Spizella_breweri<-spTransform(Spizella_breweri, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Spizella_passerina<-spTransform(Spizella_passerina, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Passerella_arborea<-spTransform(Passerella_arborea, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Sturnella_neglecta<-spTransform(Sturnella_neglecta, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Tachycineta_thalassina<-spTransform(Tachycineta_thalassina, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Thryomanes_bewickii<-spTransform(Thryomanes_bewickii, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Thryothorus_ludovicianus<-spTransform(Thryothorus_ludovicianus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Toxostoma_curvirostre<-spTransform(Toxostoma_curvirostre, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Vireo_gilvus<-spTransform(Vireo_gilvus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Vireo_griseus<-spTransform(Vireo_griseus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Vireo_huttoni<-spTransform(Vireo_huttoni, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Vireo_plumbeus<-spTransform(Vireo_plumbeus, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
Zonotrichia_albicollis<-spTransform(Zonotrichia_albicollis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))


#################################################################
# PART 2:
# At this point there are now reprojected shapefiles for each species
# Now we need to get the NEON regions with overlap
#################################################################
# Need to export reprojected shapefiles for easy processing
list_species <- c("Accipiter_cooperii","Aix_sponsa","Anas_platyrhynchos","Aphelocoma_californica","Archilochus_alexandri","Archilochus_colubris","Ardea_alba","Ardea_herodias","Aythya_affinis","Aythya_americana","Baeolophus_bicolor","Baeolophus_inornatus","Bartramia_longicauda","Bombycilla_cedrorum","Bubulcus_ibis","Buteo_jamaicensis","Buteo_lineatus","Buteo_platypterus","Calamospiza_melanocorys","Calcarius_lapponicus","Calcarius_pictus","Calidris_alpina","Calidris_melanotos","Calidris_pusilla","Calypte_anna","Campylorhynchus_brunneicapillus","Cardellina_pusilla","Cardinalis_sinuatus","Catharus_guttatus","Catharus_minimus","Catherpes_mexicanus","Certhia_americana","Chaetura_pelagica","Chamaea_fasciata","Charadrius_vociferus","Chlidonias_niger","Chondestes_grammacus","Circus_cyaneus","Cistothorus_palustris","Cistothorus_platensis","Clangula_hyemalis","Coccyzus_americanus","Colaptes_auratus","Colaptes_chrysoides","Colinus_virginianus","Contopus_cooperi","Contopus_sordidulus","Contopus_virens","Corvus_corax","Corvus_cryptoleucus","Corvus_ossifragus","Cyanocitta_cristata","Dolichonyx_oryzivorus","Dumetella_carolinensis","Empidonax_alnorum","Empidonax_difficilis","Empidonax_hammondii","Empidonax_minimus","Empidonax_oberholseri","Empidonax_occidentalis","Empidonax_traillii","Empidonax_virescens","Empidonax_wrightii","Eudocimus_albus","Euphagus_cyanocephalus","Falco_sparverius","Gavia_pacifica","Geococcyx_californianus","Geothlypis_formosa","Geothlypis_philadelphia","Geothlypis_trichas","Haemorhous_cassinii","Haemorhous_purpureus","Helmitheros_vermivorum","Hirundo_rustica","Hylocichla_mustelina","Icterus_parisorum","Icterus_spurius","Ixoreus_naevius","Junco_hyemalis","Lanius_ludovicianus","Larus_delawarensis","Larus_hyperboreus","Loxia_curvirostra","Loxia_leucoptera","Melanerpes_carolinus","Melanerpes_erythrocephalus","Melanerpes_formicivorus","Melanerpes_uropygialis","Melospiza_lincolnii","Melospiza_melodia","Melozone_crissalis","Mimus_polyglottos","Mniotilta_varia","Myiarchus_cinerascens","Myiarchus_crinitus","Nucifraga_columbiana","Numenius_phaeopus","Nycticorax_nycticorax","Oreoscoptes_montanus","Parkesia_motacilla","Passer_domesticus","Passerculus_sandwichensis","Passerella_iliaca","Passerina_amoena","Passerina_ciris","Pelecanus_erythrorhynchos","Perisoreus_canadensis","Petrochelidon_pyrrhonota","Peucaea_aestivalis","Peucaea_carpalis","Peucaea_cassinii","Phainopepla_nitens","Phasianus_colchicus","Pheucticus_melanocephalus","Pica_hudsonia","Pipilo_chlorurus","Pipilo_erythrophthalmus","Pipilo_maculatus","Piranga_ludoviciana","Piranga_olivacea","Piranga_rubra","Plectrophenax_nivalis","Pluvialis_dominica","Poecile_hudsonicus","Polioptila_caerulea","Porzana_carolina","Progne_subis","Protonotaria_citrea","Quiscalus_major","Quiscalus_quiscula","Rallus_limicola","Regulus_calendula","Sayornis_nigricans","Sayornis_phoebe","Selasphorus_platycercus","Selasphorus_rufus","Setophaga_citrina","Setophaga_coronata","Setophaga_discolor","Setophaga_fusca","Setophaga_magnolia","Setophaga_nigrescens","Setophaga_pensylvanica","Setophaga_petechia","Setophaga_pinus","Setophaga_ruticilla","Setophaga_striata","Sialia_currucoides","Sialia_mexicana","Sitta_carolinensis","Sitta_pusilla","Sitta_pygmaea","Sphyrapicus_ruber","Sphyrapicus_varius","Spinus_lawrencei","Spinus_pinus","Spinus_tristis","Spiza_americana","Spizella_pallida","Spizella_pusilla","Stercorarius_longicaudus","Stercorarius_parasiticus","Sturnella_magna","Sturnus_vulgaris","Tachycineta_bicolor","Toxostoma_crissale","Toxostoma_rufum","Troglodytes_aedon","Turdus_migratorius","Tyrannus_forficatus","Tyrannus_tyrannus","Tyrannus_verticalis","Tyrannus_vociferans","Vireo_bellii","Vireo_cassinii","Vireo_flavifrons","Vireo_olivaceus","Vireo_solitarius","Vireo_vicinior","Xanthocephalus_xanthocephalus","Zenaida_asiatica","Zenaida_macroura","Zonotrichia_leucophrys")

list_rangemaps <- c(Accipiter_cooperii, Aix_sponsa, Anas_platyrhynchos, Aphelocoma_californica, Archilochus_alexandri, Archilochus_colubris, Ardea_alba, Ardea_herodias, Aythya_affinis, Aythya_americana, Baeolophus_bicolor, Baeolophus_inornatus, Bartramia_longicauda, Bombycilla_cedrorum, Bubulcus_ibis, Buteo_jamaicensis, Buteo_lineatus, Buteo_platypterus, Calamospiza_melanocorys, Calcarius_lapponicus, Calcarius_pictus, Calidris_alpina, Calidris_melanotos, Calidris_pusilla, Calypte_anna, Campylorhynchus_brunneicapillus, Cardellina_pusilla, Cardinalis_sinuatus, Catharus_guttatus, Catharus_minimus, Catherpes_mexicanus, Certhia_americana, Chaetura_pelagica, Chamaea_fasciata, Charadrius_vociferus, Chlidonias_niger, Chondestes_grammacus, Circus_cyaneus, Cistothorus_palustris, Cistothorus_platensis, Clangula_hyemalis, Coccyzus_americanus, Colaptes_auratus, Colaptes_chrysoides, Colinus_virginianus, Contopus_cooperi, Contopus_sordidulus, Contopus_virens, Corvus_corax, Corvus_cryptoleucus, Corvus_ossifragus, Cyanocitta_cristata, Dolichonyx_oryzivorus, Dumetella_carolinensis, Empidonax_alnorum, Empidonax_difficilis, Empidonax_hammondii, Empidonax_minimus, Empidonax_oberholseri, Empidonax_occidentalis, Empidonax_traillii, Empidonax_virescens, Empidonax_wrightii, Eudocimus_albus, Euphagus_cyanocephalus, Falco_sparverius, Gavia_pacifica, Geococcyx_californianus, Geothlypis_formosa, Geothlypis_philadelphia, Geothlypis_trichas, Haemorhous_cassinii, Haemorhous_purpureus, Helmitheros_vermivorum, Hirundo_rustica, Hylocichla_mustelina, Icterus_parisorum, Icterus_spurius, Ixoreus_naevius, Junco_hyemalis, Lanius_ludovicianus, Larus_delawarensis, Larus_hyperboreus, Loxia_curvirostra, Loxia_leucoptera, Melanerpes_carolinus, Melanerpes_erythrocephalus, Melanerpes_formicivorus, Melanerpes_uropygialis, Melospiza_lincolnii, Melospiza_melodia, Melozone_crissalis, Mimus_polyglottos, Mniotilta_varia, Myiarchus_cinerascens, Myiarchus_crinitus, Nucifraga_columbiana, Numenius_phaeopus, Nycticorax_nycticorax, Oreoscoptes_montanus, Parkesia_motacilla, Passer_domesticus, Passerculus_sandwichensis, Passerella_iliaca, Passerina_amoena, Passerina_ciris, Pelecanus_erythrorhynchos, Perisoreus_canadensis, Petrochelidon_pyrrhonota, Peucaea_aestivalis, Peucaea_carpalis, Peucaea_cassinii, Phainopepla_nitens, Phasianus_colchicus, Pheucticus_melanocephalus, Pica_hudsonia, Pipilo_chlorurus, Pipilo_erythrophthalmus, Pipilo_maculatus, Piranga_ludoviciana, Piranga_olivacea, Piranga_rubra, Plectrophenax_nivalis, Pluvialis_dominica, Poecile_hudsonicus, Polioptila_caerulea, Porzana_carolina, Progne_subis, Protonotaria_citrea, Quiscalus_major, Quiscalus_quiscula, Rallus_limicola, Regulus_calendula, Sayornis_nigricans, Sayornis_phoebe, Selasphorus_platycercus, Selasphorus_rufus, Setophaga_citrina, Setophaga_coronata, Setophaga_discolor, Setophaga_fusca, Setophaga_magnolia, Setophaga_nigrescens, Setophaga_pensylvanica, Setophaga_petechia, Setophaga_pinus, Setophaga_ruticilla, Setophaga_striata, Sialia_currucoides, Sialia_mexicana, Sitta_carolinensis, Sitta_pusilla, Sitta_pygmaea, Sphyrapicus_ruber, Sphyrapicus_varius, Spinus_lawrencei, Spinus_pinus, Spinus_tristis, Spiza_americana, Spizella_pallida, Spizella_pusilla, Stercorarius_longicaudus, Stercorarius_parasiticus, Sturnella_magna, Sturnus_vulgaris, Tachycineta_bicolor, Toxostoma_crissale, Toxostoma_rufum, Troglodytes_aedon, Turdus_migratorius, Tyrannus_forficatus, Tyrannus_tyrannus, Tyrannus_verticalis, Tyrannus_vociferans, Vireo_bellii, Vireo_cassinii, Vireo_flavifrons, Vireo_olivaceus, Vireo_solitarius, Vireo_vicinior, Xanthocephalus_xanthocephalus, Zenaida_asiatica, Zenaida_macroura, Zonotrichia_leucophrys)
# Birds Online Names
list_species <- c("Acanthis_flammea","Agelaius_phoeniceus","Aimophila_ruficeps","Passerculus_henslowii","Ammodramus_savannarum","Amphispiza_bilineata","Mareca_americana","Spatula_discors","Anthus_rubescens","Auriparus_flaviceps","Baeolophus_ridgwayi","Bonasa_umbellus","Branta_canadensis","Callipepla_californica","Callipepla_gambelii","Callipepla_squamata","Cardinalis_cardinalis","Cathartes_aura","Catharus_fuscescens","Catharus_ustulatus","Columbina_passerina","Corvus_brachyrhynchos","Cyanocitta_stelleri","Hylatomus_pileatus","Eremophila_alpestris","Geothlypis_tolmiei","Haemorhous_mexicanus","Icteria_virens","Icterus_bullockiorum","Icterus_galbula","Limnothlypis_swainsonii","Meleagris_gallopavo","Melospiza_georgiana","Molothrus_ater","Myadestes_townsendi","Passerina_caerulea","Passerina_cyanea","Peucaea_botterii","Nannopterum_auritus","Steganopus_tricolor","Pheucticus_ludovicianus","Leuconotopicus_albolarvatus","Dryobates_nuttallii","Dryobates_scalaris","Leuconotopicus_villosus","Pinicola_enucleator","Poecile_atricapillus","Poecile_gambeli","Polioptila_melanura","Psaltriparus_minimus","Pyrocephalus_rubinus","Regulus_satrapa","Seiurus_aurocapilla","Setophaga_caerulescens","Setophaga_dominica","Setophaga_occidentalis","Setophaga_virens","Sialia_sialis","Sitta_canadensis","Spinus_psaltria","Spizella_breweri","Spizella_passerina","Passerella_arborea","Sturnella_neglecta","Tachycineta_thalassina","Thryomanes_bewickii","Thryothorus_ludovicianus","Toxostoma_curvirostre","Vireo_gilvus","Vireo_griseus","Vireo_huttoni","Vireo_plumbeus","Zonotrichia_albicollis")

list_rangemaps <- c(Acanthis_flammea, Agelaius_phoeniceus, Aimophila_ruficeps, Passerculus_henslowii, Ammodramus_savannarum, Amphispiza_bilineata, Mareca_americana, Spatula_discors, Anthus_rubescens, Auriparus_flaviceps, Baeolophus_ridgwayi, Bonasa_umbellus, Branta_canadensis, Callipepla_californica, Callipepla_gambelii, Callipepla_squamata, Cardinalis_cardinalis, Cathartes_aura, Catharus_fuscescens, Catharus_ustulatus, Columbina_passerina, Corvus_brachyrhynchos, Cyanocitta_stelleri, Hylatomus_pileatus, Eremophila_alpestris, Geothlypis_tolmiei, Haemorhous_mexicanus, Icteria_virens, Icterus_bullockiorum, Icterus_galbula, Limnothlypis_swainsonii, Meleagris_gallopavo, Melospiza_georgiana, Molothrus_ater, Myadestes_townsendi, Passerina_caerulea, Passerina_cyanea, Peucaea_botterii, Nannopterum_auritus, Steganopus_tricolor, Pheucticus_ludovicianus, Leuconotopicus_albolarvatus, Dryobates_nuttallii, Dryobates_scalaris, Leuconotopicus_villosus, Pinicola_enucleator, Poecile_atricapillus, Poecile_gambeli, Polioptila_melanura, Psaltriparus_minimus, Pyrocephalus_rubinus, Regulus_satrapa, Seiurus_aurocapilla, Setophaga_caerulescens, Setophaga_dominica, Setophaga_occidentalis, Setophaga_virens, Sialia_sialis, Sitta_canadensis, Spinus_psaltria, Spizella_breweri, Spizella_passerina, Passerella_arborea, Sturnella_neglecta, Tachycineta_thalassina, Thryomanes_bewickii, Thryothorus_ludovicianus, Toxostoma_curvirostre, Vireo_gilvus, Vireo_griseus, Vireo_huttoni, Vireo_plumbeus, Zonotrichia_albicollis)

# Testing # # # # # # # # 
# list_species <- c("Accipiter_cooperii","Aix_sponsa")
# list_specID <- c("V2","V5")
# list_rangemaps <- c(Accipiter_cooperii, Aix_sponsa)
# # # # # # # # # # # # # 

# Write Reprojected Shapefiles for each species 
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/")

for(k in 1:length(list_rangemaps)){ 
  rangemap <- list_rangemaps[[k]]
writeOGR(obj=rangemap, dsn=".", layer=paste0(list_species[k],"_reproject"), driver="ESRI Shapefile")
}
# # # # # # # # # # # # # #


#################################################################
NEON_Domains<- st_read("/Users/colinsweeney/Documents/Documents/PhD/NEON/Sites/GIS_Files/NEONDomains_0/NEON_Domains.shp")

# Testing # # # # # # # # 
# list_species <- c("Accipiter_cooperii","Aix_sponsa")

# list_rangemaps <- c(Accipiter_cooperii2, Aix_sponsa2)

# ID NEON Domains that overlap with species range maps. 
# 

sf::sf_use_s2(FALSE) # used to eliminate error message about spherical geometry.

################################################################################
# Create separate shapefiles of NEON domains that overlap with individual sp range maps. 
# # # # # # # # # # # # # #
for(k in 1:length(list_species)){ 
  rangemap <- st_read(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/",list_species[k],"_reproject.shp"))
  
  Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]
  st_write(Adash, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/",list_species[k],"NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))
}
# # # # # # # # # # # # # #



# MISC CODE to Test Individual Plots
# Zonotrichia_leucophrys2 <- readOGR("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/Zonotrichia_leucophrysNEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp")

# plot(Zonotrichia_leucophrys2)
#plot(rangemap$geometry)

#typeof(NEON_Domains)
#typeof(rangemap)

#rangemap <- list_rangemaps$geometry[1]
# Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]
# plot(Adash)
# 
# plot(list_rangemaps$geometry[1])
# 
# 
# 
# 
# plot(Adash$geometry)
# 
# `Accipiter.cooperii`
# V174

# p1<- ggplot() + 
#   geom_sf(data = Adash)+
#   geom_point(data = Data, aes(x=decimalLongitude, y=decimalLatitude, colour = `Poecile.gambeli`), size = 3)+
#   ggtitle("NEON Plots: Poecile_gambeli Subsetting NEON Domains")+
#   xlab("Longitude") + 
#   ylab("Latitude") + 
#   #scale_fill_binned(guide = guide_coloursteps(show.limits = TRUE, even.steps = FALSE))+
#   #scale_size(guide = "legend")+
#   scale_color_gradientn(colours = rainbow(6),
#                         limits=c(0, 50), na.value = "transparent")+ 
#   #scale_colour_viridis() +
#   theme(panel.background = element_rect(fill = 'white', colour = 'grey85'),
#         panel.border = element_rect(fill=NA, colour = "white", size=1),
#         axis.line = element_line(color = 'black', size=1.5),
#         plot.title = element_text(size=24, vjust=0, family="sans", hjust=0.5),
#         axis.text.x = element_text(colour='black',size=12),
#         axis.text.y = element_text(colour='black',size=12),
#         axis.title.x = element_text(colour='black',size=12),
#         axis.title.y = element_text(colour='black',size=15),
#         #axis.ticks = element_line(color = 'black', size=1.5),
#         axis.ticks.length=unit(0.25,"cm"),
#         legend.position="right",
#         legend.text=element_text(size=13),
#         legend.title=element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())
# 
# p3<-p1+ geom_sf(data = Poecile_gambeli2, , fill=NA, colour="grey3", alpha=2)


# ggsave(file=paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/Poecile_gambeli.jpeg"), p3, width=10,height=10, dpi=600)


################################################################################
# Create shapefiles of subsets of NEON domains that weren't created before (4 weird species)
################################################################################
library(rgdal)
# Fix Weird Shapefiles/those with issues
birds_behrmann <- readRDS("/Users/colinsweeney/Documents/Documents/GIS_General/Shapefiles/RangeMaps/BirdLifeRangeMaps/birds_behrmann.rds")
# Circus hudsonius AKA Circus cyaneus
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/")
test <- birds_behrmann[birds_behrmann$SCINAME == "Circus hudsonius", ] 

plot(test)

writeOGR(obj=test, dsn=".", layer="Circus hudsonius", driver="ESRI Shapefile")
Circus_hudsonius <- readOGR("Circus hudsonius.shp")

Circus_hudsonius<-spTransform(Circus_hudsonius, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))

setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/")
writeOGR(obj=Circus_hudsonius, dsn=".", layer=paste0("Circus_hudsonius","_reproject"), driver="ESRI Shapefile")

# Phalaropus fulicarius
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/")
test <- birds_behrmann[birds_behrmann$SCINAME == "Phalaropus fulicarius", ] 
writeOGR(obj=test, dsn=".", layer="Phalaropus fulicarius", driver="ESRI Shapefile", overwrite_layer=T)
Phalaropus_fulicarius <- readOGR("Phalaropus fulicarius.shp")

Phalaropus_fulicarius<-spTransform(Phalaropus_fulicarius, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))

setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/")

writeOGR(obj=Phalaropus_fulicarius, dsn=".", layer=paste0("Phalaropus_fulicarius","_reproject"), driver="ESRI Shapefile")

# Zonotrichia atricapilla
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/")
test <- birds_behrmann[birds_behrmann$SCINAME == "Zonotrichia atricapilla", ] 
writeOGR(obj=test, dsn=".", layer="Zonotrichia atricapilla", driver="ESRI Shapefile", overwrite_layer=T)
Zonotrichia_atricapilla <- readOGR("Zonotrichia atricapilla.shp")

Zonotrichia_atricapilla<-spTransform(Zonotrichia_atricapilla, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))

setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/")

writeOGR(obj=Zonotrichia_atricapilla, dsn=".", layer=paste0("Zonotrichia_atricapilla","_reproject"), driver="ESRI Shapefile")

# Grus canadensis (?? Antigone canadensis )
setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/RangeMaps/Species_specific_rangemaps/")
test <- birds_behrmann[birds_behrmann$SCINAME == "Grus canadensis", ] 
test

test <- birds_behrmann[birds_behrmann$SCINAME == "Antigone canadensis", ] 

writeOGR(obj=test, dsn=".", layer="Antigone canadensis", driver="ESRI Shapefile")
Antigone_canadensis <- readOGR("Antigone canadensis.shp")

Antigone_canadensis<-spTransform(Antigone_canadensis, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))

setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/")

writeOGR(obj=Antigone_canadensis, dsn=".", layer=paste0("Antigone_canadensis","_reproject"), driver="ESRI Shapefile")


##########
library(sf)
library(rgdal)

NEON_Domains<- st_read("/Users/colinsweeney/Documents/Documents/PhD/NEON/Sites/GIS_Files/NEONDomains_0/NEON_Domains.shp")
NEON_Domains<-spTransform(NEON_Domains, CRS( "+proj=longlat +datum=WGS84 +no_defs" ))
plot(NEON_Domains)

# Circus hudsonius
rangemap <- st_read(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/","Circus_hudsonius","_reproject.shp"))

sf::sf_use_s2(FALSE) # used to eliminate error message about spherical geometry.

Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]

plot(Adash)

st_write(Adash, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Circus_hudsonius","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

C_hudsoniaus <- readOGR(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Circus_hudsonius","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

plot(C_hudsoniaus)

# Phalaropus fulicarius

rangemap <- st_read(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/","Phalaropus_fulicarius","_reproject.shp"))

Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]

st_write(Adash, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Phalaropus_fulicarius","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

Phalaropus_fulicarius <- readOGR(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Phalaropus_fulicarius","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

plot(Phalaropus_fulicarius)

# Zonotrichia atricapilla
rangemap <- st_read(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/","Zonotrichia_atricapilla","_reproject.shp"))

Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]

st_write(Adash, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Zonotrichia_atricapilla","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

Zonotrichia_atricapilla <- readOGR(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Zonotrichia_atricapilla","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

plot(Zonotrichia_atricapilla)

# Antigone canadensis
rangemap <- st_read(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/SpeciesReprojectedShapefiles/","Antigone_canadensis","_reproject.shp"))

Adash = NEON_Domains[lengths(st_intersects(NEON_Domains,rangemap))>0,]

st_write(Adash, paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Antigone_canadensis","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

Antigone_canadensis <- readOGR(paste0("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Subset_NEONDomain_SpRange_overlap/","Antigone_canadensis","NEONDomain_DistCorr_Super2017_20000_SpRangeOverlap.shp"))

plot(Antigone_canadensis)

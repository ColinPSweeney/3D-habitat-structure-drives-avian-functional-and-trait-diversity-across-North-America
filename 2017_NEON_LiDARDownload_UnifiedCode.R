################################################################################
### 2017 NEON LiDAR data collection
################################################################################

# Download all LAZ files directly from NEON 

# Load Libraries 
library(neonUtilities)
library(lidR)
library(dplyr)
library(rgdal)
library(sf)

######################### Download NEON LiDAR data #######################################
## Core
# Domain 1: HARV
Cord_HARV <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_HARV/BirdPlot_AllPoints_HARV.csv")
E_Harv<- Cord_HARV$easting # vector of cordinates
E_Harv

N_HARV<- Cord_HARV$northing # vector of cordinates
N_HARV

byTileAOP("DP1.30003.001", site="HARV", year="2017", check.size = T,
          easting=E_Harv, northing=N_HARV, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_HARV") #DP1.30003.001 is code for NEON LiDAR data. Since I used a 2 vectors for the easting and northing, this code downloads all tiles with spatial overlap for these points. You can specify a single point if need be or type out the string.

# Check data 
# Now that things are downloaded, check if you have all necessary files in the proper folder. 
# LAScatalog allows you to check files without loading large LiDAR files into R

# HARV
ctg_HARV <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_HARV/DP1.30003.001/neon-aop-products/2017/FullSite/D01/2017_HARV_4/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_Harv)
y <- c(N_HARV)
plot(ctg_HARV)
points(x, y)
############################################

# Domain 2: SCBI - point
# Domain 3: OSBS
Cord_OSBS <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_OSBS/BirdPlot_AllPoints_OSBS.csv")
E_OSBS<- Cord_OSBS$easting # vector of cordinates
E_OSBS

N_OSBS<- Cord_OSBS$northing # vector of cordinates
N_OSBS

byTileAOP("DP1.30003.001", site="OSBS", year="2017", check.size = T,
          easting=E_OSBS, northing=N_OSBS, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_OSBS") 

ctg_OSBS <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_OSBS/DP1.30003.001/neon-aop-products/2017/FullSite/D03/2017_OSBS_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_OSBS)
y <- c(N_OSBS)
plot(ctg_OSBS)
points(x, y)

# Domain 5: UNDE
Cord_UNDE <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_UNDE/BirdPlot_AllPoints_UNDE.csv")
E_UNDE<- Cord_UNDE$easting # vector of cordinates
N_UNDE<- Cord_UNDE$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="UNDE", year="2017", check.size = T,
          easting=E_UNDE, northing=N_UNDE, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_UNDE") 

ctg_UNDE <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_UNDE/DP1.30003.001/neon-aop-products/2017/FullSite/D05/2017_UNDE_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_UNDE)
y <- c(N_UNDE)
plot(ctg_UNDE)
points(x, y)


# Domain 6:KONZ
Cord_KONZ <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_KONZ/BirdPlot_AllPoints_KONZ.csv")
E_KONZ<- Cord_KONZ$easting # vector of cordinates
N_KONZ<- Cord_KONZ$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="KONZ", year="2017", check.size = T,
          easting=E_KONZ, northing=N_KONZ, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_KONZ") 

ctg_KONZ <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_KONZ/DP1.30003.001/neon-aop-products/2017/FullSite/D06/2017_KONZ_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_KONZ)
y <- c(N_KONZ)
plot(ctg_KONZ)
points(x, y)

# Domain 8:TALL
Cord_TALL <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_TALL/BirdPlot_AllPoints_TALL.csv")
E_TALL<- Cord_TALL$easting # vector of cordinates
N_TALL<- Cord_TALL$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="TALL", year="2017", check.size = T,
          easting=E_TALL, northing=N_TALL, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_TALL") 

ctg_TALL <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_TALL/DP1.30003.001/neon-aop-products/2017/FullSite/D08/2017_TALL_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_TALL)
y <- c(N_TALL)
plot(ctg_TALL)
points(x, y)

# Domain 9:WOOD
Cord_WOOD <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_WOOD/BirdPlot_AllPoints_WOOD.csv")
E_WOOD<- Cord_WOOD$easting # vector of cordinates
N_WOOD<- Cord_WOOD$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="WOOD", year="2017", check.size = T,
          easting=E_WOOD, northing=N_WOOD, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_WOOD") 

ctg_WOOD <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_WOOD/DP1.30003.001/neon-aop-products/2017/FullSite/D09/2017_WOOD_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_WOOD)
y <- c(N_WOOD)
plot(ctg_WOOD)
points(x, y)

# Domain 10:CPER
Cord_CPER <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_CPER/BirdPlot_AllPoints_CPER.csv")
E_CPER<- Cord_CPER$easting # vector of cordinates
N_CPER<- Cord_CPER$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="CPER", year="2017", check.size = T,
          easting=E_CPER, northing=N_CPER, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_CPER") 

ctg_CPER <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_CPER/DP1.30003.001/neon-aop-products/2017/FullSite/D10/2017_CPER_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_CPER)
y <- c(N_CPER)
plot(ctg_CPER)
points(x, y)

# Domain 11:CLBJ
Cord_CLBJ <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_CLBJ/BirdPlot_AllPoints_CLBJ.csv")
E_CLBJ<- Cord_CLBJ$easting # vector of cordinates
N_CLBJ<- Cord_CLBJ$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="CLBJ", year="2017", check.size = T,
          easting=E_CLBJ, northing=N_CLBJ, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_CLBJ") 

ctg_CLBJ <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_CLBJ/DP1.30003.001/neon-aop-products/2017/FullSite/D11/2017_CLBJ_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_CLBJ)
y <- c(N_CLBJ)
plot(ctg_CLBJ)
points(x, y)

# Domain 13:NIWO
Cord_NIWO <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_NIWO/BirdPlot_AllPoints_NIWO.csv")
E_NIWO<- Cord_NIWO$easting # vector of cordinates
N_NIWO<- Cord_NIWO$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="NIWO", year="2017", check.size = T,
          easting=E_NIWO, northing=N_NIWO, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_NIWO") 

ctg_NIWO <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_NIWO/DP1.30003.001/neon-aop-products/2017/FullSite/D13/2017_NIWO_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_NIWO)
y <- c(N_NIWO)
plot(ctg_NIWO)
points(x, y)

# Domain 14:SRER
Cord_SRER <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_SRER/BirdPlot_AllPoints_SRER.csv")
E_SRER<- Cord_SRER$easting # vector of cordinates
N_SRER<- Cord_SRER$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="SRER", year="2017", check.size = T,
          easting=E_SRER, northing=N_SRER, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_SRER") 

ctg_SRER <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_SRER/DP1.30003.001/neon-aop-products/2017/FullSite/D14/2017_SRER_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_SRER)
y <- c(N_SRER)
plot(ctg_SRER)
points(x, y)

# Domain 15:ONAQ
Cord_ONAQ <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D15_ONAQ/BirdPlot_AllPoints_ONAQ.csv")
E_ONAQ<- Cord_ONAQ$easting # vector of cordinates
N_ONAQ<- Cord_ONAQ$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="ONAQ", year="2017", check.size = T,
          easting=E_ONAQ, northing=N_ONAQ, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D15_ONAQ") 

ctg_ONAQ <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D15_ONAQ/DP1.30003.001/neon-aop-products/2017/FullSite/D15/2017_ONAQ_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_ONAQ)
y <- c(N_ONAQ)
plot(ctg_ONAQ)
points(x, y)

# Domain 17:SJER
Cord_SJER <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SJER/BirdPlot_AllPoints_SJER.csv")
E_SJER<- Cord_SJER$easting # vector of cordinates
N_SJER<- Cord_SJER$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="SJER", year="2017", check.size = T,
          easting=E_SJER, northing=N_SJER, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SJER") 

ctg_SJER <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SJER/DP1.30003.001/neon-aop-products/2017/FullSite/D17/2017_SJER_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_SJER)
y <- c(N_SJER)
plot(ctg_SJER)
points(x, y)

# Domain 18:TOOL - point


# Domain 19:BONA
Cord_BONA <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_BONA/BirdPlot_AllPoints_BONA.csv")
E_BONA<- Cord_BONA$easting # vector of cordinates
N_BONA<- Cord_BONA$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="BONA", year="2017", check.size = T,
          easting=E_BONA, northing=N_BONA, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_BONA") 

ctg_BONA <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_BONA/DP1.30003.001/neon-aop-products/2017/FullSite/D19/2017_BONA_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_BONA)
y <- c(N_BONA)
plot(ctg_BONA)
points(x, y)


## Relocatable
# Domain 1: BART
Cord_BART <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_BART/BirdPlot_AllPoints_BART.csv")
E_BART<- Cord_BART$easting # vector of cordinates
N_BART<- Cord_BART$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="BART", year="2017", check.size = T,
          easting=E_BART, northing=N_BART, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_BART") 

ctg_BART <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D01_BART/DP1.30003.001/neon-aop-products/2017/FullSite/D01/2017_BART_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_BART)
y <- c(N_BART)
plot(ctg_BART)
points(x, y)

# Domain 2: BLAN - point
# Domain 2: SERC - point
# Domain 3: DSNY
Cord_DSNY <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_DSNY/BirdPlot_AllPoints_DSNY.csv")
E_DSNY<- Cord_DSNY$easting # vector of cordinates
N_DSNY<- Cord_DSNY$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="DSNY", year="2017", check.size = T,
          easting=E_DSNY, northing=N_DSNY, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_DSNY") 

ctg_DSNY <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_DSNY/DP1.30003.001/neon-aop-products/2017/FullSite/D03/2017_DSNY_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_DSNY)
y <- c(N_DSNY)
plot(ctg_DSNY)
points(x, y)

# Domain 3: JERC
Cord_JERC <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_JERC/BirdPlot_AllPoints_JERC.csv")
E_JERC<- Cord_JERC$easting # vector of cordinates
N_JERC<- Cord_JERC$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="JERC", year="2017", check.size = T,
          easting=E_JERC, northing=N_JERC, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_JERC") 

ctg_JERC <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D03_JERC/DP1.30003.001/neon-aop-products/2017/FullSite/D03/2017_JERC_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_JERC)
y <- c(N_JERC)
plot(ctg_JERC)
points(x, y)

# Domain 5: STEI
Cord_STEI <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_STEI/BirdPlot_AllPoints_STEI.csv")
E_STEI<- Cord_STEI$easting # vector of cordinates
N_STEI<- Cord_STEI$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="STEI", year="2017", check.size = T,
          easting=E_STEI, northing=N_STEI, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_STEI") 

ctg_STEI <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_STEI/DP1.30003.001/neon-aop-products/2017/FullSite/D05/2017_CHEQ_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_STEI)
y <- c(N_STEI)
plot(ctg_STEI)
points(x, y)

# Domain 5: TREE - point

# Domain 6: UKFS
Cord_UKFS <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_UKFS/BirdPlot_AllPoints_UKFS.csv")
E_UKFS<- Cord_UKFS$easting # vector of cordinates
N_UKFS<- Cord_UKFS$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="UKFS", year="2017", check.size = T,
          easting=E_UKFS, northing=N_UKFS, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_UKFS") 

ctg_UKFS <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D06_UKFS/DP1.30003.001/neon-aop-products/2017/FullSite/D06/2017_UKFS_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_UKFS)
y <- c(N_UKFS)
plot(ctg_UKFS)
points(x, y)

# Domain 7: GRSM
Cord_GRSM <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D07_GRSM/BirdPlot_AllPoints_GRSM.csv")
E_GRSM<- Cord_GRSM$easting # vector of cordinates
N_GRSM<- Cord_GRSM$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="GRSM", year="2017", check.size = T,
          easting=E_GRSM, northing=N_GRSM, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D07_GRSM") 

ctg_GRSM <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D07_GRSM/DP1.30003.001/neon-aop-products/2017/FullSite/D07/2017_GRSM_3/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_GRSM)
y <- c(N_GRSM)
plot(ctg_GRSM)
points(x, y)

# Domain 8: DELA - point
# Domain 8: LENO - point
# Domain 9: DCFS - point
# Domain 9: NOGP - point

# Domain 10: RMNP
Cord_RMNP <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_RMNP/BirdPlot_AllPoints_RMNP.csv")
E_RMNP<- Cord_RMNP$easting # vector of cordinates
N_RMNP<- Cord_RMNP$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="RMNP", year="2017", check.size = T,
          easting=E_RMNP, northing=N_RMNP, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_RMNP") 

ctg_RMNP <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_RMNP/DP1.30003.001/neon-aop-products/2017/FullSite/D10/2017_RMNP_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_RMNP)
y <- c(N_RMNP)
plot(ctg_RMNP)
points(x, y)

# Domain 10: STER - point
# Domain 11: OAES
Cord_OAES <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_OAES/BirdPlot_AllPoints_OAES.csv")
E_OAES<- Cord_OAES$easting # vector of cordinates
N_OAES<- Cord_OAES$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="OAES", year="2017", check.size = T,
          easting=E_OAES, northing=N_OAES, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_OAES") 

ctg_OAES <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D11_OAES/DP1.30003.001/neon-aop-products/2017/FullSite/D11/2017_OAES_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_OAES)
y <- c(N_OAES)
plot(ctg_OAES)
points(x, y)

# Domain 13: MOAB
Cord_MOAB <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_MOAB/BirdPlot_AllPoints_MOAB.csv")
E_MOAB<- Cord_MOAB$easting # vector of cordinates
N_MOAB<- Cord_MOAB$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="MOAB", year="2017", check.size = T,
          easting=E_MOAB, northing=N_MOAB, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_MOAB") 

ctg_MOAB <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D13_MOAB/DP1.30003.001/neon-aop-products/2017/FullSite/D13/2017_MOAB_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_MOAB)
y <- c(N_MOAB)
plot(ctg_MOAB)
points(x, y)

# Domain 14: JORN
Cord_JORN <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_JORN/BirdPlot_AllPoints_JORN.csv")
E_JORN<- Cord_JORN$easting # vector of cordinates
N_JORN<- Cord_JORN$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="JORN", year="2017", check.size = T,
          easting=E_JORN, northing=N_JORN, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_JORN") 

ctg_JORN <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D14_JORN/DP1.30003.001/neon-aop-products/2017/FullSite/D14/2017_JORN_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_JORN)
y <- c(N_JORN)
plot(ctg_JORN)
points(x, y)

# Domain 16: ABBY - point
# Domain 17: SOAP - point
# Domain 17: TEAK
Cord_TEAK <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_TEAK/BirdPlot_AllPoints_TEAK.csv")
E_TEAK<- Cord_TEAK$easting # vector of cordinates
N_TEAK<- Cord_TEAK$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="TEAK", year="2017", check.size = T,
          easting=E_TEAK, northing=N_TEAK, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_TEAK") 

ctg_TEAK <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_TEAK/DP1.30003.001/neon-aop-products/2017/FullSite/D17/2017_TEAK_2/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_TEAK)
y <- c(N_TEAK)
plot(ctg_TEAK)
points(x, y)

# Domain 18: BARR
Cord_BARR <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_BARR/BirdPlot_AllPoints_BARR.csv")
E_BARR<- Cord_BARR$easting # vector of cordinates
N_BARR<- Cord_BARR$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="BARR", year="2017", check.size = T,
          easting=E_BARR, northing=N_BARR, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_BARR") 

ctg_BARR <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_BARR/DP1.30003.001/neon-aop-products/2017/FullSite/D18/2017_BARR_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_BARR)
y <- c(N_BARR)
plot(ctg_BARR)
points(x, y)

# Domain 19: DEJU
Cord_DEJU <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_DEJU/BirdPlot_AllPoints_DEJU.csv")
E_DEJU<- Cord_DEJU$easting # vector of cordinates
N_DEJU<- Cord_DEJU$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="DEJU", year="2017", check.size = T,
          easting=E_DEJU, northing=N_DEJU, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_DEJU") 

ctg_DEJU <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_DEJU/DP1.30003.001/neon-aop-products/2017/FullSite/D19/2017_DEJU_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_DEJU)
y <- c(N_DEJU)
plot(ctg_DEJU)
points(x, y)

# Domain 19: HEAL
Cord_HEAL <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_HEAL/BirdPlot_AllPoints_HEAL.csv")
E_HEAL<- Cord_HEAL$easting # vector of cordinates
N_HEAL<- Cord_HEAL$northing # vector of cordinates

byTileAOP("DP1.30003.001", site="HEAL", year="2017", check.size = T,
          easting=E_HEAL, northing=N_HEAL, savepath="//Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_HEAL") 

ctg_HEAL <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D19_HEAL/DP1.30003.001/neon-aop-products/2017/FullSite/D19/2017_HEAL_1/L1/DiscreteLidar/ClassifiedPointCloud")
x <- c(E_HEAL)
y <- c(N_HEAL)
plot(ctg_HEAL)
points(x, y)

################################################################################
# Now complete the same process but for single point sites ---------------------
################################################################################

######################## Point to Grid and LiDAR download ################################
## Core
# Domain 2: SCBI (17N)
SCBI_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "SCBI_points")
SCBI_points
plot(SCBI_points, axes = TRUE, cex.axis = 0.75)

SCBI_points <- spTransform(SCBI_points, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
SCBI_points
plot(SCBI_points, axes = TRUE, cex.axis = 0.75)

write.csv(SCBI_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI/BirdPoints_AllPoints_SCBI.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI/BirdPoints_AllPoints_SCBI.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI/BirdPoints_AllPoints_SCBI.csv")

############################################
# Domain 18: TOOL (UTM: 6N)

TOOL_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "TOOL_points")
TOOL_points
plot(TOOL_points, axes = TRUE, cex.axis = 0.75)

TOOL_points <- spTransform(TOOL_points, CRS("+proj=utm +zone=6 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
TOOL_points
plot(TOOL_points, axes = TRUE, cex.axis = 0.75)

write.csv(TOOL_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL/BirdPoints_AllPoints_TOOL.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL/BirdPoints_AllPoints_TOOL.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL/BirdPoints_AllPoints_TOOL.csv")

############################################
# Domain 02: BLAN (UTM: 17N)

BLAN_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "BLAN_points")
BLAN_points
plot(BLAN_points, axes = TRUE, cex.axis = 0.75)

BLAN_points <- spTransform(BLAN_points, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
BLAN_points
plot(BLAN_points, axes = TRUE, cex.axis = 0.75)

write.csv(BLAN_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN/BirdPoints_AllPoints_BLAN.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN/BirdPoints_AllPoints_BLAN.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN/BirdPoints_AllPoints_BLAN.csv")

############################################
# Domain 02: SERC (UTM: 18N)

SERC_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "SERC_points")
SERC_points
plot(SERC_points, axes = TRUE, cex.axis = 0.75)

SERC_points <- spTransform(SERC_points, CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
SERC_points
plot(SERC_points, axes = TRUE, cex.axis = 0.75)

write.csv(SERC_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC/BirdPoints_AllPoints_SERC.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC/BirdPoints_AllPoints_SERC.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC/BirdPoints_AllPoints_SERC.csv")

############################################
# Domain 05: TREE (UTM: 16N)

TREE_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "TREE_points")
TREE_points
plot(TREE_points, axes = TRUE, cex.axis = 0.75)

TREE_points <- spTransform(TREE_points, CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
TREE_points
plot(TREE_points, axes = TRUE, cex.axis = 0.75)

write.csv(TREE_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE/BirdPoints_AllPoints_TREE.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE/BirdPoints_AllPoints_TREE.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE/BirdPoints_AllPoints_TREE.csv")

############################################
# Domain 08: DELA (UTM: 16N)

DELA_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "DELA_points")
DELA_points
plot(DELA_points, axes = TRUE, cex.axis = 0.75)

DELA_points <- spTransform(DELA_points, CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
DELA_points
plot(DELA_points, axes = TRUE, cex.axis = 0.75)

write.csv(DELA_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA/BirdPoints_AllPoints_DELA.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA/BirdPoints_AllPoints_DELA.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA/BirdPoints_AllPoints_DELA.csv")

############################################
# Domain 08: LENO (UTM: 16N)

LENO_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "LENO_points")
LENO_points
plot(LENO_points, axes = TRUE, cex.axis = 0.75)

LENO_points <- spTransform(LENO_points, CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
LENO_points
plot(LENO_points, axes = TRUE, cex.axis = 0.75)

write.csv(LENO_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO/BirdPoints_AllPoints_LENO.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO/BirdPoints_AllPoints_LENO.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO/BirdPoints_AllPoints_LENO.csv")

############################################
# Domain 09: DCFS (UTM: 14N)

DCFS_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "DCFS_points")
DCFS_points
plot(DCFS_points, axes = TRUE, cex.axis = 0.75)

DCFS_points <- spTransform(DCFS_points, CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
DCFS_points
plot(DCFS_points, axes = TRUE, cex.axis = 0.75)

write.csv(DCFS_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS/BirdPoints_AllPoints_DCFS.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS/BirdPoints_AllPoints_DCFS.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS/BirdPoints_AllPoints_DCFS.csv")

############################################
# Domain 09: NOGP (UTM: 14N)

NOGP_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "NOGP_points")
NOGP_points
plot(NOGP_points, axes = TRUE, cex.axis = 0.75)

NOGP_points <- spTransform(NOGP_points, CRS("+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
NOGP_points
plot(NOGP_points, axes = TRUE, cex.axis = 0.75)

write.csv(NOGP_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP/BirdPoints_AllPoints_NOGP.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP/BirdPoints_AllPoints_NOGP.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP/BirdPoints_AllPoints_NOGP.csv")

############################################
# Domain 10: STER (UTM: 13N)

STER_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "STER_points")
STER_points
plot(STER_points, axes = TRUE, cex.axis = 0.75)

STER_points <- spTransform(STER_points, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
STER_points
plot(STER_points, axes = TRUE, cex.axis = 0.75)

write.csv(STER_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER/BirdPoints_AllPoints_STER.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER/BirdPoints_AllPoints_STER.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER/BirdPoints_AllPoints_STER.csv")

############################################
# Domain 16: ABBY (UTM: 10N)

ABBY_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "ABBY_points")
ABBY_points
plot(ABBY_points, axes = TRUE, cex.axis = 0.75)

ABBY_points <- spTransform(ABBY_points, CRS("+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
ABBY_points
plot(ABBY_points, axes = TRUE, cex.axis = 0.75)

write.csv(ABBY_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY/BirdPoints_AllPoints_ABBY.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY/BirdPoints_AllPoints_ABBY.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY/BirdPoints_AllPoints_ABBY.csv")

############################################
# Domain 17: SOAP (UTM: 11N)

SOAP_points <- readOGR(dsn = "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/NEON_point_conversion", layer = "SOAP_points")
SOAP_points
plot(SOAP_points, axes = TRUE, cex.axis = 0.75)

SOAP_points <- spTransform(SOAP_points, CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
SOAP_points
plot(SOAP_points, axes = TRUE, cex.axis = 0.75)

write.csv(SOAP_points, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP/BirdPoints_AllPoints_SOAP.csv")

temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP/BirdPoints_AllPoints_SOAP.csv")
head(temp)

radius <- 250
yPlus <- temp$coords.x2+radius
xPlus <- temp$coords.x1+radius
yMinus <- temp$coords.x2-radius
xMinus <- temp$coords.x1-radius
yOR <- temp$coords.x2
xOR <- temp$coords.x1

A1=cbind(xMinus,yPlus)  # NW corner (A1)
A2=cbind(xOR,yPlus)
A3=cbind(xPlus, yPlus)  # NE corner (A3)
B1=cbind(xMinus,yOR)
B2=cbind(xOR, yOR) # original Point
B3=cbind(xPlus,yOR)
C1=cbind(xMinus,yMinus) # SW corner (C1)
C2=cbind(xOR,yMinus)
C3=cbind(xPlus,yMinus)  # SE corner (C3)

pA1 <- A1 %>% as.data.frame(A1) %>% rename(easting=xMinus, northing=yPlus)
pA2 <- A2 %>% as.data.frame(A2) %>% rename(easting=xOR, northing=yPlus)
pA3 <- A3 %>% as.data.frame(A3) %>% rename(easting=xPlus, northing=yPlus)
pB1 <- B1 %>% as.data.frame(B1) %>% rename(easting=xMinus, northing=yOR)
pB2 <- B2 %>% as.data.frame(B2) %>% rename(easting=xOR, northing=yOR)
pB3 <- B3 %>% as.data.frame(B3) %>% rename(easting=xPlus, northing=yOR)
pC1 <- C1 %>% as.data.frame(C1) %>% rename(easting=xMinus, northing=yMinus)
pC2 <- C2 %>% as.data.frame(C2) %>% rename(easting=xOR, northing=yMinus)
pC3 <- C3 %>% as.data.frame(C3) %>% rename(easting=xPlus, northing=yMinus)

A1 <- A1 %>% as.data.frame(A1) %>% rename(easting_A1=xMinus, northing_A1=yPlus)
A2 <- A2 %>% as.data.frame(A2) %>% rename(easting_A2=xOR, northing_A2=yPlus)
A3 <- A3 %>% as.data.frame(A3) %>% rename(easting_A3=xPlus, northing_A3=yPlus)
B1 <- B1 %>% as.data.frame(B1) %>% rename(easting_B1=xMinus, northing_B1=yOR)
B2 <- B2 %>% as.data.frame(B2) %>% rename(easting_B2=xOR, northing_B2=yOR)
B3 <- B3 %>% as.data.frame(B3) %>% rename(easting_B3=xPlus, northing_B3=yOR)
C1 <- C1 %>% as.data.frame(C1) %>% rename(easting_C1=xMinus, northing_C1=yMinus)
C2 <- C2 %>% as.data.frame(C2) %>% rename(easting_C2=xOR, northing_C2=yMinus)
C3 <- C3 %>% as.data.frame(C3) %>% rename(easting_C3=xPlus, northing_C3=yMinus)

Grid=cbind(A1,A2,A3,B1,B2,B3,C1,C2,C3)
blah <- cbind(temp, Grid)
head(blah)

pGrid=rbind(pA1,pA2,pA3,pB1,pB2,pB3,pC1,pC2,pC3)
plot(pGrid)

write.csv(blah, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP/BirdPoints_AllPoints_SOAP.csv")

#######################################################################
# SCBI
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI/BirdPoints_AllPoints_SCBI.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="SCBI", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI") 

ctg_SCBI <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SCBI/DP1.30003.001/neon-aop-products/2017/FullSite/D02/2017_SCBI_2/L1/DiscreteLidar/ClassifiedPointCloud")

plot(ctg_SCBI)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# TOOL
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL/BirdPoints_AllPoints_TOOL.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="TOOL", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL") 

ctg_TOOL <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D18_TOOL/DP1.30003.001/neon-aop-products/2017/FullSite/D18/2017_TOOL_1/L1/DiscreteLidar/ClassifiedPointCloud")

plot(ctg_TOOL)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# BLAN
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN/BirdPoints_AllPoints_BLAN.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="BLAN", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN") 

ctg_BLAN <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_BLAN/DP1.30003.001/neon-aop-products/2017/FullSite/D02/2017_BLAN_2/L1/DiscreteLidar/ClassifiedPointCloud")

plot(ctg_BLAN)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# SERC
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC/BirdPoints_AllPoints_SERC.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="SERC", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC") 

ctg_SERC <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D02_SERC/DP1.30003.001/neon-aop-products/2017/FullSite/D02/2017_SERC_2/L1/DiscreteLidar/ClassifiedPointCloud")

plot(ctg_SERC)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# TREE
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE/BirdPoints_AllPoints_TREE.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="TREE", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE") 

ctg_TREE <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D05_TREE/DP1.30003.001/neon-aop-products/2017/FullSite/D05/2017_STEI_2/L1/DiscreteLidar/ClassifiedPointCloud") # TREE is part of STEI flightplath

plot(ctg_TREE)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# DELA
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA/BirdPoints_AllPoints_DELA.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="DELA", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA") 

ctg_DELA <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_DELA/DP1.30003.001/neon-aop-products/2017/FullSite/D08/2017_DELA_3/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_DELA)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# LENO
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO/BirdPoints_AllPoints_LENO.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="LENO", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO") 

ctg_LENO <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D08_LENO/DP1.30003.001/neon-aop-products/2017/FullSite/D08/2017_LENO_3/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_LENO)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# DCFS
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS/BirdPoints_AllPoints_DCFS.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="DCFS", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS") 

ctg_DCFS <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_DCFS/DP1.30003.001/neon-aop-products/2017/FullSite/D09/2017_WOOD_2/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_DCFS)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# NOGP
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP/BirdPoints_AllPoints_NOGP.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="NOGP", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP") 

ctg_NOGP <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D09_NOGP/DP1.30003.001/neon-aop-products/2017/FullSite/D09/2017_NOGP_2/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_NOGP)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# STER
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER/BirdPoints_AllPoints_STER.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="STER", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER") 

ctg_STER <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D10_STER/DP1.30003.001/neon-aop-products/2017/FullSite/D10/2017_STER_2/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_STER)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# ABBY
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY/BirdPoints_AllPoints_ABBY.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="ABBY", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY") 

ctg_ABBY <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D16_ABBY/DP1.30003.001/neon-aop-products/2017/FullSite/D16/2017_ABBY_1/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_ABBY)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)

#######################################################################
# SOAP
temp <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP/BirdPoints_AllPoints_SOAP.csv")
E<- temp$coords.x1 # vector of cordinates
E

N<- temp$coords.x2 # vector of cordinates
N

byTileAOP("DP1.30003.001", site="SOAP", year="2017", check.size = T, buffer = 250,
          easting=E, northing=N, savepath="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP") 

ctg_SOAP <- readLAScatalog("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_LiDAR/D17_SOAP/DP1.30003.001/neon-aop-products/2017/FullSite/D17/2017_SOAP_2/L1/DiscreteLidar/ClassifiedPointCloud") 

plot(ctg_SOAP)
points(E, N)
points(temp$easting_A1, temp$northing_A1)
points(temp$easting_A3, temp$northing_A3)
points(temp$easting_C1, temp$northing_C1)
points(temp$easting_C3, temp$northing_C3)






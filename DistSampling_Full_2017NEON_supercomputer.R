###########################################################
# Load Packages --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rjags)
library(jagsUI)
###########################################################
# Set WD --------------------------------------------------------------
# setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/Models")

###########################################################
# Load Data --------------------------------------------------------------
df<- read.csv("data/birds2017_AllPlotTypes_B2_FirstCount_SpeciesRank_NoNoct_250Distance_NoOverlap_covariateReconstruct.csv", header = T) # Code ready 2017 count data with covariates (filtered)

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

# create pivot table of plotID vs scientificName, summarizing clusterSize
# blah2 <- blah %>% group_by(plotID, scientificName)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% pivot_wider(names_from = scientificName, values_from = clusterSize)

# blah2 <- blah %>% group_by(plotNum, spec)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% pivot_wider(names_from = spec, values_from = clusterSize)

blah2 <- blah %>% group_by(plotNum, spec)%>% summarize(clusterSize = n()) %>% ungroup() %>% as.data.frame() %>% arrange(spec) %>% pivot_wider(names_from = spec, values_from = clusterSize)

blah2[is.na(blah2)] = 0 # asign zeros to all "NA" 
blah2 # check 
blah2<- blah2%>%arrange(plotNum) # put the plotNum in order

blah2<-as.data.frame(blah2)
row.names(blah2)<- blah2$plotID
blah2 <- blah2[,-1] # take off the plotNum label column 
sum(blah2)

# blah2<- blah2[ , order(names(blah2))] # order the columns by header (spec)

blah2<- as.matrix(blah2)
class(blah2) # should be a matrix
sum(blah2) # (number of individuals)
dim(blah2) # (number of sites by number of species)

##############################################################
# Data prep--------------------------------------------------------------

# Prepare Distance Calculated data
# Number of individuals detected per site
# ncap replaced by matrix "y"/test2 (sum of each species per plot)
# ncap <- table(df[,"siteNum"])        	# number of captures for each site
# ncap <- as.vector(ncap)         # make ncap into vector 

site <- df[!is.na(df[,"observerDistance"]),"plotNum"] # Site ID of each observation (1-451)
site <- df$plotNum				                # site index for all observations (repeated from above)

# site<- c(1,2,3,4,5,6) # TEST
# site <- df[!is.na(df[,"observerDistance"]),"plotID"] # TESTING 

delta <- 25/1000                   	# Distance bin width for rect. approx. (renamed to v down below)
B <- 250/1000                        # Upper bound (radius of survey site)
midpt <- seq(delta/2, B, delta)	# Make mid-points and chop up data

# db <- seq(0, B, delta) # breaks in distance catagories 

dclass <- df[,"observerDistance"] %/% delta  +1 # Convert distance to distance category (why +1?)
dclass <- dclass[!is.na(df[,"clusterSize"])] # shouldn't be needed..... 

nD <- length(midpt)           # Number of distance intervals

# y<-structure(df$clusterSize)	# total detentions by site and species: OLD nind<-length(dclass)	
# y<-structure(test2) # generated pivot table from excel 
y<-blah2 # TEST: See if this works now that it's from a r generated pivot table (unlike test2)

# nind=length(y)    					  # total number of observations
nind=sum(y)                   # sums number of individuals (clusters treated differently)

length(unique(df$plotID)) # 451 (452 includinging TOOL_007)
length(unique(df$siteID)) # 39

nsites=length(unique(df$plotID)) # 451 sites total 

# nsites=c(446, 447, 448, 449, 450, 451, 448, 450, 448, 450) ########## TEST #########

# Indexes 
nsp=length(unique(df$scientificName))	  # number of species in data set
nSeg = length(unique(df$plotID))	      # number of point counts (plots)

species<-df$spec				                # species index for all observations
# species<-sort(species) ########## TEST ##############
# species<-df$scientificName #TEST

# Intial Values
Nin=y+1	    					        # initial values for N. Previously: Nst <- ncap + 1
# Nmax=1200 		    			      # number above 97.5% of N for most abundant species

# Rename variables to match Sollman code terms. 
v<- delta			    			      # width of distance categories (Rename to match code)
nG<- nD   # Sollmann nG = K&R nD of distance categories: (original: nG <- length(dp)-1 )

pix<- ((2 * midpt) / (B * B)) * delta # % area covered by each category (delta= bin width)

######################
# Not used from Sollman et al 
# flock<-sbdata$flock				# augmented flock size matrix (Nmax by species) 

#####################
# Check the data to make sure lengths of site, dclass, nind, and species align
length(site) # site index for all observations
length(dclass) # distance class index for all observations
nind # number of individuals
length(species) # species index for all observations


######################################################
# Covariate data----------------------------------------------
# prepare covarate columns as individual vectors 

# each continuous vector is scaled
elevation <- as.vector(scale(df[,"elevation"])) #ecological
latitude <- as.vector(scale(df[,"decimalLatitude"])) #ecological

cloudcover<- as.vector(scale(df[,"startCloudCoverPercentage"])) #detection
airtemp <- as.vector(scale(df[,"observedAirTemp"])) #detection
wind <- as.vector(scale(df[,"kmPerHourObservedWindSpeed"])) #detection

# categorical ecological covariates
#  nlcdClass,  observedHabitat
# detectionMethod <- as.vector(scale(df[,"detectionMethod"])) #detection

identifiedBy <- factor(df[,"identifiedBy"]) #detection

########################################################
# Create covariate objects
OBSVAR<-as.matrix(cbind(cloudcover, airtemp, wind))			# extract observation covariates
CatOBSVAR<-as.matrix(cbind(identifiedBy))

VAR<-as.matrix(cbind(elevation, latitude)) # extract abundance covariates

# Model includes cloudcover, airtemp, wind, and identifiedBy as detection covariates
# Model includes elevation and latitude as environmental covariates
########################################################

########## All variabales ##########
#OBSVAR<-as.matrix(cbind(cloudcover, humid, airtemp, wind))			# extract observation covariates
#CatOBSVAR<-as.matrix(cbind(identifiedBy))

#VAR<-as.matrix(cbind(elevation, latitude)) # extract abundance covariates

######### TEST ONLY ################
#OBSVAR<-as.matrix(cbind(cloudcover))			# TEST
#VAR<-as.matrix(cbind(elevation))          # TEST

nVAR=dim(VAR)[2]	   			# number of covariates on abundance
nOBSVAR=dim(OBSVAR)[2]
nCatOBSVAR=dim(CatOBSVAR)[2]

##########################################################
# Bundle and summarize data set-----------------------------------
str(win.df_NEON <- list(spec=nsp,
                        species=species, 
                        nG=nG, 
                        pix=pix,
                        #B=B,
                        #delta=delta,
                        midpt=midpt,
                        nsites=nsites, 
                        nVAR=nVAR,
                        nOBSVAR=nOBSVAR,
                        nCatOBSVAR=nCatOBSVAR,
                        VAR = VAR,
                        OBSVAR=OBSVAR,
                        CatOBSVAR=CatOBSVAR,
                        dclass=dclass,
                        y=y,
                        nind=nind,
                        site=site) )

##################################################################################################
##################### JAGS model code for community DS model: NEON Adaptated ####################
cat(" # commented out just for the purposes of editing code (delete before running)
model{ 
  #################################### Priors #######################
  mu_s~dnorm(0,0.01) # part of asig: species specific covariate
  sig_s~dunif(0.001,100) # part of tau_s, which is part of asig: species specific covariate
  tau_s<-1/(sig_s*sig_s) # part of asig: species specific covariate
  
  mu_a~dnorm(0,0.01) # part of alpha: species specific covariate
  tau_a~dgamma(0.1,0.1)# part of sig_a, which is part of alpha: species specific covariate
  sig_a<-1/sqrt(tau_a) # part of alpha: species specific covariate
  
  r.N~dunif(0.001,100) # community hyperparamter: used in rho.N (has to be postive)
  
  ########################################################
  # community hyperpriors
  ########################################################
  
  ########   ############   ############
  for (k in 1:nVAR){
    mu_b[k]~dnorm(0,0.01)
    sig_b[k]<-1/sqrt(tau_b[k]) # This doesn't appear to be used anywhere??
    tau_b[k]~dgamma(0.1,0.1)
  }
  
  ########   ############   ############
  for (k in 1:nOBSVAR){ # NEW: Trying to rework bsig (Observation covariates): treat it like beta
    mu_bsig[k]~dnorm(0,0.01)
    tau_bsig[k]~dgamma(0.1,0.1)
    sig_bsig[k]<-1/sqrt(tau_bsig[k])
  }
  
  ########   ############   ############
  for(k in 1:nCatOBSVAR){ # NEW: for catagorical obs variable observer. 
    mu_csig[k]~dnorm(0,0.01) # Used in csig
    tau_csig[k]~dgamma(0.1,0.1) # Used in csig
    sig_csig[k]<-1/sqrt(tau_csig[k])
  }
  
  ########################################################
  ### draw species-specific parameters from hyperdistribution
  ########################################################
  
  ########   ############   ############
  for (s in 1:spec){
    for (k in 1:nVAR){
      beta[s,k]~dnorm(mu_b[k],tau_b[k])
    }
    
    ########   ############   ############
    # NEW Trying to rework bsig (Observation covariates): treat it like beta
    for (k in 1:nOBSVAR){ 
         bsig[s,k]~dnorm(mu_bsig[k],tau_bsig[k]) # community hyperparameter for detection model 
    }
    
    ########   ############   ############ 
    # NEW: Catagorical Observation covariates
    for(k in 1:nCatOBSVAR){ 
    csig[s,k]~dnorm(mu_csig[k], tau_csig[k])
    }
    
    ########   ############   ############
    asig[s] ~dnorm(mu_s, tau_s) # detection model species specific intercept
    
    ########   ############   ############
    alpha[s]~dnorm(mu_a,tau_a)  # ecological model species specific intercept
  }
  
#########################################################################################  
### Main HM - > parallel structure to Kery and Royle 
#########################################################################################
  

#########################################################################################
    for (s in 1:spec){ # New 
    
    for (j in 1:nsites){ # number of sites
    
      ########### linear model for detection (Detection model) #######
      sigma[s,j]<- exp(asig[s] + inprod(bsig[s,],OBSVAR[j,]) + inprod(csig[s,],CatOBSVAR[j,]) )
      
      # sigma[s,j]<- exp(asig[s] + inprod(bsig[s,],OBSVAR[j,])) # Old
      
      ### construct detection probabilities by distance class w/ half-normal model
      for(k in 1:nG){ # nG: number of distance intervauls (nG is db-1 in length)
        log(p[s,j,k])<- -midpt[k]*midpt[k]/(2*(sigma[s,j]*sigma[s,j])) # half-normal detection
        
        fc[s,j,k]<- p[s,j,k]*pix[k] # pix: proportion of area of each intervaul
        fct[s,j,k]<- fc[s,j,k]/sum(fc[s,j,1:nG])  # fsc -> fst
        
      }
      
      pcap[s,j]<-sum(fc[s,j,1:nG])  				 # overall detection probability
      
      ############ Part 2 of HM   ################
      y[j,s]~ dbin(pcap[s,j],N[j,s]) 
      
      ############ Part 3 of HM   ################
      N[j,s]~dpois(lambda.star[j,s]) 
      
        ########### linear model for abundance (Ecological model) #######
        log(lambda[j,s])<- alpha[s] + inprod(beta[s,],VAR[j,])

        rho.N[j,s]~dgamma(r.N,r.N) 
        
        lambda.star[j,s]<-lambda[j,s]*rho.N[j,s]
    }
  }
  
  for(i in 1:nind){
    
    ############ Part 1 of HM   ################
    dclass[i] ~ dcat(fct[species[i],site[i],1:nG]) 
    
    # distance class of individual distributed by catagorical distribution of binned probabilites
    # nG: number of distance bins
  }
  ################################################################################################
  ### monitor total abundance
  for (i in 1:spec){
    Nspec[i]<-sum(N[1:nsites,i])
  }
} 
    ",fill=TRUE, file="MultispeciesDist_2017NEON_ver1.txt")

################################## MCMC Settings #########################
# Inits --------------------------------------------------------------
# Initial values to start run at

inits<-function(){list(N=Nin,
                       # Shape paramters
                       mu_a=runif(1,0,1), # alpha
                       tau_a=runif(1,0,1), # alpha (has to be positive)
                       
                       mu_b=runif(nVAR,0,1), # beta
                       tau_b=runif(nVAR,0,1), # beta
                       
                       mu_bsig=runif(nOBSVAR,0,1), # NEW: bsig (OBSVAR)
                       tau_bsig=runif(nOBSVAR,0,1), # NEW: bsig (OBSVAR)
                       
                       mu_csig=runif(nCatOBSVAR,0,1), # NEW: Used in csig (CatOBSVAR)
                       tau_csig=runif(nCatOBSVAR,0,1), # NEW: Used in csig (CatOBSVAR)
                       
                       asig=runif(nsp,0,1), # OBSVAR
                       mu_s = runif(1, 0, 1), # asig (OBSVAR) ~ like alpha
                       sig_s=runif(1, 0, 1) )} # asig (OBSVAR) ~ like alpha

# Params --------------------------------------------------------------
# Params to save: list of parameters to save (variables of interest)

params<-c('asig', # species specific parameter, used in detection model
          'mu_s', # species specific shape parameter for asig (detection model )
          'sig_s', # species specific shape parameter for asig (detection model )
          'mu_a', # species specific shape parameter for alpha (ecological model)
          'alpha', # species specific covariate, ecological model (intercept)
          'sig_a', # species specific shape parameter for alpha (ecological model)
          'mu_b', # species specific shape parameter for beta (ecological model)-VAR
          'sig_b', # species specific: beta parameter(ecological model)-VAR
          'beta', # species specific covariate, ecological model
          'Nspec', # abundance for each species across all plots
          'N', # species abundance for each plot
          'r.N', # community hyper parameter
          'mu_bsig', # NEW
          'sig_bsig', # NEW
          'bsig' # MODIFIED # new format to match beta's format
)

# MCMC chain settings--------------------------------------------------------------
# ni: number of iterations in MCMC chain
# nb: burn in period
# nt: thinning
# nc: number of chains

# ni <- 52000   ;   nb <- 2000   ;   nt <- 5   ;   nc <- 3
ni <- 20000   ;   nb <- 2000   ;   nt <- 10   ;   nc <- 3

# TEST ONLY
#ni <- 300   ;   nb <- 200   ;   nt <- 2   ;   nc <- 3

###########################################################
# Make sure wd is correct--------------------------------------------------------------
# setwd("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistanceModel_Test/Models")
# Make sure the txt file is exported to the correct folder

###########################################################
# Run MCMC (Gibbs Sampler) in JAGS --------------------------------------------------------------
##### Run JAGS and summarize posteriors.
MultiSp_FullTest_2 <- jags(win.df_NEON, 
                           inits, 
                           params, 
                           "MultispeciesDist_2017NEON_ver1.txt", 
                           n.thin=nt,
                           n.chains=nc, 
                           n.burnin=nb, 
                           n.iter=ni)

# MultiSp_FullTest_2 <- jags.basic(win.df_NEON, 
#                                   inits, 
#                                   params, 
#                                   "MultispeciesDist_2017NEON_ver1.txt", 
#                                   n.thin=nt,
#                                   n.chains=nc, 
#                                   n.burnin=nb, 
#                                   n.iter=ni)

saveRDS(MultiSp_FullTest_2, file="output/MultiSp_DistModel_2017_20000run.rds")

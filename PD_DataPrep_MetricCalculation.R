################################################################################
# Calculate PD and MPD using distance sampled abundances 
################################################################################
library(ape)
library(dplyr)

setwd("/Users/colinsweeney/Documents/Documents/Datasets/Phylogentics/BirdTree/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/")

# Load data --------------------------------------------------------------------
BirdTree <- read.tree("AllBirdsEricson1.tre") # 1000 random phylogentic trees
df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/2017_DistModel_Final/Biodiversity/FullData_spRich_combined_indvidualsp_distcorr_20000_sprangefilter_transposed_traits2.csv")

##############-----------------------------------------------------------------
# 1) Find the Scientific names for the missing species
# 2) RENAME Scientific Names to Match NEON Scientific Names

df2 <- df
df2$scientificName

# df2["scientificName"][df2["scientificName"] == ""] <- ""

df2["scientificName"][df2["scientificName"] == "Acanthis flammea"] <- "Carduelis flammea"
df2["scientificName"][df2["scientificName"] == "Ardea alba"] <- "Casmerodius albus"
df2["scientificName"][df2["scientificName"] == "Cardellina pusilla"] <- "Wilsonia pusilla"
df2["scientificName"][df2["scientificName"] == "Geothlypis formosa"] <- "Oporornis formosus"
df2["scientificName"][df2["scientificName"] == "Geothlypis philadelphia"] <- "Oporornis philadelphia"
df2["scientificName"][df2["scientificName"] == "Geothlypis tolmiei"] <- "Oporornis tolmiei"
df2["scientificName"][df2["scientificName"] == "Haemorhous cassinii"] <- "Carpodacus cassinii"
df2["scientificName"][df2["scientificName"] == "Haemorhous mexicanus"] <- "Carpodacus mexicanus"
df2["scientificName"][df2["scientificName"] == "Haemorhous purpureus"] <- "Carpodacus purpureus"
df2["scientificName"][df2["scientificName"] == "Ixoreus naevius"] <- "Zoothera naevia"
df2["scientificName"][df2["scientificName"] == "Melozone crissalis"] <- "Pipilo crissalis"
df2["scientificName"][df2["scientificName"] == "Parkesia motacilla"] <- "Seiurus motacilla"
df2["scientificName"][df2["scientificName"] == "Peucaea aestivalis"] <- "Aimophila aestivalis"
df2["scientificName"][df2["scientificName"] == "Peucaea botterii"] <- "Aimophila botterii"
df2["scientificName"][df2["scientificName"] == "Peucaea carpalis"] <- "Aimophila carpalis"
df2["scientificName"][df2["scientificName"] == "Peucaea cassinii"] <- "Aimophila cassinii"
df2["scientificName"][df2["scientificName"] == "Phalaropus tricolor"] <- "Steganopus tricolor"
df2["scientificName"][df2["scientificName"] == "Poecile atricapillus"] <- "Parus atricapillus"
df2["scientificName"][df2["scientificName"] == "Poecile gambeli"] <- "Parus gambeli"
df2["scientificName"][df2["scientificName"] == "Poecile hudsonicus"] <- "Parus hudsonicus"
df2["scientificName"][df2["scientificName"] == "Setophaga caerulescens"] <- "Dendroica caerulescens"
df2["scientificName"][df2["scientificName"] == "Setophaga citrina"] <- "Wilsonia citrina"
df2["scientificName"][df2["scientificName"] == "Setophaga coronata"] <- "Dendroica coronata"
df2["scientificName"][df2["scientificName"] == "Setophaga discolor"] <- "Dendroica discolor"
df2["scientificName"][df2["scientificName"] == "Setophaga dominica"] <- "Dendroica dominica"
df2["scientificName"][df2["scientificName"] == "Setophaga fusca"] <- "Dendroica fusca"
df2["scientificName"][df2["scientificName"] == "Setophaga magnolia"] <- "Dendroica magnolia"
df2["scientificName"][df2["scientificName"] == "Setophaga nigrescens"] <- "Dendroica nigrescens"
df2["scientificName"][df2["scientificName"] == "Setophaga occidentalis"] <- "Dendroica occidentalis"
df2["scientificName"][df2["scientificName"] == "Setophaga pensylvanica"] <- "Dendroica pensylvanica"
df2["scientificName"][df2["scientificName"] == "Setophaga petechia"] <- "Dendroica aestiva"
df2["scientificName"][df2["scientificName"] == "Setophaga pinus"] <- "Dendroica pinus"
df2["scientificName"][df2["scientificName"] == "Setophaga striata"] <- "Dendroica striata"
df2["scientificName"][df2["scientificName"] == "Setophaga virens"] <- "Dendroica virens"
df2["scientificName"][df2["scientificName"] == "Spinus lawrencei"] <- "Carduelis lawrencei"
df2["scientificName"][df2["scientificName"] == "Spinus pinus"] <- "Carduelis pinus"
df2["scientificName"][df2["scientificName"] == "Spinus psaltria"] <- "Carduelis psaltria"
df2["scientificName"][df2["scientificName"] == "Spinus tristis"] <- "Carduelis tristis"
df2["scientificName"][df2["scientificName"] == "Spizelloides arborea"] <- "Spizella arborea"

head(df[df$scientificName == "Spizelloides arborea",])
head(df2[df2$scientificName == "Spizella arborea",])# check to see if the numbers match

df3<-df2

df3$scientificName <- gsub(" ", "_", df3$scientificName) # replace spaces with "_" to match BirdTree

# head(df3)

################################################################################
# Calculate PD and MPD for each plot for each of the 100 trees------------------
################################################################################
# Create placeholder objects 
list_plotNum <- unique(df3$plotNum) # make a list of plotIDs 
#   head(BirdTree[[1]]$tip.label)
# length(df_temp$plotNum)

#
PD <- matrix(NA,length(list_plotNum),100) # Faith’s Phylogenetic Diversity
MPD <-matrix(NA,length(list_plotNum),100) # Mean Pairwise distance
#

for (k in 1:100){	
  tree <- BirdTree[[k]]
  
  for(i in 1:length(list_plotNum)){ # iterate through the list of each plot
  df_temp <- dplyr::filter(df3, plotNum== list_plotNum[i]) # pull out all observations for each plot
  df_temp <- df_temp[!is.na(df_temp$abundance),] # remove rows with NA for abundance
  df_temp <- dplyr::filter(df_temp, abundance>=0.95) # filter out abundances lower than 95%
  
  # # # # # # # # # # # # # #
  # Data Cleaning
  df_temp <- df_temp[!duplicated(df_temp$spec), ] # remove any duplicated species
  list_sp_names <- unique(df_temp$scientificName) # list of species per plot: used to subset trees
  
  if(length(list_sp_names)>1){
  # Create dataframe of single site presences to be used in 
  #temp <- c(rep(1,length(list_sp_names)))
  
  temp <- c(round(df_temp$abundance,0))
  temp2<- rbind(list_sp_names, temp)
  temp2<- as.data.frame(temp2)
  # temp2<- as.matrix(temp2)
  colnames(temp2) <- temp2[1,]
  temp2 <- temp2[-1, ] 
  
  # # # # # # # # # # #
  # Faith’s Phylogenetic Diversity: Sum of branch lengths
  pruned.tree<-keep.tip(tree, tree$tip.label[match(list_sp_names, tree$tip.label)])
  PD[i,k] <- sum(pruned.tree$edge.length) # Sum the branch length
  
  # # # # # # # # # # #
  # Mean pairwise distance: Average branch lengths between closest relatives
  #create a distance matrix based on branch lengths between species pairs
  dist.mat <- cophenetic.phylo(pruned.tree) 
  #dist.mat_2 <- round(cophenetic.phylo(pruned.tree),0)
  mpd.out <- mpd(temp2, dist.mat, abundance.weighted = F)
  # mpd.out2 <- mpd(temp2, dist.mat_2, abundance.weighted = T)
  # mean(dist.mat) # Assuming this is just the average of distance matrix
  
  MPD[i,k] <- mpd.out
  } 
  #else{
   # PD[i,k] <- 0
    #MPD[i,k] <- 0
    #}
  }
  
}

summary(PD)
PD2 <- PD
MPD2<- MPD
#calculate mean values across the 100 trees randomly selected from the 10000 in Upham.
PD_mean <- rowMeans(PD2[,1:100])
PD2<-cbind(PD2, PD_mean)
head(PD2)

MPD_mean <- rowMeans(MPD2[,1:100])
MPD2<-cbind(MPD2, MPD_mean)
head(MPD2)
###############################################################################

write.csv(MPD2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Phylogenetic/MPD_100trees_noweighting_95distsamp_rangefilter.csv")

write.csv(PD2, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Phylogenetic/FaithsPD_100trees_noweighting_95distsamp_rangefilter.csv")



###############################################################################
###############################################################################
###############################################################################
# examples
#data(phylocom)
#mpd(phylocom$sample, cophenetic(phylocom$phylo), abundance.weighted=TRUE)

# pruned.tree$

# plot(pruned.tree)
# ?drop.tip
# test <- as.character(list_sp_names[! list_sp_names %in% df3$scientificName])
# test
#############################
# Brook's Code

# for (i in 1:100){	
#   pdi <- phylotree[[i]]
#   
#   for (k in 1:length(poIDs)) {
#     comm <- subset(SpOcc, poId == poIDs[k])
#     nspec <- nrow(comm)
#     comm <- setDF(comm) # turn into data frame
#     sp.id <- pdi$tip.label # extract list of species
#     
#     rmTip <- as.character(sp.id[! sp.id %in% comm$sppPhy]) # Remove missing tips 
#     subTree <- drop.tip(pdi, tip=rmTip)  # remove those branches
#     
#     PD[k,i] <- sum(subTree$edge.length) # Sum the branch length
#     di <- evol.distinct(subTree, type = "fair.proportion",scale = FALSE, use.branch.lengths = TRUE)
#     PDSm[k,i] <- psych::geometric.mean(di[,2])
#     PDSi[[k]] <- di[,2]
#   }
#   
#   PDS[[i]] <- PDSi
# }


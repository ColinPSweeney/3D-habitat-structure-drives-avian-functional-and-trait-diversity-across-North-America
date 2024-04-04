###################################################################
# Chapter 1: PCoA analysis 
##################################################################
# Load packages
library(FD)
library(gawdis)
library(tidyverse)
library(funrar)
library(ape)
library(ggplot2)
library(ggrepel)
library(funspace)

################################################################################
# Load Data --------------------------------------------------------------------
df_test2 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/SpAbundances_byplot_df_test2.csv") # Plot x SpAbun: 448
trait_data3 <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/SpTraits_trait_data3.csv") # Sp x Traits: 260

df <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/Fall2023/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag.csv") # plot by plot values (indicies): 390

#df_old <- read.csv(file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_PCA_ratings.csv")

################################################################################
# Data prep: -------------------------------------------------------------------
# subset 448 plots to 390 plots (df start), Also remove 5 plots with NAs for PD values. 
df
df_B <- df[!is.na(df$SES.PD),] # remove 5 NA columns: 385
df_C <- df_B[,-c(1:2)] # clean up first two columns
length(df_C$plotID) # 385

# Fix abundance data -----------------------------------------------------------
# First, match with df_C to remove 5 NA plots. 
temp_plotIDs <- df_C%>%dplyr::select(plotID, plotNum) # 385: Full list of plotIDs 
df_test3 <- left_join(temp_plotIDs, df_test2, by="plotNum") # plot IDs added to species abundances
length(df_test3$plotNum) # 385
# Filter raw distance corrected abundances to 0.95 and higher abundances. 
df_test3[df_test3 < 0.95] <- 0 # Set all abundances less than 0.95 to zero
# Test data compatability
df_test3$plotID == df_B$plotID # test to see if they match up 
ncol(df_test3) # must match length(trait_data3$SciName) # 259 + 2 (plotID and plotNum) = 261
df_test3_subset <- df_test3[,3:261] # remove first two rows 
df_test3_subset # DATA: Subset species x abundance data: 95% adjusted and 

# Fix trait data: --------------------------------------------------------------
# Fix species names
trait_data4 <- trait_data3 %>% mutate_if(is.character, str_replace_all, ' ', '.') # Fix names to match df_test3
# Add Rownames
trait_data5 <- trait_data4[,-1] 
rownames(trait_data5) <- trait_data4[,1] # DATA: Subset trait data

# Data Check -------------------------------------------------------------------
rownames(trait_data5) == colnames(df_test3[,3:261]) # test to see if they match up 
colnames(df_test3_subset) == trait_data4$SciName # test to make sure you have all species. 

# END OF DATA PREP -------------------------------------------------------------

################################################################################
# PCoA -------------------------------------------------------------------------
################################################################################
# Datacheck with subset data ---------------------------------------------------
df_test3_subset # Subset abundance data: Plot x SpAbun
trait_data5 # Trait Data: Species x Trait

rownames(trait_data5) == colnames(df_test3_subset) # test to see if they match up 

# saveRDS(df_test3_subset, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/PCoA_July2023/df_test3_subset.rds")
# saveRDS(trait_data5, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/PCoA_July2023/trait_data5.rds")

# STEP 1: Calculate gower's distance -------------------------------------------
# Trait groups: Diet (10), ForStrat (8), BodyMass, Beak, Hand-wing Index
Trait.groups = c(rep(1, 10), rep(2, 8), 3, rep(4, 3), 5)
fuzzy.groups = c(1, 2, 4)  # Diet, ForStrat, Beak

gowdis.sp.sp <- gawdis(trait_data5, 
                          w.type = "optimized", 
                          groups = Trait.groups, 
                          fuzzy = fuzzy.groups, 
                          opti.maxiter = 300) #300 is the default
# Save output 
saveRDS(gowdis.sp.sp, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/PCoA_July2023/gowdis_SpSp_July2023.rds")

# STEP 2: Run PCoA -------------------------------------------------------------
pcoa_cmdscale <- cmdscale(gowdis.sp.sp, k=20, eig=T, add=T) # add=T : lingoes eig value correction 

# STEP 2.1: Determine the number of PC axes ------------------------------------
percent_explained <- pcoa_cmdscale$eig/sum(pcoa_cmdscale$eig)*100
plot(percent_explained)
sum(percent_explained[1:10]) # 39.98% 
percent_explained

# STEP 3: Extract vector info --------------------------------------------------
cmdscale.vector = envfit(ord=pcoa_cmdscale, env=trait_data5, permutations = 999, choices = c(1,2,3,4)) # ASSUMING THIS WORKS FOR PCOA
cmdscale.vector # Look at correlations

cmdscale.vector2 = data.frame(scores(cmdscale.vector, display = c('vectors')))
cmdscale.vector3 = cbind(cmdscale.vector2, trait.variables=rownames(cmdscale.vector2))
cmdscale.vector4 = cbind(cmdscale.vector3, pval=cmdscale.vector$vectors$pvals)

# Save output 
saveRDS(cmdscale.vector4, file="/Users/colinsweeney/Documents/Documents/PhD/Chapter1/PCoA/PCoA_July2023/cmdscale.vector4.rds")

# STEP 3.5: Create seperate vector data for each catagory of traits (Diet, Foraging, Morphological) ----------------------------------------------
cmdscale.vector_diet <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="Diet.Inv" | cmdscale.vector4$trait.variables=="Diet.Vend"| cmdscale.vector4$trait.variables=="Diet.Vect"| cmdscale.vector4$trait.variables=="Diet.Vfish"| cmdscale.vector4$trait.variables=="Diet.Scav"| cmdscale.vector4$trait.variables=="Diet.Fruit"| cmdscale.vector4$trait.variables=="Diet.Nect"| cmdscale.vector4$trait.variables=="Diet.Seed"| cmdscale.vector4$trait.variables=="Diet.PlantO"),] # remove Diet.Vunk

cmdscale.vector_morph <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="BodyMass.Value" | cmdscale.vector4$trait.variables=="Beak.Width"| cmdscale.vector4$trait.variables=="Beak.Depth"| cmdscale.vector4$trait.variables=="Beak.Length_Culmen"| cmdscale.vector4$trait.variables=="Hand.wing.Index"),]

cmdscale.vector_forage <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="ForStrat.watbelowsurf" | cmdscale.vector4$trait.variables=="ForStrat.wataroundsurf"| cmdscale.vector4$trait.variables=="ForStrat.ground"| cmdscale.vector4$trait.variables=="ForStrat.understory"| cmdscale.vector4$trait.variables=="ForStrat.midhigh"| cmdscale.vector4$trait.variables=="ForStrat.canopy"| cmdscale.vector4$trait.variables=="ForStrat.aerial"),] # remove PelagicSpecialist


# Create Unified Dataframe -----------------------------------------------------
pcoa_cmdscale.scores = data.frame(scores(pcoa_cmdscale)) # Convert to dataframe
length(pcoa_cmdscale.scores$Dim1) # 259, number of species

# Add family names -------------------------------------------------------------
AVONET_SpList <- read.csv("/Users/colinsweeney/Documents/Documents/PhD/Traits/AVONET/ELEData/TraitData/AVONET_Extant_Species_List.csv")
AVONET_SpList<- AVONET_SpList %>% mutate_if(is.character, str_replace_all, ' ', '.')
AVONET_SpList2 <- data.frame(Species.name=AVONET_SpList$Species.name, Order.name=AVONET_SpList$Order.name)

df_Sp_PCAxes <- data.frame(Species.name=rownames(pcoa_cmdscale.scores), PC1=pcoa_cmdscale.scores$Dim1, PC2=pcoa_cmdscale.scores$Dim2, PC3=pcoa_cmdscale.scores$Dim3, PC4=pcoa_cmdscale.scores$Dim4)
# head(df_Sp_PCAxes)
df_Sp_PCAxes2<- left_join(df_Sp_PCAxes, AVONET_SpList2, by="Species.name")%>%unique()
df_Sp_PCAxes3 = df_Sp_PCAxes2[!duplicated(df_Sp_PCAxes2$Species.name),]
length(df_Sp_PCAxes3$Species.name) # 259 (number of species should match with the start)

# PLOT -------------------------------------------------------------------------
###########################
# p1, p2, p3, p4, p5, p6
p2 <- ggplot2::ggplot(# gowdis.cwm.test.subset.pca.scores
  data=df_Sp_PCAxes3, aes(#x=PC1, y=PC2 
                          #, col=Order.name
                          x=PC3, y=PC4
                    # NewNLCD
                    # VarianceRating
                    # TotFragRating
                    #VegRating 
  )) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  # xlab("PCA1") +
  # ylab("PCA2") +
  xlab("PCA3") +
  ylab("PCA4") +
   xlim(-1, 1)+
   ylim(-1, 1)+
  # ylim(20, 15)+
  geom_point(colour = "white",
             #size  = 8,
             #stroke = 0,
             shape=16,
             #alpha = .3
  )+
  geom_segment(#data=cmdscale.vector_diet , # Trait loadings (blue)
    data=cmdscale.vector_morph , # Trait loadings (red)
    #data=cmdscale.vector_forage , # Trait loadings (green)
    aes(x=0, y=0, 
        #xend=Dim1, yend=Dim2
        xend=Dim3, yend=Dim4
        ), 
    #colour="darkblue", 
    colour="darkred",
    #colour="darkgreen",
    linewidth=1.5 #, 
    #arrow=arrow()
  ) +
   # geom_label_repel(#data=cmdscale.vector_diet, #(blue)
   #   #data=cmdscale.vector_morph, #(red)
   #   data=cmdscale.vector_forage, #(green)
   #   aes(#x=Dim1, y=Dim2, 
   #       x=Dim3, y=Dim4, 
   #       label=trait.variables),
   #   colour="grey33") +
  theme_classic() #+
#stat_ellipse() +
#ggforce::geom_mark_ellipse(aes(fill = Order.name,
#                               color = Order.name))+
#stat_density_2d(aes(fill = Order.name), geom = "polygon", colour="grey", alpha=0.25, bins=100, linewidth = 0.05)+
p1

ggsave(#"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Diet_labeled.png",
       #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Morph_labeled.png",
       "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Forage_labeled.png",
  plot = p1,
  # plot = last_plot(),
  width = 4, height = 4)

ggsave(#"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Diet_blank.png",
       #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Morph_blank.png",
       "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Forage_blank.png",
       plot = p1,
       # plot = last_plot(),
       width = 4, height = 4)

p2

#Labeled
ggsave(#"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Diet_labeled.png",
  #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Morph_labeled.png",
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Forage_labeled.png",
  plot = p2,
  # plot = last_plot(),
  width = 4, height = 4)
#Blank
ggsave(#"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Diet_blank.png",
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Morph_blank.png",
  #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Forage_blank.png",
  plot = p2,
  # plot = last_plot(),
  width = 4, height = 4)

# IN PROGRESS ------
###############################################################################
# STEP 3.5: Create seperate vector data for each catagory of traits (Diet, Foraging, Morphological) ----------------------------------------------
cmdscale.vector_diet_PC1PC2 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="Diet.Inv" | cmdscale.vector4$trait.variables=="Diet.Vfish"| cmdscale.vector4$trait.variables=="Diet.Seed"),]

cmdscale.vector_diet_PC3PC4 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="Diet.Vend"| cmdscale.vector4$trait.variables=="Diet.Vect"| cmdscale.vector4$trait.variables=="Diet.Scav"| cmdscale.vector4$trait.variables=="Diet.Fruit"| cmdscale.vector4$trait.variables=="Diet.Nect"| cmdscale.vector4$trait.variables=="Diet.PlantO"),]

#
cmdscale.vector_forage_PC1PC2 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="ForStrat.watbelowsurf" | cmdscale.vector4$trait.variables=="ForStrat.wataroundsurf"| cmdscale.vector4$trait.variables=="ForStrat.ground"| cmdscale.vector4$trait.variables=="ForStrat.midhigh"| cmdscale.vector4$trait.variables=="ForStrat.aerial"),]

cmdscale.vector_forage_PC3PC4 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="ForStrat.understory"| cmdscale.vector4$trait.variables=="ForStrat.canopy"),]

#
cmdscale.vector_morph_PC1PC2 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="BodyMass.Value"| cmdscale.vector4$trait.variables=="Beak.Length_Culmen"| cmdscale.vector4$trait.variables=="Hand.wing.Index"),]

cmdscale.vector_morph_PC3PC4 <-cmdscale.vector4[(cmdscale.vector4$trait.variables=="Beak.Width"| cmdscale.vector4$trait.variables=="Beak.Depth"),]

# ALTERNATIVE PLOT
p_test <- ggplot2::ggplot(# gowdis.cwm.test.subset.pca.scores
  data=df_Sp_PCAxes3, aes(
    x=PC1, y=PC2 
    #, col=Order.name
    #x=PC3, y=PC4
    # NewNLCD
    # VarianceRating
    # TotFragRating
    #VegRating 
  )) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
   xlab("PCA1") +
   ylab("PCA2") +
  #xlab("PCA3") +
  #ylab("PCA4") +
  xlim(-1, 1)+
  ylim(-1, 1)+
  # ylim(20, 15)+
  geom_point(#colour = "white",
             colour="grey",
             #size  = 8,
             #stroke = 0,
             shape=16,
             alpha = .3
  )+
  # NON SIGNIFICANT 
  geom_segment(
    #data=cmdscale.vector_diet_PC3PC4 , # Trait loadings (blue)
    #data=cmdscale.vector_morph_PC3PC4 , # Trait loadings (red)
    data=cmdscale.vector_forage_PC3PC4 , # Trait loadings (green)
    
    #data=cmdscale.vector_diet_PC1PC2 , # Trait loadings (blue)
    #data=cmdscale.vector_morph_PC1PC2 , # Trait loadings (red)
    #data=cmdscale.vector_forage_PC1PC2 , # Trait loadings (green)
    aes(x=0, y=0, 
        xend=Dim1, yend=Dim2
        #xend=Dim3, yend=Dim4
    ), 
    #colour="skyblue", 
    #colour="tomato1",
    colour="lightgreen",
    linewidth=1.5 #, 
    #arrow=arrow()
  ) +
  # SIGNIFICANT 
  geom_segment(
    #data=cmdscale.vector_diet_PC1PC2 , # Trait loadings (blue)
    #data=cmdscale.vector_morph_PC1PC2 , # Trait loadings (red)
    data=cmdscale.vector_forage_PC1PC2 , # Trait loadings (green)
    
    #data=cmdscale.vector_diet_PC3PC4 , # Trait loadings (blue)
    #data=cmdscale.vector_morph_PC3PC4 , # Trait loadings (red)
    #data=cmdscale.vector_forage_PC3PC4 , # Trait loadings (green)
    aes(x=0, y=0, 
        xend=Dim1, yend=Dim2
        #xend=Dim3, yend=Dim4
    ), 
    #colour="darkblue", 
    #colour="darkred",
    colour="darkgreen",
    linewidth=1.5 #, 
    #arrow=arrow()
  ) +
  
  # geom_label_repel(#data=cmdscale.vector_diet, #(blue)
  #   #data=cmdscale.vector_morph, #(red)
  #   data=cmdscale.vector_forage, #(green)
  #   aes(#x=Dim1, y=Dim2, 
  #       x=Dim3, y=Dim4, 
  #       label=trait.variables),
  #   colour="grey33") +
  theme_classic()

p_test
#############################################
# In Progress 2: Function 
# --------------------------------
PC_TraitGroups_Plot <-  function(Data, xlab, ylab, xmin, xmax, ymin, ymax, PtColor, NonSig_segment,NonSig_color,Sig_segment,Sig_color, xend, yend) {
  
  # Data Prep
  Data2 <- data.frame(dplyr::select(Data, xend+1, xend+1), dplyr::select(Data, yend+1, yend+1))%>% select(DimX=1, DimY=2)
  Sig_segment2 <- data.frame(dplyr::select(Sig_segment, xend, xend), dplyr::select(Sig_segment, yend, yend))%>% select(DimX=1, DimY=2)
  NonSig_segment2 <- data.frame(dplyr::select(NonSig_segment, xend, xend), dplyr::select(NonSig_segment, yend, yend))%>% select(DimX=1, DimY=2)
  
  # Plotting
  ggplot2::ggplot(data=Data2, aes(
    x=DimX, y=DimY
  )) +
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
    xlab(xlab) +
    ylab(ylab) +
    xlim(xmin, xmax)+
    ylim(ymin, ymax)+
    
    geom_point(colour = PtColor,
               shape=16,
               alpha = .3
    )+
    # NON SIGNIFICANT 
    geom_segment(
      data=NonSig_segment2,
      aes(x=0, y=0, 
          #xend=Dim1, yend=Dim2
          xend=DimX, yend=DimY
      ), 
      colour= NonSig_color,
      linewidth=1.5 #, 
      #arrow=arrow()
    ) +
    # SIGNIFICANT 
    geom_segment(
      data=Sig_segment2,
      aes(x=0, y=0, 
          #xend=Dim1, yend=Dim2
          xend=DimX, yend=DimY
      ), 
      colour=Sig_color,
      linewidth=1.5 #, 
      #arrow=arrow()
    ) +
    theme_classic()
}


NonSig_traits <- list(
               # For plotting PC1 and PC2
               cmdscale.vector_forage_PC3PC4 , # Trait loadings (green)
               cmdscale.vector_diet_PC3PC4 , # Trait loadings (blue)
               cmdscale.vector_morph_PC3PC4, # Trait loadings (red)
               # For plotting PC3 and PC4
               cmdscale.vector_forage_PC1PC2 , # Trait loadings (green)
               cmdscale.vector_diet_PC1PC2 , # Trait loadings (blue)
               cmdscale.vector_morph_PC1PC2 # Trait loadings (red)
               )

Sig_traits <- list(
                   # For plotting PC1 and PC2
                   cmdscale.vector_forage_PC1PC2 , # Trait loadings (green)
                   cmdscale.vector_diet_PC1PC2 , # Trait loadings (blue)
                   cmdscale.vector_morph_PC1PC2, # Trait loadings (red)
                   # For plotting PC3 and PC4
                   cmdscale.vector_forage_PC3PC4 , # Trait loadings (green)
                   cmdscale.vector_diet_PC3PC4 , # Trait loadings (blue)
                   cmdscale.vector_morph_PC3PC4 # Trait loadings (red)
               )

sig_colors <- c("darkgreen", #forage
                "darkblue", # diet
                "darkred", # morph
                "darkgreen", #forage
                "darkblue", # diet
                "darkred" # morph
                )
trait_type_names <- c("forage", "diet", "morph", "forage", "diet", "morph")
xlab_list <- c("PC1","PC1","PC1","PC3","PC3","PC3" )
ylab_list <- c("PC2","PC2","PC2","PC4","PC4","PC4")
xend_list <- c(1, 1, 1, 3, 3, 3)
yend_list <- c(2, 2, 2, 4, 4, 4)

for(i in 1:6){
  p_test <- PC_TraitGroups_Plot(Data=df_Sp_PCAxes3, 
                                xlab=xlab_list[i], 
                                ylab=ylab_list[i],
                                xend=xend_list[i],# Specify PC for X axis (should match xlab)
                                yend=yend_list[i], # Specify PC for Y axis (should match ylab)
                                xmin=-1, 
                                xmax=1, 
                                ymin=-1, 
                                ymax=1,
                                PtColor="white",
                                NonSig_segment= NonSig_traits[[i]],
                                NonSig_color="grey", # grey out
                                Sig_segment= Sig_traits[[i]],
                                Sig_color= sig_colors[i]
  )
  assign(paste0("p_", trait_type_names[i],"_",i), p_test)
  
}

#Plot
p_full <- ggpubr::ggarrange(p_forage_1, p_forage_4, p_diet_2,p_diet_5, p_morph_3,p_morph_6,
                  # labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 3,
                  common.legend = TRUE,
                  legend = "right")

ggsave("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC_FullPlot_blank.png",
       plot = p_full,
       # plot = last_plot(),
       width = 7, height = 9)

# End of In Progress
#############################################
# PC3 x PC4 
ggsave("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Diet_blank_B.png",
  #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Morph_blank_B.png",
  #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC3_PC4_Forage_blank_B.png",
  plot = p_test,
  # plot = last_plot(),
  width = 4, height = 4)

# PC1 x PC2
ggsave(#"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Diet_blank_B.png",
  #"/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Morph_blank_B.png",
  "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_Forage_blank_B.png",
  plot = p_test,
  # plot = last_plot(),
  width = 4, height = 4)

###############################################################################

# COMMUNITY Assemblage ----------------------------------------------------------
# Prepare community assembly matrix --------------------------------------------
# Need: sp by PC axes, and plot by SpAbund
pcoa_cmdscale.scores # Sp by PC axes 
df_test3 # with plotIDs and plotNum
df_test3_subset # plot by SpAbund

# 
output <- matrix(ncol=20, nrow=nrow(df_test3_subset))

for(i in 1:nrow(df_test3_subset)){
  temp_plot <- dplyr::slice(df_test3_subset, i) # pull a single plot by row
  
  for(k in 1:20){
    temp_Dim <- pcoa_cmdscale.scores[,k] # pull a single PC axis 
    temp_prod <- temp_plot*temp_Dim # product of abundance and PC axis score
    temp_prod_nonzero <- temp_prod[, colSums(temp_prod != 0) > 0] # remove absent species
    temp_mean <- rowMeans(temp_prod_nonzero[1,]) # Calculate abundance weighted mean of PC value per plot
    output[i,k] <- temp_mean
  }
}
# Create Unified Dataframe -----------------------------------------------------
output2 <- as.data.frame(cbind(plotID=df_test3$plotID,plotNum=df_test3$plotNum, as.data.frame(output)))
output3 <- select(output2, plotID, PC1=V1, PC2=V2, PC3=V3, PC4=V4)

df_D<- full_join(df_C, output3, by="plotID")

# Create Categories of Vegetation and Variance ---------------------------------
# Vegetation amount: 4 categories
df_D$VegRating <- ifelse(df_D$volume_und>0 & df_D$volume_mid==0, "Understory",
                           ifelse(df_D$volume_mid>0 & df_D$volume_subcan==0 , "Midstory",
                                  ifelse(df_D$volume_subcan>0 & df_D$volume_can == 0, "Subcanopy",
                                         ifelse(df_D$volume_can>0, "Canopy", "Other"))))

# Adjust NA variance values to 0 -----------------------------------------------
df_D["variance_plot"][is.na(df_D["variance_plot"])] <- 0

################################################################################
# Export Dataframe ------------------------------------------------------------
# This is the output used in Chapter1_FinalModels.R file 
write.csv(df_D, "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/July2023/NewPCoA/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_NEW_PCoA.csv")
################################################################################


# PLOT community data ------------------------------------------------------------------------
# p7, p8, p9, p10, p11, p12
test_plot <- ggplot2::ggplot(# gowdis.cwm.test.subset.pca.scores
  data=df_D, aes(x=PC1, y=PC2 #, col=Order.name
                          # NewNLCD
                          # VarianceRating
                          # TotFragRating
                          #,col=domainID
                          #,col=VegRating
  )) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  xlab("PCA1") +
  ylab("PCA2") +
  #geom_text(aes(label=plotID))+
  #ggforce::geom_mark_ellipse(aes(#fill = domainID,
  #                               color = domainID))+
  #stat_density_2d(aes(fill = domainID), geom = "polygon", colour="grey", alpha=0.25, bins=10)+
  stat_density_2d(aes(fill = VegRating), geom = "polygon", colour="grey", alpha=0.25, bins=100, linewidth = 0.05)+
  facet_wrap(vars(VegRating))+
   xlim(-1.6, 1)+
   ylim(-1.5, 1)+
  # ylim(20, 15)+
  geom_point(#colour = "black",
    #size  = 8,
    #stroke = 0,
    shape=16,
    alpha=0.05
    #alpha = .3
  )+
  #stat_ellipse() +
 geom_segment(data=cmdscale.vector_diet , # Trait loadings (blue)
   #data=cmdscale.vector_morph , # Trait loadings (red)
   #data=cmdscale.vector_forage , # Trait loadings (green)
   aes(x=0, y=0, xend=Dim1, yend=Dim2), 
   colour="darkblue", 
   #colour="darkred",
   #colour="darkgreen",
   linewidth=1 #, 
   #arrow=arrow()
 ) +
  geom_label_repel(data=cmdscale.vector_diet, #(blue)
    #data=cmdscale.vector_morph, #(red)
    #data=cmdscale.vector_forage, #(green)
    aes(x=Dim1, y=Dim2, label=trait.variables),
    colour="grey33") +
 theme_classic() +
 geom_point(#colour = "black",
   #size  = 8,
   #stroke = 0,
   shape=16,
   #alpha = .3
 )

test_plot

p7
p8
p9
p10
p11
# PC2 and PC3
p8

# Plot heatmap -----------------------------------------------------------------
# Veg Rating
# PC1 and PC2
test_funspace_input <- df_D%>%dplyr::select(PC1, PC2)
test_funspace_groupvec_input <- as.vector(df_D%>%dplyr::select(VegRating))

test_funspace <- funspace(test_funspace_input,
                          #group.vec=test_funspace_groupvec_input[,1], # THIS SHOULD WORK
                          group.vec=df_D$VegRating,
                          #test_funspace_input, 
                          PCs = c(PC1, PC2) #, 
                          #group.vec = VegRating
)

plot(test_funspace, 
     #type = "global",
     type = "groups",
     quant.plot = TRUE,
     quant = c(0.99, 0.75, 0.5, 0.25),
     quant.col = "grey22",
     quant.labels=F,
     pnt = T,
     pnt.cex =0.6,
     #pnt.col="black",
     pnt.col = rgb(0.001, .001, .001, alpha = 0.70),
     # pnt.col = rgb(0.1, 1, 1, alpha = 0.8),
     # colors= c("red", "orange","yellow","green", "blue", "violet"),
     colors= c("#1F8F82","#F0EBD5", "#D25828"),
     globalContour=T,
     globalContour.lwd=1.75,
     axis.title.x = "PC1",
     axis.title.y = "PC2")

# ggsave("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/PC1_PC2_DensityPlot.png",
#        plot = last_plot(),
#        width = 4, height = 4)

# PC3 and PC4
test_funspace_input_B <- df_D%>%dplyr::select(PC3, PC4)
test_funspace_groupvec_input_B <- as.vector(df_D%>%dplyr::select(VegRating))

test_funspace_B <- funspace(test_funspace_input_B,
                          #group.vec=test_funspace_groupvec_input[,1], # THIS SHOULD WORK
                          group.vec=df_D$VegRating,
                          #test_funspace_input, 
                          PCs = c(PC3, PC4) #, 
                          #group.vec = VegRating
)

plot(test_funspace_B, 
     #type = "global",
     type = "groups",
     quant.plot = TRUE,
     quant = c(0.99, 0.75, 0.5, 0.25),
     quant.col = "grey22",
     quant.labels=F,
     pnt = T,
     pnt.cex =0.6,
     #pnt.col="black",
     pnt.col = rgb(0.001, .001, .001, alpha = 0.70),
     # pnt.col = rgb(0.1, 1, 1, alpha = 0.8),
     # colors= c("red", "orange","yellow","green", "blue", "violet"),
     colors= c("#1F8F82","#F0EBD5", "#D25828"),
     globalContour=T,
     globalContour.lwd=1.75,
     axis.title.x = "PC3",
     axis.title.y = "PC4")

################################################################################

###############################################################################
# mFD_PCoA_DiagnosticPlots
###############################################################################

# MFD Package, Functional Diversity Diagnostics 
library(mFD)

# Load in Data -----------------------------------------------------------------
colnames(trait_data5) # Trait Data: Species x Trait
gowdis.sp.sp
#########################

# Trait groups: Diet (10), ForStrat (8), BodyMass, Beak, Hand-wing Index
Trait.groups = c(rep(1, 10), rep(2, 8), 3, rep(4, 3), 5)
fuzzy.groups = c(1, 2, 4)  # Diet, ForStrat, Beak

# Code Fuzzy traits and trait catagories ---------------------------------------
trait_cat_TEST <- as.data.frame(cbind(trait_name=colnames(trait_data5), trait_type=c(rep("F", 10), rep("F", 8), "Q", rep("F", 3), "Q"),fuzzy_name=c(rep("Diet", 10), rep("ForStrat", 8), NA, rep("Beak", 3), NA)))

# Summarize Traits -------------------------------------------------------------
traits_summ_TEST <- mFD::sp.tr.summary(
  tr_cat     = trait_cat_TEST,   
  sp_tr      = trait_data5, 
  stop_if_NA = TRUE)

traits_summ_TEST$tr_summary_fuzzy_list
traits_summ_TEST$tr_types

# Generate Gower's Distance Matrix ----------------------------------------------
sp_dist_GOWERS_TEST <- mFD::funct.dist(
  sp_tr         = trait_data5,
  tr_cat        = trait_cat_TEST,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# Run a PCoA -------------------------------------------------------------------
fspaces_gowdis.sp.sp_mad <- mFD::quality.fspaces(
  sp_dist             = gowdis.sp.sp,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")
fspaces_gowdis.sp.sp_rmsd <- mFD::quality.fspaces(
  sp_dist             = gowdis.sp.sp,
  maxdim_pcoa         = 10,
  deviation_weighting = "squared",
  fdist_scaling       = FALSE,
  fdendro             = "average")
fspaces_gowdis.sp.sp_mad_scaled <- mFD::quality.fspaces(
  sp_dist             = gowdis.sp.sp,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = TRUE,
  fdendro             = "average")
fspaces_gowdis.sp.sp_rmsd_scaled <- mFD::quality.fspaces(
  sp_dist             = gowdis.sp.sp,
  maxdim_pcoa         = 10,
  deviation_weighting = "squared",
  fdist_scaling       = TRUE,
  fdendro             = "average")

fspaces_GOWERS_TEST <- mFD::quality.fspaces(
  sp_dist             = sp_dist_GOWERS_TEST,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


# Test to see how close it is to other PCoAs -----------------------------------
cor(pcoa_cmdscale$points[,1], fspaces_gowdis.sp.sp$details_fspaces$sp_pc_coord[,1])

# Identify Quaility Axes -------------------------------------------------------
mFD::quality.fspaces.plot(
  #fspaces_quality            = fspaces_gowdis.sp.sp_mad,
  #quality_metric             = "mad",
  fspaces_quality            = fspaces_gowdis.sp.sp_rmsd,
  quality_metric             = "rmsd",
  #fspaces_quality            = fspaces_gowdis.sp.sp_mad_scaled,
  #quality_metric             = "mad_scaled",
  #fspaces_quality            = fspaces_gowdis.sp.sp_rmsd_scaled,
  #quality_metric             = "rmsd_scaled",
  fspaces_plot               = c("pcoa_1d", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  #fspaces_plot               = c("pcoa_6d","pcoa_7d", "pcoa_8d", "pcoa_9d", 
  #                               "pcoa_10d", "tree_average"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# OLD 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# IN PROGRESS ----------------------------------------------------------------
# Screeplot
#.   tibble(pe=cumsum(percent_explained), 
#.          axis=1:length(percent_explained))%>%
#.     ggplot(aes(x=axis, y=pe))+
#.     geom_line()+
#.     coord_cartesian(xlim=c(1,10))
#.   
#.   # plot the eigenvalues and interpret
#.   barplot(PCOA$values$Relative_eig) # Some negative, therefore correct 
#.   
#.   # STEP 2.5: Examine/Correct Eigen Values -----------------------------------------
#.   # Some distance measures may result in negative eigenvalues. In that case, add a correction:
#.   PCOA <- pcoa(gowdis.sp.sp, correction = "cailliez")
#.   barplot(PCOA$values$Corr_eig)
#.   
#.   sum((as.vector(PCOA$values$Corr_eig)/sum(PCOA$values$Corr_eig))[1:3])
#.   # Plot results
#.   biplot.pcoa(PCOA, trait_data5)
#.   View(PCOA)
#.   
#.   # STEP 3: Exctract PC Axes -----------------------------------------------------
#.   PCOAaxes <- PCOA$vectors[,c(1,2,3,4)]
#.   plot(PCOAaxes[,1], PCOAaxes[,2])
#.   plot(PCOAaxes[,2], PCOAaxes[,3])
#.   
#.   
#.   
#.   
#.   #
#.   barplot(as.vector(PCOA$values$Eigenvalues)/sum(PCOA$values$Eigenvalues))
#.   
#.   # Calculate the percent of variance explained by first two axes
#.   
#.   
#.   
#.   
#.   
#.   
#.   
#.   # STEP 2: Exctract PC Axes -----------------------------------------------------
#.   
#.   #################################################################################
#.   # Old code
#.   p_test <- PC_TraitGroups_Plot(Data=df_Sp_PCAxes3, 
#.                                 xlab="PC1", 
#.                                 ylab="PC2",
#.                                 xmin=-1, 
#.                                 xmax=1, 
#.                                 ymin=-1, 
#.                                 ymax=1,
#.                                 PtColor="white",
#.                                 NonSig_segment= 
#.                                   #cmdscale.vector_forage_PC3PC4 , # Trait loadings (green)
#.                                   cmdscale.vector_diet_PC3PC4 , # Trait loadings (blue)
#.                                 # cmdscale.vector_morph_PC3PC4 , # Trait loadings (red)
#.                                 NonSig_color= 
#.                                   # "lightgreen", # forage
#.                                   # "skyblue", # diet
#.                                   #"tomato1",  # morph
#.                                   "grey", # grey out
#.                                 
#.                                 Sig_segment= 
#.                                   #cmdscale.vector_forage_PC1PC2 , # Trait loadings (green)
#.                                   cmdscale.vector_diet_PC1PC2 , # Trait loadings (blue)
#.                                 # cmdscale.vector_morph_PC1PC2 , # Trait loadings (red)
#.                                 Sig_color= 
#.                                   # "darkgreen" #forage
#.                                   "darkblue" # diet
#.                                   # "darkred" # morph
#.   )
#.   
#.   p_test
#.   
#.   
#.   PC_TraitGroups_Plot_PC1PC2 <-  function(Data, xlab, ylab, xmin, xmax, ymin, ymax, PtColor, NonSig_segment,NonSig_color,Sig_segment,Sig_color) {
#.     ggplot2::ggplot(data=Data, aes(
#.       x=PC1, y=PC2 
#.       #, col=Order.name
#.       #x=PC3, y=PC4
#.       # NewNLCD
#.       # VarianceRating
#.       # TotFragRating
#.       #VegRating 
#.     )) +
#.       geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
#.       geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
#.       xlab(xlab) +
#.       ylab(ylab) +
#.       xlim(xmin, xmax)+
#.       ylim(ymin, ymax)+
#.       
#.       geom_point(colour = PtColor,
#.                  #colour="grey",
#.                  #size  = 8,
#.                  #stroke = 0,
#.                  shape=16,
#.                  alpha = .3
#.       )+
#.       # NON SIGNIFICANT 
#.       geom_segment(
#.         data=NonSig_segment,
#.         aes(x=0, y=0, 
#.             xend=Dim1, yend=Dim2
#.             #xend=Dim3, yend=Dim4
#.         ), 
#.         colour= NonSig_color,
#.         linewidth=1.5 #, 
#.         #arrow=arrow()
#.       ) +
#.       # SIGNIFICANT 
#.       geom_segment(
#.         data=Sig_segment,
#.         aes(x=0, y=0, 
#.             xend=Dim1, yend=Dim2
#.             #xend=Dim3, yend=Dim4
#.         ), 
#.         colour=Sig_color,
#.         linewidth=1.5 #, 
#.         #arrow=arrow()
#.       ) +
#.       theme_classic()
#.   }
#.   
#.   

################################################################################
# Density Plots 
# Create density plots for paper figure 
library(ggpubr)
library(ggplot2)

################################################################################
df_D <- read.csv( "/Users/colinsweeney/Documents/Documents/PhD/Chapter1/FinalData/July2023/NewPCoA/Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_NEW_PCoA.csv")
################################################################################


df_D_can <- df_D %>% filter(VegRating=="Canopy")
df_D_sub <- df_D %>% filter(VegRating=="Subcanopy")
df_D_mid <- df_D %>% filter(VegRating=="Midstory")
df_D_und <- df_D %>% filter(VegRating=="Understory")

p9 <- ggplot2::ggplot(
  #data=df_D,
  #rbind(data.frame(df_D, group="a"), data.frame(df_D_can, group="b")),
  #rbind(data.frame(df_D, group="a"), data.frame(df_D_sub, group="b")),
  rbind(data.frame(df_D, group="a"), data.frame(df_D_mid, group="b")),
  #rbind(data.frame(df_D, group="a"), data.frame(df_D_und, group="b")),
  aes(x=PC1, y=PC2 
      #x=PC3, y=PC4 
                 #, col=Order.name
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
  #xlab("PCA3") +
  #ylab("PCA4") +
  #ggtitle("Canopy")+
  #ggtitle("Subcanopy")+
  ggtitle("Midstory")+
  #ggtitle("Understory")+
  geom_density_2d(colour="grey23", alpha=1, bins=100, linewidth = 0.5)+
  stat_density_2d(aes(fill = group), 
                  geom = "polygon", 
                  colour="white", alpha=1, bins=100, linewidth = 0.05)+
  #facet_wrap(vars(VegRating))+
  
   #scale_fill_manual(values=c("a"="white", "b"="#F8766D")) + # Can
  #scale_fill_manual(values=c("a"="white", "b"="#7CAE00")) + # Sub
   scale_fill_manual(values=c("a"="white", "b"="#00BFC4")) + # Mid
  # scale_fill_manual(values=c("a"="white","b"="#C77CFF")) + # Und
  
  geom_point(#colour = "black",
    #size  = 8,
    #stroke = 0,
    shape=16,
    alpha=0.05
    #alpha = .3
  )+
  xlim(-1.75, 1)+
  ylim(-1.5, 1)+
  # ylim(20, 15)+
  
  theme_classic() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(#colour = "black",
    #size  = 8,
    #stroke = 0,
    shape=16,
    #alpha = .3
  )
plot_test
p7
p8
p9
p10
p11
p12
p13
p14

ggpubr::ggarrange(p7, p8, p9, p10,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")

ggpubr::ggarrange(p11, p12, p13, p14,
                  labels = c("E", "F", "G", "H"),
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")

p7 
p8
p9 + 
stat_density_2d(aes(fill = group), 
                geom = "polygon", 
                colour="grey", alpha=1, bins=100, linewidth = 0.05)+
  
p10

####################
# Plot Density plots by Function 
# Function 
densityPlot_subset <- function(main_data, subset_data, title, xlab, ylab, xmin, xmax, ymin, ymax, subset_color, subset_colorB, subset_colorC) {
  ggplot2::ggplot(
  data=main_data,
  aes(#x=PC1, y=PC2
     x=PC3, y=PC4 
  )) +
    
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
    
  xlab(xlab) +
  ylab(ylab) +
  ggtitle(title)+
    
  geom_density_2d(colour="grey60", alpha=1, bins=100, linewidth = 0.8)+
  
  xlim(xmin, xmax)+
  ylim(ymin, ymax)+
  
  theme_classic() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+

# ADD Overall Traitspace
#plot_test  +   
  stat_density_2d(data = main_data, 
                              fill="grey80",
                               geom = "polygon", 
                               colour="grey94", alpha=1, bins=100, linewidth = 0.3)+
  # ADD subset Traitspace (currently just midstory)
  geom_density_2d(data = subset_data, colour=subset_colorC, alpha=1, bins=100, linewidth = 0.8)+
  stat_density_2d(data = subset_data, 
                   fill=subset_color,
                   geom = "polygon", 
                   colour=subset_colorB, alpha=1, bins=100, linewidth = 0.3)+
  # Add points on top
  geom_point(data=main_data,
    colour = "grey29",
    size  = 1.25,
    #stroke = 0,
    shape=16,
    alpha=0.5
  )+
  geom_point(data=subset_data,
             colour = subset_color,
             fill = "black",
             size  = 1.5,
             stroke = 0.8,
             shape=21,
             alpha=0.78
  )
}

strata <- list(df_D_can, df_D_sub, df_D_mid, df_D_und)
titles <- c("Canopy","Subcanopy", "Midstory", "Understory")
colors_strata <- c("#F8766D","#7CAE00", "#00BFC4",  "#C77CFF")
colors_strataB <- c("#f2b5b1","#d5ed98", "#52f6fa",  "#d8b6f2")
colors_strataC <- c("darkred", "darkgreen", "darkblue", "purple4")
#colors_strataB <- c("#F8766D","#7CAE00", "#00BFC4",  "#C77CFF")
length(strata)
strata_names <- list("can", "sub", "mid", "und")

# Plot PC 1 and 2
for(i in 1:4){
  plot_test <- densityPlot_subset(
    main_data= df_D,
    subset_data= strata[[i]], 
    # X=df_D$PC1, 
    # Y=df_D$PC2, 
    title=titles[i], 
    xlab="PC1", 
    ylab="PC2", 
    xmin=-1.75, 
    xmax=1, 
    ymin=-1.5, 
    ymax=1, 
    subset_color=colors_strata[i], 
    subset_colorB=colors_strataB[i],
    subset_colorC=colors_strataC[i]
  )
  assign(paste0("p_", strata_names[i],"_A"), plot_test)
  
}

#Plot
ggpubr::ggarrange(p_can_A, p_sub_A, p_mid_A, p_und_A,
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")
# PC 3 and 4
for(i in 1:4){
  plot_test <- densityPlot_subset(
    main_data= df_D,
    subset_data= strata[[i]], 
    title=titles[i], 
    xlab="PC3", 
    ylab="PC4", 
    xmin=-1.75, 
    xmax=1, 
    ymin=-1.5, 
    ymax=1, 
    subset_color=colors_strata[i], 
    subset_colorB=colors_strataB[i],
    subset_colorC=colors_strataC[i]
  )
  assign(paste0("p_", strata_names[i],"_B"), plot_test)
  
}
ggpubr::ggarrange(p_can_B, p_sub_B, p_mid_B, p_und_B,
                  labels = c("E", "F", "G", "H"),
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")

# All 
ggpubr::ggarrange(p_can_A, p_can_B, p_sub_A, p_sub_B, p_mid_A, p_mid_B, p_und_A, p_und_B,
                  labels = c("a","e","b","f","c","g","d","h"),
                  ncol = 2, nrow = 4,
                  common.legend = TRUE,
                  legend = "right")

 ggsave("/Users/colinsweeney/Documents/Documents/PhD/Chapter1/Plots/MainModel_ParameterEstimateOutputs_Update_August2023/Full_DensityPlot_ggplot.png",
        plot = last_plot(),
        width = 5.5, height = 8)


######### In PROGRESS 
p7 + stat_density_2d(aes(fill = "b"), geom = "polygon", colour="#F8766D", alpha=0.25, bins=100, linewidth = 0.05)
?stat_density2d

p9 +  stat_density_2d(data=df_D_can, aes(fill = VegRating), 
                      geom = "polygon", 
                      colour="#F8766D", alpha=1, bins=100, linewidth = 0.05)
p9 +  stat_density_2d(data=df_D_sub, aes(fill = VegRating), 
                      geom = "polygon", 
                      colour="#7CAE00", alpha=1, bins=100, linewidth = 0.05)
p9 +  stat_density_2d(data=df_D_mid, aes(fill = VegRating), 
                      geom = "polygon", 
                      colour="#00BFC4", alpha=1, bins=100, linewidth = 0.05)
p9 +  stat_density_2d(data=df_D_und, aes(fill = VegRating), 
                      geom = "polygon", 
                      colour="#C77CFF", alpha=1, bins=100, linewidth = 0.05)

#title: ABCD Psychopathology and GMV PLS and CCA Figures

#load libraries
library(ggplot2) 
library(ggExtra)
library(Hmisc)
library(scales)
library(compare)
library(gdata)
library(mgcv)
library(mgcViz)
library(visreg)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(stats)

#read in data for plots 
FactorCorrs <- read.csv("plsr_psychopathology_loadings.csv", header = TRUE)
#BrainLoads <- read.csv("plsc_cortical_loadings.csv", header = TRUE)
#LVscores <- read.csv("PrimCCA_Group1_LVscores.csv", header = TRUE) #need for scatterplots of scores


#PLOTS OF FACTOR LOADINGS WITH LV

attach(FactorCorrs)

jpeg(filename = "plsr_psychchart3.jpeg", res = 300, width = 1500, height = 1300)

ggplot(FactorCorrs, aes(x = reorder(Factor, -comp3), y = comp3), x = Factor, y = comp3) +
  labs(x = "Psychopathology Dimension", y = "Loading on Component") +
  geom_bar(stat="identity", fill= c("#17b050", "#c00000", "#ed7d31", "#702fa0"), width = .8) + 
  #geom_errorbar(aes(ymin=llcorr, ymax=ulcorr), size=.4, width=.1, position=position_dodge(.9)) +
  coord_flip() +
  theme(axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        axis.text.x=element_text(size=10, colour="black"),
        axis.text.y=element_text(size=10, colour=c("#ed7d31","#17b050","#c00000","#702fa0")))
#opposite color order if needed: "#17b050","#c00000","#ed7d31","#702fa0"
#original order: red "#702fa0", green "#ed7d31", purple "#c00000", orange "#17b050"
dev.off()


#PLOTS OF BRAIN LOADINGS ON LV

attach(BrainLoads)

jpeg(filename = "plsr_brainchart4.jpeg", res = 100, width = 600, height = 900)

ggplot(BrainLoads, aes(x = factor(Region, levels = rev(levels(factor(Region)))), y = comp4)) +
  labs(x = "Brain Region", y = "Loading on Component") +
  geom_bar(stat="identity", fill="#0070C0", width = .8) + 
  #geom_errorbar(aes(ymin=ll, ymax=ul), size=.4, width=.3, position=position_dodge(.9)) +
  coord_flip() +
  theme(axis.title.x=element_text(size=16, colour = "black"),
        axis.title.y=element_text(size=16, colour = "black"),
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=8, colour="black")) 

dev.off()


#SCATTER PLOT OF BRAIN LV SCORES AND FACTOR LV SCORES 

#attach(LVscores)

#scatter <- ggplot(LVscores, aes(x=brain, y=factor)) +
#  geom_point(size=0.7, shape=19, col = black) +
#  labs(x = "LV Brain Scores", y = "LV Factor Scores")+
#  theme(axis.title.x=element_text(size=12, colour = "black"),
#        axis.title.y=element_text(size=12, colour = "black"),
#        axis.text.x=element_text(size=10, colour="black"),
#        axis.text.y=element_text(size=10, colour="black"))

#scattermarginals <- ggMarginal(scatter, type="density")

#ggsave(scattermarginals, file="PrimPLS_Group1_LVscores.jpeg", width = 5, height = 5, units = 'in', dpi = 300)


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
FactorCorrs <- read.csv("PrimPLS_Group1_FactorCorrs.csv", header = TRUE)
BrainLoads <- read.csv("PrimCCA_Group1_LV2BrainLoads.csv", header = TRUE)


#PLOTS OF FACTOR CORRELATIONS WITH LV

attach(FactorCorrs)

jpeg(filename = "PrimPLS_Group1_FactorCorrs.jpeg", res = 300, width = 1500, height = 1300)

ggplot(FactorCorrs, aes(x = reorder(Factor, -LV1), y = LV1), x = Factor, y = LV1) +
  labs(x = "Psychopathology Dimension", y = "Correlation with LV") +
  geom_bar(stat="identity", fill= c("#17b050", "#c00000", "#ed7d31", "#702fa0"), width = .8) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), size=.4, width=.1, position=position_dodge(.9)) +
  coord_flip() +
  theme(axis.title.x=element_text(size=12, colour = "black"),
        axis.title.y=element_text(size=12, colour = "black"),
        axis.text.x=element_text(size=10, colour="black"),
        axis.text.y=element_text(size=10, colour=c("#17b050", "#c00000", "#ed7d31", "#702fa0")))
#opposite color order if needed: "#702fa0", "#ed7d31", "#c00000", "#17b050"
dev.off()


#PLOTS OF BRAIN LOADINGS ON LV

attach(BrainLoads)

jpeg(filename = "PrimCCA_Group1_LV2BrainLoads.jpeg", res = 100, width = 600, height = 900)

ggplot(BrainLoads, aes(x = factor(Region, levels = rev(levels(factor(Region)))), y = LV1)) +
  labs(x = "Brain Region", y = "Loading on LV") +
  geom_bar(stat="identity", fill="#0070C0", width = .8) + 
  geom_errorbar(aes(ymin=LL, ymax=UL), size=.4, width=.3, position=position_dodge(.9)) +
  coord_flip() +
  theme(axis.title.x=element_text(size=16, colour = "black"),
        axis.title.y=element_text(size=16, colour = "black"),
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=8, colour="black")) 

dev.off()



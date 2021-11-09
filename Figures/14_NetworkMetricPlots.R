#Kayla Blincow
#4/24/2020

#Plotting Network Metrics

rm(list = ls())

#load libraries
library(tidyverse)

#load ze data
d <- read.table("NetworkMetrics_Centrality.txt", header = T)

#set levels for the SST classes
d$SSTClass <- factor(d$SSTClass, levels = c("Cool", "Norm", "Warm"))



#Violin plot of centrality values by SST Classification
#let's make those prettier/closer to what we might want as a final product
mycolors = c("blue", "grey20", "red")

p <- ggplot(d) +
  geom_violin(aes(x = SSTClass, y = CENTVAL5, fill = SSTClass, color = SSTClass),
              trim = F, alpha = 0.3) +
  geom_point(aes(x = SSTClass, y = CENTVAL5)) +
  scale_x_discrete(labels=c("Cool" = "Anomalously Cool",
                            "Norm" = "Normal",
                            "Warm" = "Anomalously Warm"
                            )) +
  labs(x = "SST Classification", y = "Degree Centrality (alpha = 0.5)") +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "none") 


png("NetworkVis/CentralityPlot.png", 
    units="in", 
    width=8, 
    height=7, 
    pointsize=12, 
    res=400)

suppressWarnings(print(p))

dev.off()



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



####OLD CODE####
#Exploratory Plots

#make ze plot!
#color by SST Classification, species on x axis
ggplot(d) +
  geom_point(aes(x = SP, y = CENTVAL5, shape = CENT, color = SSTClass),
             size = 4) + 
  theme_bw()

#SST classification on x axis (I like this more)--alpha = 0.5
ggplot(d) +
  geom_point(aes(x = SSTClass, y = CENTVAL5, shape = CENT, color = SP),
             size = 5) + 
  theme_bw()



#filter to only look at OUTgoing centrality
d2 <- filter(d, CENT == "OUT")
ggplot(d2) +
  geom_point(aes(x = SSTClass, y = CENTVAL75, color = SP),
             size = 5) + 
  theme_bw()

#boxplots?
ggplot(d) +
  geom_boxplot(aes(x = SSTClass, y = CENTVAL75, fill = CENT)) +
  theme_bw()

ggplot(d) +
  geom_boxplot(aes(x = SSTClass, y = CENTVAL1, fill = CENT))


#can i make on centrality metric by summing all the values?
d3 <- d %>% 
  group_by(SSTClass, CENT) %>% 
  summarize(CENTVAL5s = sum(CENTVAL5),
            CENTVAL75s = sum(CENTVAL75),
            CENTVAL1s = sum(CENTVAL1)) %>% 
  pivot_longer(CENTVAL5s:CENTVAL1s)

ggplot(d4) +
  geom_point(aes(x = SSTClass, y = value, shape = CENT, color = name), size = 5)+
  theme_bw()


#Brice likes the box plots best...
#let's make those prettier/closer to what we might want as a final product
ggplot(d) +
  geom_boxplot(aes(x = SSTClass, y = CENTVAL5), fill = "gray") + 
  scale_x_discrete(labels=c("Cool" = "Anomalously Cool",
                            "Warm" = "Anomalously Warm",
                            "Hot" = "Anomalously Warm")) +
  labs(x = "SST Classification", y = "Degree Centrality (alpha = 0.5)") +
  theme_classic() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14)) 
  

#Maybe I want something specifying the species??

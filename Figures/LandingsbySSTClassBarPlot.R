#Kayla Blincow
#4/29/2020

#Tuna explorations!
#There is some concern that since there is a variety of temperature preferences
#and life history strategies for different tuna species. So I am going to explore
#the landings of different species of tuna to see how it falls out.

#clear my workspace
rm(list = ls())

#load the libraries
library(tidyverse)
library(lubridate)
library(scales)
library(viridis)
library(shadowtext)

#load ze data!
d <- read.table("NO_daily_raw_3wysplt.txt", header = T)
#SST1 <- read.table("sst_SCB_rawd3.txt", header = T)
SST2 <- read.table("sst_SCB_new3.txt", header = T)
SST2 <- SST2[,c(1,3)]


# #join the temperature datasets
# SST <- right_join(SST1, SST2, by = "year")
# SST <- select(SST, year, overall_mean, overall_sd, SST, classi.y)
# names(SST)[7] <- "class"

#let's take a looksie...
#convert date to date object
d$Date <- as.Date(d$Date, format = "%Y-%m-%d")
d$year <- year(d$Date)
d$month <- month(d$Date)

#filter the data so we only have the months we care about, and create a column
#that has the total landings for that year
d <- filter(d, month > 2 & month < 11 & year > 1982) %>% 
  mutate(total = RF + KB + SB + BO + YT + AL + YF + BF,
         TU = AL + YF + BF)

#combine ze data!!!
dSST <- right_join(d, SST2, by = "year")

#tweak our dataframe a bit to make it easier to plot based on species
dSST2 <- pivot_longer(dSST, cols = c(RF:BF, TU), values_to = "Landings")

dSST2$classi <- factor(dSST2$classi, levels = c("cool",
                                                "norm",
                                                "warm"))
dSST2$name <- factor(dSST2$name, levels = c("RF", "KB", "SB", "BO", "YT", "AL",
                                            "YF", "BF", "TU"))

dSST3 <- dSST2 %>% 
  group_by(classi, name) %>% 
  summarize(Landing = sum(Landings))


#let's do some plot exploration!!

ggplot(dSST2) + 
  geom_bar(aes(fill = name, y = Landings, x = classi), 
           position = "stack", stat = "identity") +
  scale_fill_viridis_d(name = "Species") +
  theme_classic()
#ack! need to scale so norm isn't bigger than everything else, also remove TU
#until I need it


dSST3 %>% group_by(classi) %>% 
  summarize(total = sum(Landing))

dSST4 <- dSST3[dSST3$name!="TU",]
dSST5 <- dSST3[dSST3$name!="AL" & dSST3$name!="YF" & dSST3$name!="BF",]


dSST4 <- dSST4 %>% 
  group_by(classi) %>% 
  mutate(comp = Landing/sum(Landing))

dSST4$sp <- NA
for(i in 1:nrow(dSST4)){
  if(dSST4$name[i] == "BF" | dSST4$name[i] == "YF" |dSST4$name[i] == "AL"){
    dSST4$sp[i] <- "TU"
  } else {
    dSST4$sp[i] <- dSST4$name[i]
  }
  }

levels(dSST4$classi) <- c("Anomalously Cool", "Normal", "Anomalously Warm")

dSST4$name <- factor(dSST4$name, levels = c("Species", "RF", "KB", "SB", "BO",
                                            "YT", "", "Tunas", "BF", "YF", "AL"))

#I like this one more...
ggplot(dSST4) + 
  geom_col(aes(fill = name, y = comp, x = sp), 
           position = "stack") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "SST Classification", y = "Proportion of Total Landings") +
  scale_fill_manual(values = c("white", viridis(5, begin = 0, end = 0.5), 
                               "white", "white", viridis(3, begin = 0.7, 0.9)),
                    drop = F) +
  facet_wrap(~classi, strip.position = "bottom") +
  geom_hline(yintercept = 0) +
  guides(fill=guide_legend(ncol=1)) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.title=element_blank(),
        legend.text = element_text(size = 12)) 

#Going to change color scheme to match my other plots
#11/13/2020 Committee asked to label the bars themselves w/o the legend
#need to define the location for labels 
dSST4 <- dSST4 %>% 
  group_by(classi, sp) %>% 
  mutate(label_y = cumsum(comp))

p_legend <- ggplot(dSST4) + 
  geom_col(aes(fill = name, y = comp, x = sp, color = classi), 
           position = "stack") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "SST Classification", y = "Proportion of Total Landings") +
  scale_fill_manual(values = c("white", gray.colors(9)[1:5], 
                               "white", "white", 
                               gray.colors(9)[6:9]),
                    drop = F) +
  scale_color_manual(values = c("blue", "black", "red")) +
  facet_wrap(~classi, strip.position = "bottom") +
  geom_hline(yintercept = 0) +
  guides(fill=guide_legend(ncol=1), color = FALSE) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.title=element_blank(),
        legend.text = element_text(size = 12)) 

p_labels <- ggplot(dSST4) + 
  geom_col(aes(fill = name, y = comp, x = sp, color = classi), 
           position = "stack") +
  # geom_shadowtext(aes(y = label_y, x = sp, label = name), 
  #           vjust = 1.5, 
  #           bg.color = "black", 
  #           size = 3.5,
  #           color = c(rep("lightblue", 8), rep("gray85", 8),
  #                     rep("pink", 8))) +
  geom_text(aes(y = label_y, x = sp, label = name), vjust = 1.5) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "SST Classification", y = "Proportion of Total Landings") +
  scale_fill_manual(values = c("white", gray.colors(10)[2:6], 
                               "white", "white", 
                               gray.colors(10)[7:10]),
                    drop = F) +
  scale_color_manual(values = c("blue", "black", "red")) +
  facet_wrap(~classi, strip.position = "bottom") +
  geom_hline(yintercept = 0) +
  guides(fill=guide_legend(ncol=1), color = FALSE) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=14),
        legend.position = "none") 


png("LandingsBarPlot_labelsg.png", 
    units="in", 
    width=9, 
    height=7, 
    pointsize=12, 
    res=400)
suppressWarnings(print(p_labels))
dev.off()

png("LandingsBarPlot_legend.png", 
    units="in", 
    width=9, 
    height=7, 
    pointsize=12, 
    res=400)
suppressWarnings(print(p_legend))
dev.off()

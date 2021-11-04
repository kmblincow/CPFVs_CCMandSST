#Kayla Blincow
#6/25/2021
#UPDATE (11/3/2021) Adding inset larger context per reviewer request.

#Study Map

#clear my workspace
rm(list = ls())

#load packages
library(tidyverse)
library(maps)
library(ggmap)
library(mapdata)
library(ggsn)
library(ggspatial)
library(rgdal)
library(sp)
library(maptools)
library("rnaturalearth")
library("rnaturalearthdata")
library(cowplot)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


#load my data
d <- read.table("ZoneA_d.txt", header = T)
vessel <- read.table("Vessel_Class_complete.txt", header = T)

A1 <- left_join(d, vessel)

d2 <- filter(A1, Group == "NO")

#unique ports
ports <- unique(d2$PortCode) %>% 
  as.data.frame()
ports$name <- c("Oceanside", "San Diego", "Mission Bay", "Coronado",
                "", "Point Loma", "", "La Jolla", "Imperial Beach", 
                "Chula Vista", "National City")

ports$lat <- c(33.196, 32.672, 32.781, 32.686, NA, 32.713, NA,
               32.850, 32.584, 32.624, 32.678)
ports$long <- c(-117.380, -117.144, -117.243, -117.183, NA, -117.228,
                NA, -117.273, -117.113, -117.098, -117.098)

#fishing block tally
blocks <- d2 %>% 
  group_by(VesselID, LogDate, Block) %>% 
  summarize(trips = n_distinct()) %>% 
  mutate(trips = 1) %>% 
  group_by(Block) %>% 
  summarize(trips = sum(trips))
  
blocks_filter <- filter(blocks, trips > 100)

#make csv to input coordinates
#write.csv(blocks_filter, "blocks_map.csv")

#reload csv with coordinates
block <- read.csv("blocks_map.csv", header = T)



####Make Ze Maps!####

#larger context map
#pull base maps
USA <- map_data("worldHires", region = "USA")
Canada <- map_data("worldHires", region = "Canada")
Mexico <- map_data("worldHires", region = "Mexico")


#make a base map to build off of
base <- ggplot() +
  geom_polygon(data = USA, aes(x = long, y = lat, group = group),
               color = "black", fill = "gray80", size = 0.1) +
  geom_polygon(data = Canada, aes(x = long, y = lat, group = group),
               color = "black", fill = "gray80", size = 0.1) +
  geom_polygon(data = Mexico, aes(x = long, y = lat, group = group),
               color = "black", fill = "gray80", size = 0.1) +
  coord_fixed(xlim = c(-130, -70), ylim = c(20, 50), ratio = 1.2) +
  geom_segment(aes(x = -116.30, xend = -120.4716, y = 30.9705, yend = 30.9705),
               size = 0.1) +
  geom_segment(aes(x = -120.4716, xend = -120.4716, y = 30.9705, yend = 34.46),
               size = 0.1) +
  labs(x = "", y = "") +
  theme_nothing() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_rect(fill = "white"))


#make ze study map
study <- 
  ggplot() +
  geom_polygon(data = USA, aes(x = long, y = lat, group = group),
               color = "black", fill = "gray80") +
  geom_polygon(data = Mexico, aes(x = long, y = lat, group = group),
               color = "black", fill = "gray80") +
  coord_fixed(xlim = c(-122, -115), ylim = c(30, 36), ratio = 1.2) +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude", size = "Number of Trips")+
  geom_point(data = block, aes(x = long, y = lat, size = trips), 
             shape = 1, color = "gray40") +
  geom_point(data = ports, aes(x = long, y = lat), size = 2) +
  geom_point(aes(y = 32.127, x = -117.576), shape = 8) +
  geom_segment(aes(x = -116.30, xend = -120.4716, y = 30.9705, yend = 30.9705)) +
  geom_segment(aes(x = -120.4716, xend = -120.4716, y = 30.9705, yend = 34.46)) +
  scalebar(dist = 100, dist_unit = "km", x.min = -121.0, 
           x.max = -121.9, y.min = 30.25, y.max = 30.5, 
           location = "bottomleft",
           transform = TRUE, model = "WGS84", st.bottom = FALSE,
           st.dist = 0.5, st.size = 3) +
  annotation_north_arrow(height = unit(1, "cm"),
                         width = unit(0.75, "cm"),
                         pad_x = unit(1, "cm"),
                         pad_y = unit(1, "cm"),
                         location = "tr") +
  theme(panel.border = element_rect(colour = "black", fill=NA))



library(patchwork)
map_full <- study + inset_element(base, right = 0.6, bottom = 0.75, left = 0.25, 
                                  top = 0.98, align_to = 'full')


png(file="StudyMap_inset.png",
    width = 2000,
    height = 1500,
    res = 300)

map_full

dev.off()


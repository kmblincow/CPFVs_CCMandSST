#Vessel grouping 
#Original 1/31/2020

#The goal of this script is to classify each vessel in the full data set based
#on the most commonly caught species

#clear workspace
rm(list=ls())

#load necessary libraries
library(tidyverse)

#load data
dat<-read.table("ZoneA_d.txt", header=T)
vsp <- read.table("Vessel_Class.txt", header = T)

#scope out vessels and create a dataframe with unique vessel IDs
VID <- unique(dat$VesselID) %>% 
  as.data.frame()

names(VID) <- "VesselID"

#join with existing vessel class dataframe (from original classification)
vsl_class <- right_join(vsp, VID)

#select just vessel IDs that don't have a classification yet
need <- vsl_class[is.na(vsl_class$Group),]
need$VesselID <- sort(need$VesselID)

need_d <- right_join(dat, need)

#Find the vessel catch of each species and sort them based on number of sp caught
catch <- need_d %>% 
  group_by(VesselID, Species) %>% 
  summarize(count = sum(NumberKept)) %>% 
  group_by(VesselID) %>% 
  arrange(VesselID, desc(count)) %>% 
  as.data.frame()


write.csv(unique(catch$VesselID), "need_vesselID.csv")

#looking through these results to create a vector of classifications
#Comparing species caught from catch data frame and species code list
#doing this in excel, cuz it's super hard to follow in R

new_vsl_class <- read.csv("need_vID_class.csv", header = T)

vsl_class_update <- rbind(new_vsl_class, vsp)

write.table(vsl_class_update, "Vessel_Class_complete.txt")

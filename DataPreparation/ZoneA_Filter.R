# CPFV Target Analysis -- Kayla Blincow 4/26/18
# Finally got the data from the state and am now going to organize the full 
# dataset using this script.

# Filtering the complete dataset of all of the available years such that we only
# include landings from ports in San Diego Area

rm(list=ls())


dat<-read.table("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/FirstYearProject/Data_Official_2018/data.txt", header=T)
portkey<-read.table("portkey.txt", header=T)


#cross reference portkey$portcode with dat$portcode column and generate zone column
library(plyr)
m1<-join(dat, portkey, by = "PortCode")
A<-m1[which(m1$Zone=="A"),]


#create a new data file with just the Zone A data
write.table(A, "C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/FirstYearProject/Data_Official_2018/ZoneA_d.txt")

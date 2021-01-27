#Kayla Blincow
#Original: 1/31/2020

#Task: Generate time series matrices of species for each vessel type (weekly 
#and daily).

#Edited 4/26/2020 to properly normalize the values between 0 and 1

library(tidyverse)

rm(list = ls())

#read in the data
d <- read.table("ZoneA_d.txt", header = T)

#load the original vessel classifications
vsl <- read.table("Vessel_Class_complete.txt", header = T)

#remove extraneous columns for the full dataset
d <- select(d, c(LogMonth, LogDay, LogYear, LogDate, VesselID, SpeciesCode,
                 Species, NumberKept, Zone))

#join the data frames based on vesselID to get classification in full dataset
A1 <- left_join(d, vsl)


#one day where one vessel reported landing 2503 rockfish on a 3.5 hour trip. I believe they landed 3, and accidentally included the species code (250) in the column. Changing it to 3.
A1$NumberKept[which(A1$NumberKept==2503)] <- 3

#one day where one vessel reported landing 40,120 yellowtail on a 12 hour trip. I believe they landed 120, and accidentally included the species code (40) in the column. Changing it to 120.
A1$NumberKept[which(A1$NumberKept==40120)] <- 120

#Sweet! Now we need to pool landings for species/species complex for each 

#let's convert the A1 date to an actual date
A1$LogDate <- as.Date(A1$LogDate, format = "%m/%d/%Y")

####ROCKFISH####
#select rows that include rockfish species
rf <- grep("Rockfish", A1$Species)
# th <- grep("Thornyheads", A1$Species)
# sf <- grep("Scorpionfish", A1$Species)

RF <- A1[c(rf),]

#sum rows based on common LogDate, VesselID, and HrsMinutesFished
RF$NumberKept[is.na(RF$NumberKept)] <- 0

RFt <- RF %>% 
  group_by(LogDate, Group) %>% 
  summarize(RF = sum(NumberKept))

RFt_NO <- RFt %>%
  filter(Group == "NO")
RFt_NT <- RFt %>% 
  filter(Group =="NT")

####TUNAS####
#select rows that include tuna species
tu <- grep("Tuna", A1$Species)

TU <- A1 %>% 
  filter(Species == "Tuna, albacore" | 
           Species == "Tuna, yellowfin" |
           Species == "Tuna, bluefin")

#sum rows based on common LogDate, VesselID, and HrsMinutesFished
TU$NumberKept[is.na(TU$NumberKept)] <- 0

#One instance of negative NumberKept... Going to assume the negative sign wasn't meant to be there, and take the absolute value of the NumberKepts
TU$NumberKept <- abs(TU$NumberKept)

TUt <- TU %>%
  group_by(LogDate, Group) %>%
  summarize(TU = sum(NumberKept))

TUt_NO <- TUt %>%
  filter(Group == "NO")
TUt_OT <- TUt %>%
  filter(Group == "OT")

####Yellowfin Tuna####
#4/30/2020-shifting to separating and look at just yellowfin tuna and albacore
YF <- A1[A1$Species=="Tuna, yellowfin",]
YF$NumberKept[is.na(YF$NumberKept)] <- 0
YFt <- YF %>% 
  group_by(LogDate, Group) %>% 
  summarize(YF = sum(NumberKept))

YFt_NO <- YFt %>% 
  filter(Group =="NO")

####Albacore Tuna####
AL <- A1[A1$Species=="Tuna, albacore",]
AL$NumberKept[is.na(AL$NumberKept)] <- 0
#One instance of negative NumberKept... Going to assume the negative sign wasn't meant to be there, and take the absolute value of the NumberKepts
AL$NumberKept <- abs(AL$NumberKept)

ALt <- AL %>% 
  group_by(LogDate, Group) %>% 
  summarize(AL = sum(NumberKept))

ALt_NO <- ALt %>% 
  filter(Group =="NO")


####YELLOWTAIL####
YT <- A1[A1$Species=="Yellowtail",]

YT$NumberKept[is.na(YT$NumberKept)] <- 0

YTt <- YT %>% 
  group_by(LogDate, Group) %>% 
  summarize(YT = sum(NumberKept))

YTt_NO <- YTt %>% 
  filter(Group =="NO")
YTt_OT <- YTt %>% 
  filter(Group == "OT")


####KELP BASS####
kb <- grep("Bass, kelp", A1$Species)
KB <- A1[c(kb),]

KB$NumberKept[is.na(KB$NumberKept)] <- 0

KBt <- KB %>% 
  group_by(LogDate, Group) %>% 
  summarize(KB = sum(NumberKept))

KBt_NO <- KBt %>% 
  filter(Group == "NO")
KBt_NT <- KBt %>% 
  filter(Group == "NT")

#### BARRED SAND BASS ####
sb <- grep("Bass, barred sand", A1$Species)
SB <- A1[c(sb),]

SB$NumberKept[is.na(SB$NumberKept)] <- 0

SBt <- SB %>% 
  group_by(LogDate, Group) %>% 
  summarize(SB = sum(NumberKept))

SBt_NO <- SBt %>% 
  filter(Group == "NO")
SBt_NT <- SBt %>% 
  filter(Group == "NT")


#### BONITO ####
bo <- grep("Bonito", A1$Species)
BO <- A1[c(bo),]

BO$NumberKept[is.na(BO$NumberKept)] <- 0

BOt <- BO %>% 
  group_by(LogDate, Group) %>% 
  summarize(BO = sum(NumberKept))

BOt_NO <- BOt %>% 
  filter(Group == "NO")

#generate full sequence of dates to input zeroes on days when 0 fish were caught
dates <- as.data.frame(seq.Date(as.Date("1980-01-01"), as.Date("2017-12-31"), "days"))
names(dates) <- "LogDate"

#generate function that will add full sequence of dates to my landings time series
fulldates <- function(x){
  left_join(dates, x) %>% 
    select(-Group)
}

#use new function to add dates to all my time series and combine them
#nearshore-offshore targeting
RF_NO <- fulldates(RFt_NO)
KB_NO <- fulldates(KBt_NO)
SB_NO <- fulldates(SBt_NO)
BO_NO <- fulldates(BOt_NO)
YT_NO <- fulldates(YTt_NO)
TU_NO <- fulldates(TUt_NO)
AL_NO <- fulldates(ALt_NO)
YF_NO <- fulldates(YFt_NO)

#construct data with split tunas
NO_daily_tsplt <- cbind(RF_NO, KB_NO$KB, SB_NO$SB, BO_NO$BO, YT_NO$YT, AL_NO$AL, YF_NO$YF)
names(NO_daily_tsplt) <- c("Date", "RF", "KB", "SB", "BO", "YT", "AL", "YF")
NO_daily_tsplt[is.na(NO_daily_tsplt)] <- 0


write.table(NO_daily_tsplt, "NO_daily_raw_tsplt.txt")



#construct data with combined tunas
NO_daily <- cbind(RF_NO, KB_NO$KB, SB_NO$SB, BO_NO$BO, YT_NO$YT, TU_NO$TU)
names(NO_daily) <- c("Date", "RF", "KB", "SB", "BO", "YT", "TU")
NO_daily[is.na(NO_daily)] <- 0


write.table(NO_daily, "NO_daily_raw.txt")


#also need the data to be split among all tunas for later plotting
NO_daily_tsplt$BF <- NO_daily$TU - (NO_daily_tsplt$AL + NO_daily_tsplt$YF)
write.table(NO_daily_tsplt, "NO_daily_raw_3wysplt.txt")


#need to scale the ts between 0 and 1
max_vec <- apply(NO_daily[,2:7], 2, max)
max_vecsplt <- apply(NO_daily_tsplt[,2:8], 2, max)


#max_vec[6] is the highest landings for all species
NO_daily_s <- as.data.frame(NO_daily$Date, col.names = c("Date"))
NO_daily_s$RF <- NO_daily$RF/max_vec[6]
NO_daily_s$KB <- NO_daily$KB/max_vec[6]
NO_daily_s$SB <- NO_daily$SB/max_vec[6]
NO_daily_s$BO <- NO_daily$BO/max_vec[6]
NO_daily_s$YT <- NO_daily$YT/max_vec[6]
NO_daily_s$TU <- NO_daily$TU/max_vec[6]

colnames(NO_daily_s)[1] <- "Date"
write.table(NO_daily_s, "NO_daily.txt")

#do it again for the split data
NO_daily_sp <- as.data.frame(NO_daily_tsplt$Date, col.names = c("Date"))
NO_daily_sp$RF <- NO_daily_tsplt$RF/max_vecsplt[6]
NO_daily_sp$KB <- NO_daily_tsplt$KB/max_vecsplt[6]
NO_daily_sp$SB <- NO_daily_tsplt$SB/max_vecsplt[6]
NO_daily_sp$BO <- NO_daily_tsplt$BO/max_vecsplt[6]
NO_daily_sp$YT <- NO_daily_tsplt$YT/max_vecsplt[6]
NO_daily_sp$YF <- NO_daily_tsplt$YF/max_vecsplt[6]
NO_daily_sp$AL <- NO_daily_tsplt$AL/max_vecsplt[6]

colnames(NO_daily_sp)[1] <- "Date"
write.table(NO_daily_sp, "NO_daily_splt.txt")






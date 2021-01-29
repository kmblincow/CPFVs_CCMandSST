# CPFV Target Analysis -- Kayla Blincow 5/2/18
# Finally got the data from the state and am now going to organize the full 
# dtaset using this script.

rm(list=ls())

library(tidyverse)


A <- read.table("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/FirstYearProject/Data_Official_2018/ZoneA_d.txt", row.names = NULL, header=T)
S <- read.table("C:/Users/kmbli/OneDrive - UC San Diego/PhDzNuts/FirstYearProject/Year1_old/speciescodes.txt")

#For each vesselID I need the Sum of the NumberKept for each species code. 
#Then I need to order it such that I can see the most numerous species landed.

B <- A %>%
  group_by(VesselID, SpeciesCode) %>% 
  summarize(n = sum(NumberKept)) %>% 
  top_n(10, n) %>% 
  arrange(VesselID, n) %>% 
  left_join(S, by = "SpeciesCode")
 

#did it!


#Now I need to look through each of the top 10 landed species and classify the vessels as Nearshore Targeters (NT), Offshore Targeters (OT), Nearshore/Offshore Targeters (NO), or Other ()

#Entering the Vessel# and Target Code into Separate Excel Sheet for ease of data entry.


#QC and Double Check my vessel classification. Comparing my classification against the top block numbers for the vessels.

B1 <- A %>%
  group_by(VesselID, Block) %>% 
  summarize(freq = n()) %>% 
  top_n(10, freq) %>% 
  arrange(VesselID, freq) 

#Entering block classification in my same excel sheet

#If island blocks are involved, likely still called NO, as that is in the spirit of vessels choosing to go offshore if bigger offshore fish are biting
#But cross reference with species targeted to make sure


#Some scenarios when I need to reconcile differences--looking into trip type to see if that can help in this process

B2 <- A %>%
  group_by(VesselID, TripType, Block) %>% 
  summarize(freq = n()) 


B3 <- A %>%
  group_by(VesselID, SpeciesCode) %>% 
  summarize(freq = n()) %>%
  left_join(S, by = "SpeciesCode")

B4 <- A %>%
  group_by(VesselID, SpeciesCode) %>% 
  summarize(n = sum(NumberReleased)) %>% 
  top_n(10, n) %>% 
  arrange(VesselID, n) %>% 
  left_join(S, by = "SpeciesCode")


#Kayla Blincow
#4/30/2020

#Generating .Rdata file for the Warm daily Data (BF, AL, YF combined)

#clear environment
rm(list = ls())

#load libraries we will need
library(dplyr)
library(lubridate)

#load data
ts <- read.table("NO_daily.txt")
sst <- read.table("sst_SCB_new3.txt")

sst <- sst[,-2]


names(sst) <- c("year", "scbclass")

#Decided in Season is March-October
#create month/year columns to help filter
ts$month <- month(ts$Date)
ts$year <- year(ts$Date)

ts_sst <- right_join(ts, sst, by = "year")

#filter for rows from months March-October
ts_matrix <- ts_sst %>% 
  filter(month > 2 & month < 11) %>% 
  filter(scbclass=="warm")

#see how many rows per year
ts_matrix %>% 
  group_by(year) %>% 
  tally() %>% 
  summary()
#245 rows per year

#create lib and pred so that it doesn't link time periods that are not actually linked
#Update this if the time series changes!!!!
lib <- cbind(seq(1,1961, by = 245), seq(245, 2205, by = 245))
pred <- lib

#remove extraneous columns from ts_matrix
ts_matrix <- select(ts_matrix, -year, -month, -year, -scbclass)

#remove extraneous objects from environment
rm("ts", "sst", "ts_sst")

#save the environment as an .Rdata file that can be used in CCM code
save.image("CCM_TCombo_WarmSeasonDaily_SCB3.RData")

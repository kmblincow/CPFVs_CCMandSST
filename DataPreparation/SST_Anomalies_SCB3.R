#Kayla Blincow
#3/29/2020

#Trying the 3month rolling average method to identify anomalies with the SCB
#satellite data


rm(list = ls())

#libraries
library(tidyverse)
library(lubridate)

#read in the data
d <- read.csv("TemperatureData/OISST_SCB.csv", header = T)

#make a date column
d$date <- as.Date(d$time, format = "%m/%d/%Y")
d$month <- month(d$date)
d$year <- year(d$date)

d <- d[complete.cases(d),]
d <- d[d$year < 2018,]


#calculate overall means by month
d2 <- d %>% group_by(month) %>% 
  mutate(overall_mean = mean(temp),
         overall_sd = sd(temp))

#standardize the daily temperature
d2$SST <- (d2$temp - d2$overall_mean)/d2$overall_sd

#calculate a rolling average to smooth the data
f21 <- rep(1/91,91)
f21

y_sym <- stats::filter(d2$SST, f21, sides=2)
d2$smth <- y_sym

#plot it
ggplot(d2) +
  geom_line(aes(x = date, y = SST), color = "gray90") +
  geom_ribbon(aes(x = date, ymin = -1, ymax = 1),
              alpha = 0.2) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = date, y = smth), color = "purple") +
  scale_x_date(breaks = as.Date(c("1980-01-01", "1985-01-01", "1990-01-01", 
                                  "1995-01-01", "2000-01-01", "2005-01-01", 
                                  "2010-01-01", "2015-01-01")),
               minor_breaks = "1 year", date_labels = "%Y") +
  theme_bw()


#compare to other classification
old <- read.table("sst_sio.txt")

#classify based on rolling average exceeding one standard deviation
d2$classi <- NA

d2[is.na(d2)] <- 0


for(i in 1:nrow(d2)){
  if(d2$smth[i] > 1){
    d2$classi[i] <- 1
  } else if(d2$smth[i] < -1){
    d2$classi[i] <- -1
  } else {
    d2$classi[i] <-0
  }
} 

new <- d2 %>% 
  group_by(year) %>% 
  summarize(total = sum(classi))

new$classi <- NA

for(i in 1:nrow(new)){
  if(new$total[i] > 0){
    new$classi[i] <- "warm"
  } else if(new$total[i] < 0){
    new$classi[i] <- "cool"
  } else {
    new$classi[i] <- "norm"
  }
}

as.data.frame(new)
new <- new[1:35,]


write.table(new, "sst_SCB_new3.txt")
write.table(d2, "sst_SCB_rawd3.txt")



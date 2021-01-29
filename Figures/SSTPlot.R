#Kayla Blincow
#3/27/2020

#Make a SST plot that I can combine with my networks to look at differences 
#based on SST anomalies


rm(list = ls())

#load my packages
library(tidyverse)
library(gridExtra)


#read in ze data!
d <- read.table("sst_SCB_rawd3.txt", header = T)
classi <- read.table("sst_SCB_new3.txt", header = T)

d$date <- as.Date(d$date)
d <- d[d$year < 2018,]
#make ze plot!
p1 <- ggplot(d) +
  geom_line(aes(x = date, y = SST), color = "gray90")+
  scale_x_date(breaks = as.Date(c("1980-01-01", "1985-01-01", "1990-01-01", 
                                  "1995-01-01", "2000-01-01", "2005-01-01", 
                                  "2010-01-01", "2015-01-01")),
               minor_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Standardized SST", x = "Year", size = 2) +
  annotate("rect", xmin = as.Date("1983-01-01"), xmax = as.Date("1985-01-01"), 
            ymin = 0, ymax = 4.4, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1992-01-01"), xmax = as.Date("1993-01-01"), 
           ymin = 0, ymax = 4.4, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"), 
           ymin = 0, ymax = 4.4, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2018-01-01"), 
           ymin = 0, ymax = 4.4, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1988-01-01"), xmax = as.Date("1990-01-01"), 
           ymin = -3, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1991-01-01"), xmax = as.Date("1992-01-01"), 
           ymin = -3, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1999-01-01"), xmax = as.Date("2000-01-01"), 
           ymin = -3, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-01-01"), xmax = as.Date("2003-01-01"), 
           ymin = -3, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-01-01"), xmax = as.Date("2012-01-01"), 
           ymin = -3, ymax = 0, fill = "blue", alpha = 0.4) +
  geom_line(aes(x = date, y = smth), size = 0.5) +
  geom_hline(yintercept = 1, color = "gray50", linetype = "twodash") +
  geom_hline(yintercept = -1, color = "gray50", linetype = "twodash") +
  geom_hline(yintercept = 0) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

png("SST_plot_SCB3.png", 
    units="in", 
    width=8, 
    height=5, 
    pointsize=12, 
    res=400)
suppressWarnings(print(p1))
dev.off()



p2 <- ggplot(d) +
  #geom_line(aes(x = date, y = SST), color = "gray90")+
  scale_x_date(breaks = as.Date(c("1980-01-01", "1985-01-01", "1990-01-01", 
                                  "1995-01-01", "2000-01-01", "2005-01-01", 
                                  "2010-01-01", "2015-01-01")),
               minor_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Standardized SST", x = "Year", size = 2) +
  annotate("rect", xmin = as.Date("1983-01-01"), xmax = as.Date("1985-01-01"), 
           ymin = 0, ymax = 3, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1992-01-01"), xmax = as.Date("1993-01-01"), 
           ymin = 0, ymax = 3, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"), 
           ymin = 0, ymax = 3, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2018-01-01"), 
           ymin = 0, ymax = 3, fill = "red", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1988-01-01"), xmax = as.Date("1990-01-01"), 
           ymin = -2, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1991-01-01"), xmax = as.Date("1992-01-01"), 
           ymin = -2, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("1999-01-01"), xmax = as.Date("2000-01-01"), 
           ymin = -2, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2001-01-01"), xmax = as.Date("2003-01-01"), 
           ymin = -2, ymax = 0, fill = "blue", alpha = 0.4) +
  annotate("rect", xmin = as.Date("2007-01-01"), xmax = as.Date("2012-01-01"), 
           ymin = -2, ymax = 0, fill = "blue", alpha = 0.4) +
  geom_line(aes(x = date, y = smth), size = 0.5) +
  geom_hline(yintercept = 1, color = "gray50", linetype = "twodash") +
  geom_hline(yintercept = -1, color = "gray50", linetype = "twodash") +
  geom_hline(yintercept = 0) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

png("SST_plot_RollingMean.png", 
    units="in", 
    width=8, 
    height=4, 
    pointsize=12, 
    res=400)
suppressWarnings(print(p2))
dev.off()


head(d)
plot(d$date, (d$temp - mean(d$temp)), type = "l")

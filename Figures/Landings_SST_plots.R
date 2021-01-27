#Kayla Blincow
#3/27/2020

#Make a plot of fish landings against temperature classifications

rm(list = ls())

#load packages I need
library(tidyverse)
library(patchwork)
library(fishualize)

#load ze data
d <- read.table("No_daily_raw.txt", header = T)

d$Date <- as.Date(d$Date)

d <- filter(d, Date > "1982-12-31")

#make quick easy time series plots
plotsp <- function(sp, xname, yname, xlabels){
  ggplot(d) +
    scale_y_continuous(limits = c(0, 5500), expand = c(0,0)) +
    labs(y = yname, x = xname, size = 2) +
    # annotate("rect", xmin = as.Date("1983-01-01"), xmax = as.Date("1985-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "red", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("1992-01-01"), xmax = as.Date("1993-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "red", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "red", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2018-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "red", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("1988-01-01"), xmax = as.Date("1990-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "blue", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("1991-01-01"), xmax = as.Date("1992-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "blue", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("1999-01-01"), xmax = as.Date("2000-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "blue", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("2001-01-01"), xmax = as.Date("2003-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "blue", alpha = 0.4) +
    # annotate("rect", xmin = as.Date("2007-01-01"), xmax = as.Date("2012-01-01"), 
    #          ymin = 0, ymax = 5500, fill = "blue", alpha = 0.4) +
    geom_line(aes(x = Date, y = sp)) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 12), 
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  
} 

pRF <- plotsp(sp = d$RF, xname = "", yname = "RF")
pKB <- plotsp(sp = d$KB, xname = "", yname = "KB") 
pSB1 <- plotsp(sp = d$SB, xname = "", yname = "SB") 
pSB <- plotsp(sp = d$SB, xname = "Year", yname = "SB") +
  scale_x_date(breaks = as.Date(c("1985-01-01", "1990-01-01",
                                  "1995-01-01", "2000-01-01", "2005-01-01", 
                                  "2010-01-01", "2015-01-01")),
               minor_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

pBO <- plotsp(sp = d$BO, xname = "", yname = "BO")
pYT <- plotsp(sp = d$YT, xname = "", yname = "YT")
pTU <- plotsp(sp = d$TU, xname = "Year", yname = "TU") +  
  scale_x_date(breaks = as.Date(c("1985-01-01", "1990-01-01",
                                  "1995-01-01", "2000-01-01", "2005-01-01", 
                                  "2010-01-01", "2015-01-01")),
               minor_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

col2 <- (pRF + pBO) / (pKB + pYT) / (pSB + pTU)

png("Landings_2col_SCB3.png", 
    units="in", 
    width=9, 
    height=7, 
    pointsize=12, 
    res=400)
suppressWarnings(print(col2))
dev.off()

col1 <- pRF / pKB / pSB1 / pBO/ pYT / pTU

png("Landings_SST_1col.png",
    units="in", 
    width=6, 
    height=10, 
    pointsize=12, 
    res=400)
suppressWarnings(print(col1))
dev.off()

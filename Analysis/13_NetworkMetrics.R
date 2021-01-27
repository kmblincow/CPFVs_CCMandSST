#Kayla Blincow
#4/3/2020

#This script calculates the network analysis metrics (density and node centrality)
#for the warm, cool, and normal networks

#density is a measure of how connected the network is, and is calculated by 
#dividing the total possible number of links by the actual number of links in
#the network


rm(list = ls())

#load the libraries
library(tidyverse)

###Density Metric###
density <- rep(NA, 3)

#function to look for significant interactions
sig <- function(siglevel){
  for(i in 1:nrow(ccm_matrix)){
    for(j in 1:ncol(ccm_matrix)){
      if(ccm_significance[i,j] > siglevel){
        ccm_matrix[i,j] <- 0
      }
      if(ccm_matrix[i,j] > 0){
        ccm_matrix[i,j] <- 1
      }
    }
  }
  return(ccm_matrix)
}

sig_wt <- function(siglevel){
  for(i in 1:nrow(ccm_matrix)){
    for(j in 1:ncol(ccm_matrix)){
      if(ccm_significance[i,j] > siglevel){
        ccm_matrix[i,j] <- 0
      }
    }
  }
  return(ccm_matrix)
}


####Density Calculations####

#Warm
load("TCombo_WarmDaily_ccm_webs_SCB3.RData")
warm_matrix <- sig(0.05)
warm_weight <- sig_wt(0.05)

n <- 6
density[1] <- sum(warm_matrix)/(n*(n-1))

#Norm
load("TCombo_NormDaily_ccm_webs_SCB3.RData")
norm_matrix <- sig(0.05)
norm_weight <- sig_wt(0.05)

n <- 6
density[2] <- sum(norm_matrix)/(n*(n-1))

#Cool
load("TCombo_CoolDaily_ccm_webs_SCB3.RData")
cool_matrix <- sig(0.05)
cool_weight <- sig_wt(0.05)

density[3] <- sum(cool_matrix)/(n*(n-1))



dens <- as.data.frame(density)
dens$SSTClass <- as.factor(c("Warm", "Norm", "Cool"))
plot(dens$SSTClass, dens$density)


####Centrality Calculations####
###Node Centrality Metric###
#we are going to use weighted degree centrality metric put forth in Opsahl et al. 2010


#since this is a directed network we need account for edges coming in and going
#out for node, so we will have in centrality and out centrality

# C = k[i] * (S[i]/k[i])^a

#where k is the number of edges coming in or out of a node, S is the total weight
#associated with those edges, and a is a tuning parameter

#if a = 0, all we care about is the number of edges
#if a = 1, all we care about is the weight of the edges
#if 0 < a < 1, we are weighting the number and weight of edges accordingly
#closer to 0 gives more importance to the number, closer to 1 to the weight
#if a > 1, we are penalizing for having higher number of edges

#create a function to calculate the centrality (in and out)
cent_in <- function(edge_mat, wt_mat, a){
  centr <- rep(NA, ncol(edge_mat))
  for(i in 1:ncol(edge_mat)){
    centr[i] <- sum(edge_mat[,i]) * (sum(wt_mat[,i])/sum(edge_mat[,i]))^a
  }
  return(centr)
}

cent_out <- function(edge_mat, wt_mat, a){
  centr <- rep(NA, nrow(edge_mat))
  for(i in 1:nrow(edge_mat)){
    centr[i] <- sum(edge_mat[i,]) * (sum(wt_mat[i,])/sum(edge_mat[i,]))^a
  }
  return(centr)
}

#in (columns of the ccm matrix)
#warm
cent_in_warm_5 <- cent_in(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.5)

#cool
cent_in_cool_5 <- cent_in(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.5)

#normal
cent_in_norm_5 <- cent_in(edge_mat = norm_matrix, wt_mat = norm_weight, a = 0.5)

#out (rows of the ccm matrix)
#warm
cent_out_warm_5 <- cent_out(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.5)

#cool
cent_out_cool_5 <- cent_out(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.5)

#cold
cent_out_norm_5 <- cent_out(edge_mat = norm_matrix, wt_mat = norm_weight, a = 0.5)



#let's convert our results to a dataframe
SP <- rep(c("RF", "KB", "SB", "BO", "YT", "TU"), 3)
SSTClass <- rep(c(rep("Warm", 6), rep("Cool", 6), rep("Norm", 6)),2)
CENT <- c(rep("IN", 18), rep("OUT", 18))
CENTVAL5 <- c(cent_in_warm_5, cent_in_cool_5, cent_in_norm_5,
              cent_out_warm_5, cent_out_cool_5, cent_out_norm_5)



d <- as.data.frame(cbind(SP, SSTClass, CENT, CENTVAL5))
d$CENTVAL5 <- as.numeric(as.character(d$CENTVAL5))

d %>% 
  group_by(SSTClass) %>% 
  summarize(maxi = max(CENTVAL5),
            mini = min(CENTVAL5),
            mn = mean(CENTVAL5),
            sd = sd(CENTVAL5))

write.table(d, "NetworkMetrics_Centrality.txt")








####OLD CODE####
#Hot
load("HotGradSsnDaily2_ccm_webs.RData")
hot_matrix <- sig(0.05)
hot_weight <- sig_wt(0.05)


#calculate number of significant interactions divide that by number of total 
#interactions: n(n-1), where n is the total number of nodes 
n <- 6
density[1] <- sum(hot_matrix)/(n*(n-1))

#Warm
load("WarmGradSsnDaily2_ccm_webs.RData")
warm_matrix <- sig(0.05)
warm_weight <- sig_wt(0.05)

density[2] <- sum(warm_matrix)/(n*(n-1))

#Cool
load("CoolGradSsnDaily2_ccm_webs.RData")
cool_matrix <- sig(0.05)
cool_weight <- sig_wt(0.05)

density[3] <- sum(cool_matrix)/(n*(n-1))

#Cold
load("ColdGradSsnDaily2_ccm_webs.RData")
cold_matrix <- sig(0.05)
cold_weight <- sig_wt(0.05)

density[4] <- sum(cold_matrix)/(n*(n-1))


###Node Centrality Metric###
#we are going to use weighted degree centrality metric put forth in Opsahl et al. 2010


#since this is a directed network we need account for edges coming in and going
#out for node, so we will have in centrality and out centrality

# C = k[i] * (S[i]/k[i])^a

#where k is the number of edges coming in or out of a node, S is the total weight
#associated with those edges, and a is a tuning parameter

#if a = 0, all we care about is the number of edges
#if a = 1, all we care about is the weight of the edges
#if 0 < a < 1, we are weighting the number and weight of edges accordingly
#closer to 0 gives more importance to the number, closer to 1 to the weight
#if a > 1, we are penalizing for having higher number of edges

#create a function to calculate the centrality (in and out)
cent_in <- function(edge_mat, wt_mat, a){
  centr <- rep(NA, ncol(edge_mat))
  for(i in 1:ncol(edge_mat)){
    centr[i] <- sum(edge_mat[,i]) * (sum(wt_mat[,i])/sum(edge_mat[,i]))^a
  }
  return(centr)
}

cent_out <- function(edge_mat, wt_mat, a){
  centr <- rep(NA, nrow(edge_mat))
  for(i in 1:nrow(edge_mat)){
    centr[i] <- sum(edge_mat[i,]) * (sum(wt_mat[i,])/sum(edge_mat[i,]))^a
  }
  return(centr)
}

#in (columns of the ccm matrix)
#hot 
cent_in_hot_5 <- cent_in(edge_mat = hot_matrix, wt_mat = hot_weight, a = 0.5)
cent_in_hot_75 <- cent_in(edge_mat = hot_matrix, wt_mat = hot_weight, a = 0.75)
cent_in_hot_1 <- cent_in(edge_mat = hot_matrix, wt_mat = hot_weight, a = 1)

#warm
cent_in_warm_5 <- cent_in(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.5)
cent_in_warm_75 <- cent_in(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.75)
cent_in_warm_1 <- cent_in(edge_mat = warm_matrix, wt_mat = warm_weight, a = 1)

#cool
cent_in_cool_5 <- cent_in(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.5)
cent_in_cool_75 <- cent_in(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.75)
cent_in_cool_1 <- cent_in(edge_mat = cool_matrix, wt_mat = cool_weight, a = 1)

#cold
cent_in_cold_5 <- cent_in(edge_mat = cold_matrix, wt_mat = cold_weight, a = 0.5)
cent_in_cold_75 <- cent_in(edge_mat = cold_matrix, wt_mat = cold_weight, a = 0.75)
cent_in_cold_1 <- cent_in(edge_mat = cold_matrix, wt_mat = cold_weight, a = 1)

#out (rows of the ccm matrix)
#hot 
cent_out_hot_5 <- cent_out(edge_mat = hot_matrix, wt_mat = hot_weight, a = 0.5)
cent_out_hot_75 <- cent_out(edge_mat = hot_matrix, wt_mat = hot_weight, a = 0.75)
cent_out_hot_1 <- cent_out(edge_mat = hot_matrix, wt_mat = hot_weight, a = 1)

#warm
cent_out_warm_5 <- cent_out(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.5)
cent_out_warm_75 <- cent_out(edge_mat = warm_matrix, wt_mat = warm_weight, a = 0.75)
cent_out_warm_1 <- cent_out(edge_mat = warm_matrix, wt_mat = warm_weight, a = 1)

#cool
cent_out_cool_5 <- cent_out(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.5)
cent_out_cool_75 <- cent_out(edge_mat = cool_matrix, wt_mat = cool_weight, a = 0.75)
cent_out_cool_1 <- cent_out(edge_mat = cool_matrix, wt_mat = cool_weight, a = 1)

#cold
cent_out_cold_5 <- cent_out(edge_mat = cold_matrix, wt_mat = cold_weight, a = 0.5)
cent_out_cold_75 <- cent_out(edge_mat = cold_matrix, wt_mat = cold_weight, a = 0.75)
cent_out_cold_1 <- cent_out(edge_mat = cold_matrix, wt_mat = cold_weight, a = 1)



#let's convert our results to a dataframe
SP <- rep(c("RF", "KB", "SB", "BO", "YT", "TU"), 8)
SSTClass <- rep(c(rep("Hot", 6), rep("Warm", 6), rep("Cool", 6), rep("Cold", 6)),2)
CENT <- c(rep("IN", 24), rep("OUT", 24))
CENTVAL5 <- c(cent_in_hot_5, cent_in_warm_5, cent_in_cool_5, cent_in_cold_5,
              cent_out_hot_5, cent_out_warm_5, cent_out_cool_5, cent_out_cold_5)
CENTVAL75 <- c(cent_in_hot_75, cent_in_warm_75, cent_in_cool_75, cent_in_cold_75,
              cent_out_hot_75, cent_out_warm_75, cent_out_cool_75, cent_out_cold_75)
CENTVAL1 <- c(cent_in_hot_1, cent_in_warm_1, cent_in_cool_1, cent_in_cold_1,
              cent_out_hot_1, cent_out_warm_1, cent_out_cool_1, cent_out_cold_1)


d <- as.data.frame(cbind(SP, SSTClass, CENT, CENTVAL5, CENTVAL75, CENTVAL1))
d$CENTVAL5 <- as.numeric(as.character(d$CENTVAL5))
d$CENTVAL75 <- as.numeric(as.character(d$CENTVAL75))
d$CENTVAL1 <- as.numeric(as.character(d$CENTVAL1))

write.table(d, "NetworkMetricsGradient2.txt")


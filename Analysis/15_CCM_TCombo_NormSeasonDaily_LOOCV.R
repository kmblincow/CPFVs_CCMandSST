#Kayla Blincow
#2/4/2020

#Running CCM for the IN season daily dataset using leave one out cross-validation,
#and incorporating seasonal surrogate test

#clear environment
rm(list = ls())

#load libraries we will need
library(rEDM)
library(parallel)
library(pbapply)
library(purrr)
library(dplyr)


#load data
load("CCM_TCombo_NormSeasonDaily_SCB3.Rdata")

#I am drawing from my code and Chase's code to generate a script that I can more
#easily follow. The hope is that I can go through a more systematic approach to 
#running CCM while using functions to reduce the lines of code.

#NOTE: the actual cross mapping relies on functions found in the Kay_CCM_fnctn.R
#and CCM_plot_fnctn.R scripts

#Here goes nothing!

#check out the data
head(ts_matrix)
head(lib)
head(pred)

#currently the lib and pred are the entire time series, so the simplex will 
#use leave-one-out-cross-validation over the entire time series
#****will compare this to other options later****

#Use simplex projection to test for the optimal embedding dimension

#Need to apply this function to every column except the first (which is the date)
#i.e. we need an optimal E for each time series in the matrix

#Set plotting area so that we can ID optimal embedding dimension using plot
par(mar = c(4,4,1,1), mfrow = c(2,3), mgp = c(2.5, 1, 0))

#create an object that is the list of species of interest
varlst <- colnames(ts_matrix)[2:ncol(ts_matrix)]

#create an empty list that to put our simplex output into
simplex_output_list <- NULL

ts_matrix$Date <- as.Date(ts_matrix$Date)

#run the simplex for each species' weekly ts
for (i in 1:length(varlst)) {
  simplex_output_list[[i]] <- simplex(ts_matrix[, c("Date", varlst[i])],
                                      lib = lib, pred = pred, E = c(2:20))
  plot(simplex_output_list[[i]]$E, simplex_output_list[[i]]$rho,
       type= "l", xlab = "Embedding Dimension (E)",
       ylab = "Forecast Skill (rho)", main = varlst[i])
}

#this code will output warning messages, but they are okay--just telling us that
#the simplex is using l-o-o cross validation methods

#create a list of the best Es
bestE <- sapply(simplex_output_list, function(simplex_output) {
  simplex_output$E[which.max(simplex_output$rho)]
})

bestE #check these results against plots that were made.



#Now we should test for prediction decay (time to prediction (tp) v forecast skill(rho))
#and non-linearity (nonlinearity(theta) v forecast skill(rho))

#Prediction Decay for each species

#create empty list to put our simplex results into
pd_simplex_list <- NULL

#run the simplex with the optimal embedding dimensions from above and plot tp v rho
for (i in 1:length(varlst)) {
  pd_simplex_list[[i]] <- simplex(ts_matrix[, c("Date", varlst[i])],
                                  lib = lib, pred = pred, E = bestE[i], tp = 1:10)
  plot(pd_simplex_list[[i]]$tp, pd_simplex_list[[i]]$rho,
       type= "l", xlab = "Time to Prediction (tp)",
       ylab = "Forecast Skill (rho)", main = varlst[i])
}
#all of the plots show the relationship we would like (decay with increase in tp)


#Nonlinearity Test for each species

#create empty list to put our smap results in
smap_output_list <- NULL

#run the smap with the optimal embedding dimensions from above and plot theta v rho
for( i in 1:length(varlst)) {
  smap_output_list[[i]] <- s_map(ts_matrix[, c("Date", varlst[i])], lib = lib, 
                                 pred = pred, E = bestE[i], silent = TRUE)
}

par(mfrow = c(3,2))
for( i in 1:length(varlst)) {
  plot(smap_output_list[[i]]$theta, smap_output_list[[i]]$rho, type = "l",
       xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)", main = varlst[i])
}

#All the time series suggest nonlinear dynamics in the data (because of the intial
#rise in rho for non-zero theta, followed by a hsarp drop-off in rho with theta)

#convert E to format we need for next function/save the csv so it can be called.
bestE <- cbind(bestE, varlst)
colnames(bestE) <- c("E", "taxa")

write.csv(bestE, "NormDaily_bestE_TComboSCB3.csv", row.names = FALSE)



#Now to actually do the cross mapping...

#Let's Try Chase's crazy function...

compute_ccm_web_1 <- function(in_file = "CCM_TCombo_NormSeasonDaily_SCB3.Rdata", 
                              best_E_file = "NormDaily_bestE_TComboSCB3.csv",
                              num_surr = 500, num_cores = "automatic",
                              progress_bar = TRUE,
                              operating_system = "windows",
                              out_file, 
                              lib_auto = FALSE, tp_select = 0,
                              chunk)
{
  #   for the ith row of ccm_params, do ccm calculations:
  #   use tp = 0 to measure CCM
  #   generate surrogate data
  #   compute significance of ccm vs surrogate data
  
  load(in_file)
  E_select <- read.csv(file = best_E_file, header = TRUE)
  
  # make sure best E is selected correctly
  
  taxa <- colnames(ts_matrix[,-1])
  E_select <- E_select[match(E_select$taxa,taxa),]
  
  drops_list <- which(is.na(E_select$E)) 
  drops <- E_select$taxa[drops_list] 
  
  ts_matrix <- ts_matrix[,!(names(ts_matrix) %in% drops)]
  E_select <- E_select[!(E_select$taxa %in% drops),]
  
  # variables to run all combinations
  n_obs <- NROW(ts_matrix)
  n_taxa <- NCOL(ts_matrix) 
  columns_to_use <- 2:n_taxa
  
  # generate all combinations where from != to
  ccm_params <- expand.grid(to = columns_to_use, from = columns_to_use)
  ccm_params <- ccm_params[ccm_params$from != ccm_params$to,]
  
  # code to run ccm for one pair
  run_ccm <- function(i)
  {
    temp_df <- ts_matrix[, c(ccm_params$from[i], ccm_params$to[i])]
    
    # select lib and pred
    # find first non-NA position: which(!is.na)
    # find max value of this position for from and to
    # start_value is after that: 1 +
    
    start_value <- 1 + max(min(which(!is.na(temp_df[, 1]))), 
                           min(which(!is.na(temp_df[, 2]))))
    
    if(lib_auto == TRUE){
      lib = c(start_value,n_obs)
      pred = lib
    }
    if(lib_auto == FALSE){
      lib = lib
      pred = pred
    }
    
    
    # Best E from s-map (hand-picked)
    
    best_E <- E_select$E[ccm_params$from[i]-1]
    
    # run ccm with tp = 0 and best_E
    
    ccm_tp_zero <- ccm(temp_df, lib = lib, pred = pred, lib_sizes = seq(1, 3675, by = 245), #MAKING CHANGES HERE!
                       E = best_E, tp = tp_select, lib_column = 1, target_column = 2, 
                       random_libs = FALSE, num_samples = 1, replace = FALSE,
                       first_column_time = FALSE, silent = TRUE)
    
    # make surrogate time series
    
    ts_index <- start_value:n_obs
    
    surrogate_data <- make_surrogate_seasonal(as.numeric(temp_df[ts_index,2]), 
                                              num_surr = num_surr, T_period = 245)
    
    # run ccm with surrogate data
    
    ccm_surrogate <- map_df(1:NCOL(surrogate_data), function(j) {
      temp_df[ts_index, 2] <- surrogate_data[,j]
      ccm(temp_df, lib = lib, pred = pred, lib_sizes = n_obs, 
          E = best_E, tp = 0, lib_column = 1, target_column = 2, 
          random_libs = FALSE, num_samples = 1, replace = FALSE,
          first_column_time = FALSE, silent = TRUE)
    })
    
    
    return(data_frame(from = ccm_params$from[i], 
                      to = ccm_params$to[i], 
                      best_E = best_E, 
                      ccm_tp_zero = list(ccm_means(ccm_tp_zero)), #ccm_means(ccm_tp_zero)
                      ccm_surrogate = list(ccm_surrogate)))
  }
  
  # run in parallel
  
  # how many cores to use
  if(is.character(num_cores) == TRUE){no_cores <- detectCores() - 1
  } else {no_cores <- num_cores}
  
  # initiate cluster
  if(operating_system == "windows"){
    cluster <- makeCluster(no_cores)}
  if(operating_system != "windows"){
    cluster <- makeCluster(no_cores, type = "FORK")}
  
  # export variables
  clusterExport(cluster, varlist = c("ts_matrix", "E_select", "ccm_params", 
                                     "num_surr", "n_obs", "lib", "pred", "lib_auto", "tp_select"),
                envir = environment())
  # export packages and run code
  if(progress_bar == TRUE){
    clusterEvalQ(cluster, expr =  c(library(rEDM),library(purrr), library(dplyr), library(pbapply)))
    ccm_results <- pblapply(cl = cluster, X = chunk, FUN = run_ccm)}
  if(progress_bar == FALSE){
    clusterEvalQ(cluster, expr =  c(library(rEDM),library(purrr), library(dplyr)))
    ccm_results <- parLapply(cl = cluster, X = chunk, fun = run_ccm)}
  
  # stop cluster
  stopCluster(cl = cluster)
  
  # convert to matrix
  n_tibble = length(ccm_results)
  
  # Create empty matrices to store results
  ccm_matrix = array(0,c(n_taxa,n_taxa)) # create an empty array to store results
  colnames(ccm_matrix) = names(ts_matrix)
  rownames(ccm_matrix) = names(ts_matrix)
  
  ccm_significance = ccm_matrix # create an empty array to store results
  bestE_matrix = ccm_matrix
  
  # Extract data in a loop
  for (i in 1:n_tibble){
    from = ccm_results[[i]]$from
    to = ccm_results[[i]]$to
    actual_rho = tail(ccm_results[[i]]$ccm_tp_zero[[1]]$rho, 1) #MADE CHANGES HERE!
    ccm_matrix[from,to] = actual_rho
    bestE_matrix[from,to] = ccm_results[[i]]$best_E
    #  Calculate and store p-value
    rho_surr = ccm_results[[i]]$ccm_surrogate[[1]]$rho
    ccm_significance[from,to] = (sum(actual_rho < rho_surr) + 1) / (length(rho_surr) + 1)
  }
  
  ccm_matrix = ccm_matrix[-1,-1]
  ccm_significance = ccm_significance[-1,-1]
  bestE_matrix = bestE_matrix[-1,-1]
  
  save(ccm_matrix, ccm_significance, bestE_matrix, ccm_results, file = out_file)
  
}


compute_ccm_web_1(chunk = 1:5, out_file = "TCombo_NormDaily_ccm_webs_1.Rdata")
#fuck yeah! that worked!

compute_ccm_web_1(chunk = 6:10, out_file = "TCombo_NormDaily_ccm_webs_2.Rdata")

compute_ccm_web_1(chunk = 11:15, out_file = "TCombo_NormDaily_ccm_webs_3.Rdata")

compute_ccm_web_1(chunk = 16:20, out_file = "TCombo_NormDaily_ccm_webs_4.Rdata")

compute_ccm_web_1(chunk = 21:25, out_file = "TCombo_NormDaily_ccm_webs_5.Rdata")

compute_ccm_web_1(chunk = 26:30, out_file = "TCombo_NormDaily_ccm_webs_6.Rdata")

#now we need to combine the split matrices (cbind first three rows of ccm_matrix_1 with second three rows of ccm_matrix_2, etc.)
load("TCombo_NormDaily_ccm_webs_1.Rdata")
ccm_matrix_1 <- ccm_matrix
ccm_significance_1 <- ccm_significance 
ccm_results_1 <- ccm_results
bestE_matrix_1 <- bestE_matrix

load("TCombo_NormDaily_ccm_webs_2.Rdata")
ccm_matrix_2 <- ccm_matrix
ccm_significance_2 <- ccm_significance
ccm_results_2 <- ccm_results
bestE_matrix_2 <- bestE_matrix

load("TCombo_NormDaily_ccm_webs_3.Rdata")
ccm_matrix_3 <- ccm_matrix
ccm_significance_3 <- ccm_significance
ccm_results_3 <- ccm_results
bestE_matrix_3 <- bestE_matrix

load("TCombo_NormDaily_ccm_webs_4.Rdata")
ccm_matrix_4 <- ccm_matrix
ccm_significance_4 <- ccm_significance
ccm_results_4 <- ccm_results
bestE_matrix_4 <- bestE_matrix

load("TCombo_NormDaily_ccm_webs_5.Rdata")
ccm_matrix_5 <- ccm_matrix
ccm_significance_5 <- ccm_significance
ccm_results_5 <- ccm_results
bestE_matrix_5 <- bestE_matrix

load("TCombo_NormDaily_ccm_webs_6.Rdata")
ccm_matrix_6 <- ccm_matrix
ccm_significance_6 <- ccm_significance
ccm_results_6 <- ccm_results
bestE_matrix_6 <- bestE_matrix

#COMBINE THE MATRICES (said in Saruman's voice)
ccm_matrix <- rbind(ccm_matrix_1[1,], ccm_matrix_2[2,], ccm_matrix_3[3,],
                    ccm_matrix_4[4,], ccm_matrix_5[5,], ccm_matrix_6[6,])
#ah yeah! (also said in Saruman's voice)
ccm_significance <- rbind(ccm_significance_1[1,], ccm_significance_2[2,], 
                          ccm_significance_3[3,], ccm_significance_4[4,],
                          ccm_significance_5[5,], ccm_significance_6[6,])


bestE_matrix <- rbind(bestE_matrix_1[1,], bestE_matrix_2[2,], bestE_matrix_3[3,],
                      bestE_matrix_4[4,], bestE_matrix_5[5,], bestE_matrix_6[6,])

ccm_results <- c(ccm_results_1, ccm_results_2, ccm_results_3, ccm_results_4,
                 ccm_results_5, ccm_results_6)

#convergence plots
library(patchwork)
conv_plot <- function(data, titl) {
  ggplot(data, aes(x = lib_size, y = rho)) +
    geom_line() +
    labs(title = titl, x = "Time Series Length", y = "Prediction Skill (rho)") +
    xlim(100,2500) +
    theme_classic()
  
}

#RF->
RFKB <- conv_plot(data = as.data.frame(ccm_results[[1]]$ccm_tp_zero[[1]]),
                  titl = "RF xmap KB")
RFSB <- conv_plot(data = as.data.frame(ccm_results[[2]]$ccm_tp_zero[[1]]),
                  titl = "RF xmap SB")
RFBO <- conv_plot(data = as.data.frame(ccm_results[[3]]$ccm_tp_zero[[1]]),
                  titl = "RF xmap BO")
RFYT <- conv_plot(data = as.data.frame(ccm_results[[4]]$ccm_tp_zero[[1]]),
                  titl = "RF xmap YT")
RFTU <- conv_plot(data = as.data.frame(ccm_results[[5]]$ccm_tp_zero[[1]]),
                  titl = "RF xmap TU")

#KB->
KBRF <- conv_plot(data = as.data.frame(ccm_results[[6]]$ccm_tp_zero[[1]]),
                  titl = "KB xmap RF")
KBSB <- conv_plot(data = as.data.frame(ccm_results[[7]]$ccm_tp_zero[[1]]),
                  titl = "KB xmap SB")
KBBO <- conv_plot(data = as.data.frame(ccm_results[[8]]$ccm_tp_zero[[1]]),
                  titl = "KB xmap BO")
KBYT <- conv_plot(data = as.data.frame(ccm_results[[9]]$ccm_tp_zero[[1]]),
                  titl = "KB xmap YT")
KBTU <- conv_plot(data = as.data.frame(ccm_results[[10]]$ccm_tp_zero[[1]]),
                  titl = "KB xmap TU")

#SB->
SBRF <- conv_plot(data = as.data.frame(ccm_results[[11]]$ccm_tp_zero[[1]]),
                  titl = "SB xmap RF")
SBKB <- conv_plot(data = as.data.frame(ccm_results[[12]]$ccm_tp_zero[[1]]),
                  titl = "SB xmap KB")
SBBO <- conv_plot(data = as.data.frame(ccm_results[[13]]$ccm_tp_zero[[1]]),
                  titl = "SB xmap BO")
SBYT <- conv_plot(data = as.data.frame(ccm_results[[14]]$ccm_tp_zero[[1]]),
                  titl = "SB xmap YT")
SBTU <- conv_plot(data = as.data.frame(ccm_results[[15]]$ccm_tp_zero[[1]]),
                  titl = "SB xmap TU")

#BO->
BORF <- conv_plot(data = as.data.frame(ccm_results[[16]]$ccm_tp_zero[[1]]),
                  titl = "BO xmap RF")
BOKB <- conv_plot(data = as.data.frame(ccm_results[[17]]$ccm_tp_zero[[1]]),
                  titl = "BO xmap KB")
BOSB <- conv_plot(data = as.data.frame(ccm_results[[18]]$ccm_tp_zero[[1]]),
                  titl = "BO xmap SB")
BOYT <- conv_plot(data = as.data.frame(ccm_results[[19]]$ccm_tp_zero[[1]]),
                  titl = "BO xmap YT")
BOTU <- conv_plot(data = as.data.frame(ccm_results[[20]]$ccm_tp_zero[[1]]),
                  titl = "BO xmap TU")

#YT->
YTRF <- conv_plot(data = as.data.frame(ccm_results[[21]]$ccm_tp_zero[[1]]),
                  titl = "YT xmap RF")
YTKB <- conv_plot(data = as.data.frame(ccm_results[[22]]$ccm_tp_zero[[1]]),
                  titl = "YT xmap KB")
YTSB <- conv_plot(data = as.data.frame(ccm_results[[23]]$ccm_tp_zero[[1]]),
                  titl = "YT xmap SB")
YTBO <- conv_plot(data = as.data.frame(ccm_results[[24]]$ccm_tp_zero[[1]]),
                  titl = "YT xmap BO")
YTTU <- conv_plot(data = as.data.frame(ccm_results[[25]]$ccm_tp_zero[[1]]),
                  titl = "YT xmap TU")


#TU->
TURF <- conv_plot(data = as.data.frame(ccm_results[[26]]$ccm_tp_zero[[1]]),
                  titl = "TU xmap RF")
TUKB <- conv_plot(data = as.data.frame(ccm_results[[27]]$ccm_tp_zero[[1]]),
                  titl = "TU xmap KB")
TUSB <- conv_plot(data = as.data.frame(ccm_results[[28]]$ccm_tp_zero[[1]]),
                  titl = "TU xmap SB")
TUBO <- conv_plot(data = as.data.frame(ccm_results[[29]]$ccm_tp_zero[[1]]),
                  titl = "TU xmap BO")
TUYT <- conv_plot(data = as.data.frame(ccm_results[[30]]$ccm_tp_zero[[1]]),
                  titl = "TU xmap YT")

norm_convergence <- 
  plot_spacer() + RFKB + RFSB + RFBO + RFYT + RFTU +
  KBRF + plot_spacer() + KBSB + KBBO + KBYT + KBTU +
  SBRF + SBKB + plot_spacer() + SBBO + SBYT + SBTU +
  BORF + BOKB + BOSB + plot_spacer() + BOYT + BOTU +
  YTRF + YTKB + YTSB + YTBO + plot_spacer() + YTTU +
  TURF + TUKB + TUSB + TUBO + TUYT + plot_spacer() +
  plot_layout(ncol = 6)

png(file = "ConvergencePlots_Norm.png",
    width = 1000, height = 750)
norm_convergence
dev.off()

rm(list = setdiff(ls(), c("ccm_matrix", "ccm_significance", "bestE_matrix", "ccm_results")))

#save the environment as an .Rdata file that can be used in CCM code
save.image("TCombo_NormDaily_ccm_webs_SCB3.RData")


#plot significance testing results

titl <- c("RF xmap KB", "RF xmap SB", "RF xmap BO", "RF xmap YT", "RF xmap TU",
          "KB xmap RF", "KB xmap SB", "KB xmap BO", "KB xmap YT", "KB xmap TU",
          "SB xmap RF", "SB xmap KB", "SB xmap BO", "SB xmap YT", "SB xmap TU",
          "BO xmap RF", "BO xmap KB", "BO xmap SB", "BO xmap YT", "BO xmap TU",
          "YT xmap RF", "YT xmap KB", "YT xmap SB", "YT xmap BO", "YT xmap TU",
          "TU xmap RF", "TU xmap KB", "TU xmap SB", "TU xmap BO", "TU xmap YT")


plots <- list()

for(i in 1:length(ccm_results)){
  plots[[i]] <- ggplot() +
    geom_density(data = ccm_results[[i]]$ccm_surrogate[[1]], aes(x = rho), 
                 fill = "gray40") +
    geom_vline(data = ccm_results[[i]]$ccm_tp_zero[[1]], aes(xintercept = tail(rho,1)),
               color = "red", size = 1) +
    labs(title = titl[i]) +
    theme_classic() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(),
          plot.title = element_text(size = 12))
  
}


norm_significance <- 
  plot_spacer() + plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] +
  plots[[6]] + plot_spacer() + plots[[7]] + plots[[8]] + plots[[9]] + plots[[10]] +
  plots[[11]] + plots[[12]] + plot_spacer() + plots[[13]] + plots[[14]] + plots[[15]] +
  plots[[16]] + plots[[17]] + plots[[18]] + plot_spacer() + plots[[19]] + plots[[20]] +
  plots[[21]] + plots[[22]] + plots[[23]] + plots[[24]] + plot_spacer() + plots[[25]] +
  plots[[26]] + plots[[27]] + plots[[28]] + plots[[29]] + plots[[30]] + plot_spacer() +
  plot_layout(ncol = 6)



png(file = "SignifancePlots_Norm.png",
    width = 1000, height = 750)
norm_significance
dev.off()




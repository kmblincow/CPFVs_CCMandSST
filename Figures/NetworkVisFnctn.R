#Kayla Blincow
#2/6/2020

#GOAL:
#Create a function that will take the ccm_webs.Rdata files, and convert them to 
#network visualizations.

rm(list = ls())

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(visNetwork)
library(networkD3)
library(RColorBrewer)



ccm_plot <- function(infile, siglevel, edgecolor, outfile){

  #load the data file
  load(infile)
  
  #remove any connections that did not show convergence
  if(infile == "TCombo_WarmDaily_ccm_webs_SCB3.RData"){
    ccm_matrix[2,4] <- 0
  }
  
  #only pull out significant results from ccm_matrix
  for(i in 1:nrow(ccm_matrix)){
    for(j in 1:ncol(ccm_matrix)){
      if(ccm_significance[i,j] > siglevel){
        ccm_matrix[i,j] <- 0
      }
    }
  }
  
  
  
  #convert the ccm_matrix to the right format
  nodes <- as.data.frame(colnames(ccm_matrix))
  colnames(nodes) <- "label"
  
  nodes <- nodes %>% rowid_to_column("id")
  
  nodes <- as.tbl(nodes)
  
  #create a matrix for output
  edges <- matrix(NA, nrow(ccm_matrix)^2, 3)
  
  edges[,1:2] <- rep(1:ncol(ccm_matrix), nrow(ccm_matrix))
  edges[,1] <- sort(edges[,1])
  
  #populate third column with the rho values from the cross map
  for(i in 1:nrow(edges)){
    edges[i,3] <- ccm_matrix[edges[i,2], edges[i,1]]
  }
  
  
  edges <- as.data.frame(edges)
  colnames(edges) <- c("driver", "response", "weight")
  edges <- edges[edges$weight!=0,]
  
  #set my color scheme for nodes
  mycolor <- c(gray.colors(6))
  
  #plot the results
  ccm_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  ccm_tidy  <- tbl_graph(nodes = nodes, edges = edges, directed = T)
  ccm_igraph_tidy <- as_tbl_graph(ccm_igraph)
  
  
  
  p1 <- ggraph(ccm_igraph, layout = "linear") + 
    geom_edge_arc(aes(width = weight, alpha = weight), color = edgecolor) + 
    scale_edge_width(range = c(1, 5), 
                     breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0,1)) +
    scale_edge_alpha(range = c(0, 1), 
                     breaks = c(0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0,1)) +
    geom_node_point(size = 15, color = "gray10")+
    geom_node_text(aes(label = label), size = 5, color = "white") +
    labs(edge_width = "rho", edge_alpha = "rho") +
    theme_graph() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16)) 
  #stupid font warnings...
  
  png(outfile, 
      units="in", 
      width=7.5, 
      height=8, 
      pointsize=12, 
      res=400)
  suppressWarnings(print(p1))
  dev.off()
}


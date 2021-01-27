#Kayla Blincow
#5/19/2020
#Playing around with circle diagrams to leave gaps like I want...

#clear my workspace
rm(list = ls())



chord <- function(infile, outfile){
  png(outfile, 
      units="in", 
      width=10, 
      height=10, 
      pointsize=12, 
      res=400)
  
  # libraries
  library(circlize)
  library(viridis)
  library(tidyverse)
  
  #load my data
  load(infile)
  
  #set my colors
  mycolor <- c(viridis(5, alpha = 1, begin = 0, end = 0.5), 
               viridis(1, alpha = 1, begin = 0.8, end = 0.8))
  
  #name rows of my matrix, same as the columns
  rownames(ccm_matrix) <- colnames(ccm_matrix)
  
  #ccm_signficance matrix
  for(i in 1:nrow(ccm_matrix)){
    for(j in 1:ncol(ccm_matrix)){
      if(ccm_significance[i,j] > 0.05){
        ccm_matrix[i,j] <- 0
      }
    }
  }
  
  #prep my node key
  nodes <- as.data.frame(cbind(seq(1:6), rownames(ccm_matrix), mycolor))
  names(nodes) <- c("order", "species", "mycolor")
  nodes <- arrange(nodes, order)
  nodes$species <- factor(nodes$species, levels = nodes$species)
  
  #sort my matrix properly
  ccm_matrix <- ccm_matrix[levels(nodes$species), levels(nodes$species)]
  
  
  #define ranges of circos sector and tehir colors
  nodes$xmin <- 0
  nodes$xmax <- rowSums(ccm_matrix) + colSums(ccm_matrix)
  n <- nrow(nodes)
  nodes$rcol <- mycolor
  nodes$lcol <- adjustcolor(nodes$rcol, alpha = 0.6)
  
  #plot sectors (outer part)
  par(mar = rep(1,4))
  circos.clear()
  
  #Basic circos graphic parameters
  circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0),
             start.degree = 90, gap.degree = 2,
             points.overflow.warning = F,
             canvas.xlim = c(-1.2, 1.2))
  
  #sector details
  circos.initialize(factors = nodes$species, 
                    xlim = cbind(nodes$xmin, 10))
  
  #Plot sectors
  circos.trackPlotRegion(ylim = c(0, 1), factors = nodes$species, 
                         track.height = 0.1,
                         #panel function for each sector
                         panel.fun = function(x,y){
                           name = get.cell.meta.data("sector.index")
                           i = get.cell.meta.data("sector.numeric.index")
                           xlim = get.cell.meta.data("xlim")
                           ylim = get.cell.meta.data("ylim")
                           
                           #plot species labels
                           circos.text(x = mean(xlim), y = 2, labels=name,
                                       cex = 1.5)
                           
                           #plot main sector
                           circos.rect(xleft = xlim[1], ybottom = ylim[1],
                                       xright = xlim[2], ytop = ylim[2],
                                       col = nodes$rcol[i], 
                                       border = nodes$rcol[i])
                           
                           #blank in part of main sector
                           circos.rect(xleft = xlim[1], ybottom = ylim[1],
                                       xright = xlim[2]/2,
                                       ytop = ylim[1] + 0.3, col = "white",
                                       border = nodes$rcol[i])
                           
                           #plot axis
                           # circos.axis(labels.cex = 0.6, direction = "outside",
                           #             major.at = seq(from = 0, 
                           #                            to = floor(nodes$xmax)[i],
                           #                            by = 0.2), 
                           #             minor.ticks = 1, 
                           #             labels.away.percentage = 0.15)
                         })
  
  
  #Plot links (inner part)
  
  #Add sum values to nodes, marking the x-position of the first links
  # out (sum1) and in (sum2). Updated for further links in loop below
  nodes$sum1 <- colSums(ccm_matrix)
  nodes$sum2 <- numeric(n)
  
  #Create a dataframe of the lfow matrix sorted by flow size, to allow largest 
  #flow plotted first
  df2 <- cbind(as.data.frame(ccm_matrix), orig = rownames(ccm_matrix), 
               stringsAsFactors = F)
  df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
                 timevar="dest", time=rownames(ccm_matrix),  
                 v.names = "ccm_matrix")
  #df2 <- arrange(df2, desc(ccm_matrix))
  
  #remove self links
  df2 <- df2[!df2$orig == df2$dest,]
  
  
  
  
  #trying something
  nodes$sum1a <- 5
  nodes$sum2a <- 0
  
  #plot links
  for(k in 1:nrow(df2)){
    #i,j reference of flow matrix
    i <- match(df2$orig[k], nodes$species)
    j <- match(df2$dest[k], nodes$species)
    
    #plot link
    if(df2$ccm_matrix[k] > 0){
      circos.link(sector.index1 = nodes$species[i], 
                  point1 = c(nodes$sum1a[i], nodes$sum1a[i] + 
                               abs(ccm_matrix[i, j])),
                  sector.index2 = nodes$species[j], 
                  point2 = c(nodes$sum2a[j], nodes$sum2a[j] + 
                               abs(ccm_matrix[i, j])),
                  col = nodes$lcol[i], directional = 1, 
                  arr.type = "big.arrow")
    } else {
      NULL
    }
    
    #update sum1 and sum2 for use when plotting the next link
    nodes$sum1a[i] = nodes$sum1a[i] + 1
    nodes$sum2a[j] = nodes$sum2a[j] + 1
  }
  
  dev.off()
  
}

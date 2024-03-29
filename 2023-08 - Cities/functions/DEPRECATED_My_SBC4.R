# =========================================================================
# SBC.R

# Created by: Channin Songchon (cs127@hw.ac.uk) & Gordon Aitken (ga41@hw.ac.uk)
# Created: 2021-06-28

# Last revised: 2021-07-05
# Last revised by: Gordon Aitken (ga41@hw.ac.uk)
# =========================================================================

rm(list = ls()); cat("\014"); gc() # Clear workspace
pacman::p_load(tidyverse,data.table, ggplot2, scales, plotly, igraph, itertools, openxlsx, tictoc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$`path`))
tic("Runtime")

#Load data and convert to a graph object in igraph
df <- read.xlsx("USAH_ADJW.xlsx", sheet = 1)
df <- df[,-1]
df[is.na(df)] <- 0
df <- as.matrix(df)
g <- graph_from_adjacency_matrix(df, mode = "undirected", weighted = TRUE)


# create sbc function
sbc <- function(graph, undirected){
  #if(undirected == TRUE) mode <- "all" else mode <- "out"
  df <- as_adjacency_matrix(graph, type = c("both"),
                           attr = "weight", edges = FALSE, names = TRUE)
  df[is.na(df)] <- 0
  nodes = V(graph)$name
  outsbc <- data.frame(id = as.character(nodes),
                       value = NA, stringsAsFactors = FALSE)
  outsbc$id <- gsub(".", " ", outsbc$id, fixed = TRUE)
  rownames(outsbc) <- outsbc$id
  outsbc$id <- NULL
  
  #Shortest path length
  if(undirected == TRUE){
    sp <- shortest.paths(graph, mode = "all")
  } else{
    sp <- shortest.paths(graph, mode = "out")
  }
  
  #delete 1 vertex, calculate path length
  #Maybe convert to lapply?
  for (i in 1:nrow(outsbc)){
    df2 <- df
    df2[i,] = df2[,i] = 0.0
    df2 <- data.matrix(df2, rownames.force = NA)

    if(undirected == TRUE){
      g2 <- graph_from_adjacency_matrix(df2, mode = "undirected", weighted = TRUE)
      sp2 <- shortest.paths(g2, mode = "all")
    } else{
      g2 <- graph_from_adjacency_matrix(df2, mode = "directed", weighted = TRUE)
      sp2 <- shortest.paths(g2, mode = "out")
    }
    
    diff <- sp2-sp
    diff[is.infinite(diff)] <- NA
    sumval = sum(diff,na.rm=TRUE)
    outsbc$value[i] = sumval
  }
  return(outsbc)

}

output = sbc(g, undirected = TRUE)

write.xlsx(output, "sbc_undirected.xlsx",col.names=TRUE, row.names=TRUE, overwrite = TRUE)

toc()

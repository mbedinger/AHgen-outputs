# =========================================================================
# UDR_Hulme.R

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: not dated, but certainly prior to publication in 2020-09
# =========================================================================

# PREPARE ENVIRONMENT -----------------------------------------------------

# Load required packages
library(igraph)
library(tnet)
library(magrittr)
library(visNetwork)
library(data.table)
library(dplyr)
library(CINNA)


# READ IN DATA ------------------------------------------------------------

# Load data from adjacency matrix CSV file (Master List)
dat = read.csv(file("Hulme_Flood_CSV_18_02_2019.csv"), 
               header = TRUE, row.names = 1, check.names = FALSE)


# WORK WITH THE DATA AS MATRICES-------------------------------------------------------------------------------------

# Define start and end of each abstraction hierarchy level
start_FP <- 1
start_VPM <- 9
end_VPM <- 20
start_GF <- 21
end_GF <- 49
start_ORP <- 50
end_ORP <- 127
end_PO <- 193

# Coerce the data sets as (sub-)matrices
m_FP_VPM = as.matrix(dat[start_FP:end_VPM,start_FP:end_VPM])
m_VPM_GF = as.matrix(dat[start_VPM:end_GF,start_VPM:end_GF])
m_GF_ORP = as.matrix(dat[start_GF:end_ORP,start_GF:end_ORP])
m_ORP_PO = as.matrix(dat[start_ORP:end_PO,start_ORP:end_PO])

# Create R objects for the sub-matrices
MASTER_FP_VPM = graph.adjacency(m_FP_VPM, mode = "undirected", weighted = TRUE)
MASTER_VPM_GF = graph.adjacency(m_VPM_GF, mode = "undirected", weighted = TRUE)
MASTER_GF_ORP = graph.adjacency(m_GF_ORP, mode = "undirected", weighted = TRUE)
MASTER_ORP_PO = graph.adjacency(m_ORP_PO, mode = "undirected", weighted = TRUE)

# Get sub-matrices as edgelists
MASTER_FP_VPM_edgelist <- get.data.frame(MASTER_FP_VPM)
head(MASTER_FP_VPM_edgelist)
MASTER_VPM_GF_edgelist <- get.data.frame(MASTER_VPM_GF)
head(MASTER_VPM_GF_edgelist)
MASTER_GF_ORP_edgelist <- get.data.frame(MASTER_GF_ORP)
head(MASTER_GF_ORP_edgelist)
MASTER_ORP_PO_edgelist <- get.data.frame(MASTER_ORP_PO)
head(MASTER_ORP_PO_edgelist)


# CREATE OVERALL NETWORK FROM INDIVIDUAL EDGELISTS-------------------------------------------------------------------

# Bind edgelists together into single abstraction hierarchy network
MASTER_all_edgelist <- do.call(rbind, lapply(list(MASTER_FP_VPM_edgelist, MASTER_VPM_GF_edgelist, MASTER_GF_ORP_edgelist, 
                                               MASTER_ORP_PO_edgelist), function(x) setNames(x, c("1","2","3"))))
MASTER <- graph.data.frame(d = MASTER_all_edgelist, directed = FALSE)

# Write overall edgelist with weights to CSV file
write.csv(get.data.frame(MASTER), file = "MASTER_all_edgelist.csv")


# ADD ABSTRACTION HIERARCHY LEVELS------------------------------------------------------------------------------------

# Assign abstraction hierarchy levels as layers (primarily for ordering in visualisations)
MASTER_layers_df <- unique( rbind(
  expand.grid( ID = MASTER_FP_VPM_edgelist[,1], Layer = 1),
  expand.grid( ID = MASTER_FP_VPM_edgelist[,2], Layer = 2),
  expand.grid( ID = MASTER_VPM_GF_edgelist[,1], Layer = 2),
  expand.grid( ID = MASTER_VPM_GF_edgelist[,2], Layer = 3),
  expand.grid( ID = MASTER_GF_ORP_edgelist[,1], Layer = 3),
  expand.grid( ID = MASTER_GF_ORP_edgelist[,2], Layer = 4),
  expand.grid( ID = MASTER_ORP_PO_edgelist[,1], Layer = 4),
  expand.grid( ID = MASTER_ORP_PO_edgelist[,2], Layer = 5)))

# Add abstraction hierarchy levels/layers to dataframe
MASTER_layers <- setNames(MASTER_layers_df$Layer, MASTER_layers_df$ID)
V(MASTER)$layer <- MASTER_layers[V(MASTER)$name]


# CALCULATE UNWEIGHTED VERTEX BETWEENNESS CENTRALITY-----------------------------------------------------------------

# Ensure network is described in dataframes for nodes and edges
MASTER_all_nodes <- get.data.frame(MASTER, what="vertices")
MASTER_all_edges <- get.data.frame(MASTER, what="edges")

# Calculate & create R object for results of basic (unweighted) vertex betweenness centrality
vertex_UW_betweenness_MASTER <- betweenness(MASTER, directed = FALSE, weights = NA)

# Add unweighted vertex betweenness centrality results to nodes dataframe
MASTER_all_nodes$unweightedbetweenness <- vertex_UW_betweenness_MASTER

# Order nodes in new master results file by layer, then alphabetically by name (for consistency of reporting)
attach(MASTER_all_nodes)
MASTER_RESULTS <- MASTER_all_nodes[order(layer,name),]
detach(MASTER_all_nodes)


# CALCULATE WEIGHTED VERTEX BETWEENNESS CENTRALITY--------------------------------------------------------------------

# Load original data file for use in tnet (as this package can conflict with igraph, i.e. igraph objects do not work)
dat_tnet = read.csv(file("Hulme_Flood_CSV_18_02_2019.csv"), 
                    header = TRUE, row.names = 1, check.names = FALSE)

# Create R object for tnet data
MASTER_tnet <- dat_tnet

# Calculate & create R object for results of weighted vertex betweenness centrality
symmetrise_w(MASTER_tnet, method = "MAX")
vertex_W_betweenness_MASTER <- betweenness_w(MASTER_tnet, directed = NULL, alpha = 0.5)

# Add weighted vertex betweenness centrality results to nodes dataframe
MASTER_all_nodes$tnetnodeid <- colnames(dat)
MASTER_all_nodes$weightedbetweenness <- vertex_W_betweenness_MASTER[,2]

# Order nodes in new master results file by layer, then alphabetically by name (for consistency of reporting)
attach(MASTER_all_nodes)
MASTER_WBC_RESULTS <- MASTER_all_nodes[order(layer,tnetnodeid),]
detach(MASTER_all_nodes)

# Add weighted vertex betweenness centrality results to master results file
MASTER_RESULTS$nodeid <- MASTER_WBC_RESULTS[,4]
MASTER_RESULTS$weightedbetweenness <- MASTER_WBC_RESULTS[,5]


# WRITE RESULTS FILE-------------------------------------------------------------------------------------------------

# Write results to MASTER_RESULTS file
write.csv(MASTER_RESULTS, file = "Hulme_Flood_RESULTS_16_10_2019.csv")


# OTHER WORK-IN-PROGRESS---------------------------------------------------------------------------------------------

# Distance (weighted)
distance_w(MASTER_tnet, directed = NULL, gconly = TRUE, subsample = 1, seed = NULL)

# Write results to dataframe file
write.csv(distance_w(MASTER_tnet, directed = NULL, gconly = TRUE, subsample = 1, seed = NULL),
          file = "MASTER_distances.csv")

# Shortest paths
# All shortest paths
# Calculate CINNA centralities
# Calculate in-level UWVBC rank
# Calculate overall UWVBC rank
# Calculate in-level WVBC rank
# Calculate overall WVBC rank


# PLOT STATIC ABSTRACTION HIERARCHY VISUALISATIONS------------------------------------------------------------------------

# Create layout for correct visualisation of abstraction hierarchy levels
layout.k_partite <- function(MASTER) {
  l <- layout.sugiyama(MASTER)$layout[,1:2]
  l[,2] <- V(MASTER)$layer
  l[,2] <- - l[,2] + 1 + max(l[,2])
  l
}

# Plot standard abstraction hierarchy
plot(MASTER, layout = layout.k_partite(MASTER), 
     vertex.shape = "circle", vertex.color = "blue", vertex.size = 3, 
     vertex.label = "0", vertex.label.font = 2, vertex.label.color = "gray40", 
     vertex.label.cex = .7, edge.color = "gray85")

# Plot abstraction hierarchy with node size reflecting (unweighted) vertex betweenness centrality
plot(MASTER, layout = layout.k_partite(MASTER), 
     vertex.shape = "circle", vertex.color = "blue", vertex.size = vertex_UW_betweenness_MASTER/100, 
     vertex.label = "0", vertex.label.font = 2, vertex.label.color = "gray40", 
     vertex.label.cex = .7, edge.color = "gray85")


# PLOT INTERACTIVE ABSTRACTION HIERARCHY VISUALISATIONS e.g. https://rstudio-pubs-static.s3.amazonaws.com/157501_93a72a58ec614946901e10edf78c1384.html

# Define interactive part of visualisation
MASTER_all_edges_interactive <- MASTER_all_edges
MASTER_all_nodes_interactive <- data.frame(id = MASTER_all_nodes$name, 
                                           title = MASTER_all_nodes$name, 
                                           group = MASTER_all_nodes$layer,
                                           UWbetweenness = MASTER_all_nodes$unweightedbetweenness,
                                           percentileUWbetweenness = MASTER_all_nodes$UWVBCpercentile)

visNetwork(MASTER_all_nodes_interactive, MASTER_all_edges_interactive, width = "100%") %>%
  visOptions(selectedBy = "UWbetweenness", highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend()
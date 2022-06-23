# =========================================================================
# NUS_1_generateTemplate.R

# Note: Part 1

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2020-10-10
# Last updated: 2021-02-03
# =========================================================================

# 1. PREPARE ENVIRONMENT --------------------------------------------------
rm(list = ls()); cat("\014") 

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(igraph)
library(readxl)
library(tnet)
library(survival)
library(data.table)
library(magrittr)
library(tools)
library(stringi)
library(janitor)

source("functions/functions.R")

ls()


# 2. FUNCTIONS ------------------------------------------------------------
# Function to check for and return any edges which may not be symmetrical i.e. 
# the row-wise 'Cultural heritage and sense of place' information does not match 
# the column-wise 'Cultural heritage and sense of place' information
find_unsymmetricalEdges <- function(adjMat) {
  
  vNames <- adjMat %>% pull(vName)
  
  noMatch <- list()
  
  for(x in vNames) {
    # add new column to identify if from rowwise or columnwise in adjMat - would then need to only select duplicated(step3) without this column
    step1 <- 
      adjMat %>% select(x) %>% t() %>% as.data.frame %>% setNames(vNames) %>% 
      setDT(keep.rownames = TRUE) %>% rename(vName = rn)
    
    step2 <- adjMat %>% select(-level, -levelName) %>% filter(vName == x)
    
    step3 <- step1 %>% rbind(step2)
    
    indDuplicatedVec <- 
      duplicated(step3) | duplicated(step3, fromLast = TRUE)
    
    noMatch[[paste0("noMatch ", x)]] <- step3[!indDuplicatedVec, ]
    
  }
  
  noMatch
  
  noMatch <- noMatch %>% do.call("rbind", .)
  
  return(noMatch)
  
}

# Function to check for and return any nodes which have redundant sets of edge
# connections
find_matchingEdges <- function(adjMat) {
  
  # Create vector of vNames
  vNames <- adjMat %>% pull(vName)
  
  # Create basic vInfo
  vInfo_template <- adjMat %>% select(level, levelName, vName)
  
  # Create igraph object
  igraph <- adjMat %>% adjMat_to_igraph(vInfo = vInfo_template)
  
  # Create list object to populate with each vertex's neighbors
  neighbors <- list()
  
  # Populate list with each vertex's neighbors
  for(x in vNames) {
    
    neighbors[[x]] <- igraph %>% neighbors(x) %>% as.vector %>% as.matrix
    
  }
  
  # Return neighbors list
  neighbors
  
  # Find vertices with duplicate neighbors
  indDuplicatedVec <- 
    duplicated(neighbors) | duplicated(neighbors, fromLast = TRUE)
  
  # Return position of vertices with duplicate neighbors
  indDuplicatedVec %>% which()
  
  ## Final step to return vNames of these vector numbers
  
}

# Function to assign subnetworks, based on assignment at objects level and
# propagating assignment upward to all connected vertices
assign_subnetworks <- function(
  edgelist_template, vInfo_template_full, subnetworkColName) {
  
  # Create dummy edge list for subnetwork assignment
  vName <- vInfo_template_full$vName
  edgelist_template$fromlayer <- 
    vInfo_template_full$level[match(edgelist_template$from, vName)] # Edges from
  edgelist_template$tolayer <- 
    vInfo_template_full$level[match(edgelist_template$to, vName)] # Edges to
  
  # Layer 5 key from input data
  key <- 
    list(key_L5 = vInfo_template_full %>% filter(level == 5) %>%
           select('vName', subnetworkColName))
  
  
  # Generate water key by loop
  for(i in 5:2) {
    
    temp <- key[[paste0("key_L", i)]] %>% as.data.frame
    
    key[[paste0("key_L", i-1)]] <- 
      edgelist_template %>%
      mutate(x = temp[, subnetworkColName][match(edgelist_template$to, temp$'vName')]) %>%
      as_tibble() %>%
      setNames(c("layer", "from", "to", "weight", "fromlevel", "tolevel", 
                 subnetworkColName)) %>%
      filter(tolevel == i) %>%
      select(from, subnetworkColName) %>%
      distinct() %>%
      arrange(from, desc(!! rlang::sym(subnetworkColName))) %>%
      filter(!duplicated(from)) %>%
      rename(`vName` = from)
    
  }
  
  key
  
  key = key %>% do.call("rbind", .)
  
  vInfo_template_full %>%
    select(-all_of(subnetworkColName)) %>%
    left_join(key, by = "vName") %>% select(vName, subnetworkColName)
  
}

# Function to generate vInfo_template_full with updated subnetwork assignment
update_subnetworks <- function(
  edgelist_template, vInfo_template_full) {
  
  # Extract vInfo_template_full column names
  namesInfo <- names(vInfo_template_full)
  
  # Find subnetwork column names
  namesSubnetworks <- namesInfo[str_detect(namesInfo, "subnetwork")]
  
  newSubnetworks <-
    lapply(namesSubnetworks, function(x) { 
      assign_subnetworks(
        edgelist_template = edgelist_template,
        vInfo_template_full = vInfo_template_full,
        subnetworkColName = x) }) %>% 
    reduce(full_join, by = "vName")
  
  output <-
    vInfo_template_full %>% select(-all_of(namesSubnetworks)) %>% 
    full_join(newSubnetworks, by = "vName")
  
}

# Function to get results for WVBC, overall rank, & rank within level
source("functions/function_getResultsNew.R")

# Function to summarise USAH
# This is a function to summarise the number of vertices at each layer of an
# abstraction hierarchy; and the number of edges at each level of an 
# abstraction hierarchy
summarise_USAH <- function(vInfo, edgelist) {
  
  vertices <- 
    vInfo %>% group_by(level, levelName) %>% count %>% adorn_totals("row")
  edges <- 
    edgelist %>% group_by(layer) %>% count %>% adorn_totals("row")
  
  list("vertices" = vertices, "edges" = edges)
  
}

# Function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 3. READ IN INPUT DATA ---------------------------------------------------
# Read in template USAH adjacency matrix
adjMat_template = read_adjMat(
  "inputs/USAH_template_adjMat_20201214.xlsx") ## Specify filename

# Create template USAH vInfo (basic)
vInfo_template = adjMat_template %>% select(level, levelName, vName)

# Read in template USAH vInfo (full)
vInfo_template_full = read_xlsx(
  "inputs/USAH_template_baseline_vInfo_full_20201214.xlsx") ## Specify filename


# 4. GENERATE USAH_TEMPLATE_BASELINE OUTPUT -------------------------------
# Confirm adjMat has completely symmetrical edges i.e. there are no input errors
unsymmetricalEdges = adjMat_template %>% find_unsymmetricalEdges

# Confirm adjMat has no vertices with redundant sets of edges i.e. no vertices
# are connected to the same set of other vertices between two levels
# If at the l4ORP_l5PO layer, those vertices should be 
# 'combined' in the USAH; if at other layers, consider combining
matchingEdges = adjMat_template %>% find_matchingEdges

# Generate edgelist
edgelist_template = 
  adjMat_template %>% adjMat_to_edgelist(vInfo = vInfo_template)

# Generate igraph
igraph_template = adjMat_template %>% adjMat_to_igraph(vInfo = vInfo_template)

# OPTIONAL - if changes have been made to how objects have been assigned to
# different subnetworks, update assignment of subnetworks to connected vertices
# and generate a new vInfo_template_full
vInfo_template_full = 
  update_subnetworks(
    edgelist_template = edgelist_template, 
    vInfo_template_full = vInfo_template_full)

# Generate output for USAH_template_baseline
USAH_template_baseline = 
  list("vInfo" = vInfo_template_full, # Attach additional info for vertices
       "vIncluded" = vInfo_template, # Attach dataframe of included vertices (all for template USAH)
       "vExcluded" = list(), # Attach dataframe of excluded vertices (empty for template USAH)
       "adjMat" = adjMat_template, # Attach adjacency matrix for template USAH
       "edgelist" = edgelist_template, # Attach edgelist for template USAH
       "igraph" = igraph_template, # Attach igraph for template USAH
       "results" = getResults(igraph = igraph_template, vInfo = vInfo_template), # Create results for template USAH
       "summary" = summarise_USAH(vInfo = vInfo_template, # Create summary of vertices by level
                                  edgelist = edgelist_template)) # Create summary of edges by layer


# Inspect elements
USAH_template_baseline$vInfo
USAH_template_baseline$vIncluded
USAH_template_baseline$vExcluded
USAH_template_baseline$adjMat
USAH_template_baseline$edgelist
USAH_template_baseline$igraph
USAH_template_baseline$results
USAH_template_baseline$summary


# 5. SAVE OUTPUTS ---------------------------------------------------------
# Write output to RDS file
USAH_template_baseline %>% saveRDS(filenameTimestamp(
  prefix = "outputs/USAH_1.0_template_baseline", extension = ".RDS")) ## Specify filename

# If subnetwork assignment has been updated, write this out to CSV for future use
USAH_template_baseline$vInfo %>% write.csv(filenameTimestamp(
  prefix = "outputs/USAH_1.0_template_baseline_vInfo_full", extension = ".csv")) ## Specify filename

# If you wanted to pull out just the results and write this to a CSV
USAH_template_baseline$results %>% write.csv(filenameTimestamp(
  prefix = "outputs/USAH_1.0_template_baseline_results", extension = ".csv")) ## Specify filename
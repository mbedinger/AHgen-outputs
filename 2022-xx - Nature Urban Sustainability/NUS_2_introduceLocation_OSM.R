# =========================================================================
# NUS_2_introduceLocation_OSM.R

# Note: Part 2

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
# Function to remove hanging vertices after initial adjustments have been made
# Note this is written in such a way that apply_OSMtidyOutput is best applied before apply_100RCindicators, not tested backward order yet
weight_hangingVertices <- function(edgelist) {
  
  # Check for ORPs which have all links = 0.0000000001 and set all their links to GFs to 0.0000000001
  l4ORP_l5PO_excluded <- 
    edgelist %>% 
    filter(layer == "l4ORP_l5PO") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == 0.0000000001) %>%
    select(-weight)
  
  l4ORP_l5PO_included <- 
    edgelist %>% 
    filter(layer == "l4ORP_l5PO") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > 0.0000000001) %>%
    select(-weight)
  
  ORP_excluded <-
    dplyr::setdiff(l4ORP_l5PO_excluded, l4ORP_l5PO_included) %>% pull(from)
  
  if(is_empty(ORP_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(ORP_excluded) | to %in% all_of(ORP_excluded)) %>% # Set aside excluded ORP vertices
      mutate(weight = 0.0000000001) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weightEdges(edgesNew)
    
    return(edgelist)
    
  }
  
  l3GF_l4ORP_excluded <- 
    edgelist %>% 
    filter(layer == "l3GF_4ORP") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == 0.0000000001) %>%
    select(-weight)
  
  l3GF_l4ORP_included <- 
    edgelist %>% 
    filter(layer == "l3GF_l4ORP") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > 0.0000000001) %>%
    select(-weight)
  
  GF_excluded <-
    dplyr::setdiff(l3GF_l4ORP_excluded, l3GF_l4ORP_included) %>% pull(from)
  
  if(is_empty(GF_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(GF_excluded) | to %in% all_of(GF_excluded)) %>% # Set aside excluded GF vertices
      mutate(weight = 0.0000000001) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weightEdges(edgesNew)
    
    return(edgelist)
    
  }
  
  l2VPM_l3GF_excluded <- 
    edgelist %>% 
    filter(layer == "l2VPM_l3GF") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == 0.0000000001) %>%
    select(-weight)
  
  l2VPM_l3GF_included <- 
    edgelist %>% 
    filter(layer == "l2VPM_l3GF") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > 0.0000000001) %>%
    select(-weight)
  
  VPM_excluded <-
    dplyr::setdiff(l2VPM_l3GF_excluded, l2VPM_l3GF_included) %>% pull(from)
  
  if(is_empty(VPM_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(VPM_excluded) | to %in% all_of(VPM_excluded)) %>% # Set aside excluded VPM vertices
      mutate(weight = 0.0000000001) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weightEdges(edgesNew)
    
    return(edgelist)
    
  }
  
  l1FP_l2VPM_excluded <- 
    edgelist %>% 
    filter(layer == "l1FP_l2VPM") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight == 0.0000000001) %>%
    select(-weight)
  
  l1FP_l2VPM_included <- 
    edgelist %>% 
    filter(layer == "l1FP_l2VPM") %>% 
    select(from, weight) %>% 
    unique %>%
    filter(weight > 0.0000000001) %>%
    select(-weight)
  
  FP_excluded <-
    dplyr::setdiff(l1FP_l2VPM_excluded, l1FP_l2VPM_included) %>% pull(from)
  
  if(is_empty(FP_excluded) == TRUE) {
    
    return(edgelist)
    
  } else {
    
    edgesNew <- 
      edgelist %>%
      filter(from %in% all_of(FP_excluded) | to %in% all_of(FP_excluded)) %>% # Set aside excluded FP vertices
      mutate(weight = 0.0000000001) %>%
      rename(weightNew = weight)
    
    edgelist <- edgelist %>% weightEdges(edgesNew)
    
    return(edgelist)
    
  }
  
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

# Function to filter USAH objects by detection of descs in OSMtidy data
# Could be split into two parts:
# (a) identifying included & excluded object vertices
# this would allow for a manual validation of which vertices have been excluded inappropriately
# because of incomplete OSM data e.g. Edinburgh currently excludes Prisons and jails
# (b) knocking out excluded object vertices and then knocking out associated vertices at higher levels
apply_OSMtidyOutput <- function(
  OSMtidy_output, key, vInfo_template, edgelist_template) {
  
  objects_location <- 
    OSMtidy_output %>% select(desc) %>% unique %>% # Select desc column from OSMtidy output and find unique entries
    mutate(OSMtidyDetected = "TRUE") %>% # Create new column to flag that these unique desc were detected by OSMtidy
    full_join(key, by = "desc") %>% # Join this to the OSMtidy-AHgen key
    replace_na(list(OSMtidyDetected = "FALSE")) # Flag the desc from the key that were not detected by OSMtidy
  
  PO_included <-
    objects_location %>%
    filter(OSMtidyDetected == "TRUE" | OSMtidyStage == "alwaysIncluded") %>% # Include rows if the desc has been detected by OSMtidy OR the key says they should always be included
    select(physicalObject) %>% unique %>% rename(vName = physicalObject) # From this subset, create unique list of objects and rename column for compatibility with vInfo
  
  PO_template <-
    vInfo_template %>% filter(level == 5) %>% select(vName) # Create dummy generic vInfo for object vertices only
  
  PO_excluded <- 
    PO_template %>% dplyr::setdiff(PO_included) # Find object vertices that are in the generic vInfo but not in the location
  
  ## Confirm whether to break here to allow for a manual validation of excluded objects
  
  PO_excluded <- PO_excluded %>% pull(vName) # create vector of object names to be excluded
  
  edgesNew <-
    edgelist_template %>%
    filter(from %in% all_of(PO_excluded) | to %in% all_of(PO_excluded)) %>% # Set aside excluded object vertices from edgelist scenario
    mutate(weight = 0.0000000001) %>%
    rename(weightNew = weight)
    
  edgelist_location <- edgelist_template %>% weightEdges(edgesNew)
  
  # Check for hanging vertices which have all links downard weighted = 0.0000000001 and iteratively set upward links weighted = 0.0000000001 as relevant
  edgelist_location <- weight_hangingVertices(edgelist = edgelist_location)
  
  # Create dataframe of only the included vertices
  to <- edgelist_location %>% select(to, weight) %>% rename(vName = to)
  from <- edgelist_location %>% select(from, weight) %>% rename(vName = from)
  all_excluded <- 
    to %>% rbind(from) %>% unique() %>% table() %>% as.data.frame() %>% 
    filter(weight == 1, Freq == 0) %>% mutate_if(is.factor, as.character) %>% # not precise but works
    pull(vName)
  
  vInfo_location <- vInfo_template %>% filter(!vName %in% all_of(all_excluded)) # Keep track of included/excluded nodes even if they are not fully removed (just edges set to 0.0000000001)
  
  # Create igraph
  igraph_location <- edgelist_location %>% edgelist_to_igraph(vInfo_template) # Create location-specific igraph, because no vertices have been removed use vInfo_template

  # Create outputList
  outputList <- 
    list("keyCheck" = objects_location, # Attach information about how key was applied
         "vIncluded" = vInfo_location, # Attach dataframe of included vertices
         "vExcluded" = dplyr::setdiff(vInfo_template, vInfo_location), # Create and attach dataframe of excluded vertices
         "adjMat" = edgelist_location %>% edgelist_to_adjMat(vInfo = vInfo_template), # Create location-specific adjacency matrix, use vInfo_template because no nodes have been fully removed (just links set to 0.0000000001)
         "edgelist" = edgelist_location, # Attach location-specific edgelist
         "igraph" = igraph_location, # Attach location-specific igraph
         "results" = getResults(igraph = igraph_location, vInfo = vInfo_template), # Create location-specific results for weighted vertex betweenness centrality, use vInfo_template because no nodes have been fully removed (just links set to 0.0000000001)
         "summary" = summarise_USAH(vInfo = vInfo_template, # Create summary of vertices by level, use vInfo_template because no nodes have been fully removed (just links set to 0.0000000001)
                                    edgelist = edgelist_location)) # Create summary of edges by layer
  
  return(outputList)
  
}

# Function to apply location-specific indicators 
## Currently works only with USAH_input = USAH_template_baseline, need to make adjustments for any other USAH_input
apply_100RCindicators <- function(USAH_input, indicators) {
  
  USAH_input$edgelist <- USAH_input$edgelist %>% weightEdges(indicators)
  
  USAH_input$adjMat <- 
    USAH_input$edgelist %>% edgelist_to_adjMat(vInfo = USAH_input$vIncluded)
  
  USAH_input$igraph <- 
    USAH_input$edgelist %>% edgelist_to_igraph(vInfo = USAH_input$vIncluded)
  
  USAH_input$results <- 
    getResults(igraph = USAH_input$igraph, vInfo = USAH_input$vIncluded)
  
  USAH_input$summary <- 
    summarise_USAH(vInfo = USAH_input$vIncluded, edgelist = USAH_input$edgelist)
  
  return(USAH_input)
  
}

# Function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 3. READ IN INPUT DATA ---------------------------------------------------
# Read in USAH_template_baseline
USAH_template_baseline = read_rds(
  "outputs/USAH_1.0_template_baseline_20210203.RDS") ## Specify filename for USAH_template

# Read in OSMtidy output for location
OSMtidy_output = read_csv(
  "inputs/Edinburgh_OSMtidyDraft_20200918.csv") ## Specify filename for OSMtidy output

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv("inputs/OSM-AH-key_1.0_20201210.csv") ## Specify filename for OSMtidy-AHgen key

# Read in 100 Resilient Cities indicators
# indicators = read_indicatorsDatabase(
#   "inputs/100RCindicators_allCities_20200902.xlsx", "Edinburgh") ## Specify filename for indicators database and location name


# 4. APPLY OSMTIDY DATA ---------------------------------------------------
# Apply OSMtidy data to USAH
USAH_location_OSM = 
  apply_OSMtidyOutput(
    OSMtidy_output = OSMtidy_output, 
    key = key, 
    vInfo_template = USAH_template_baseline$vIncluded, ## Specify vInfo
    edgelist_template = USAH_template_baseline$edgelist) ## Specify edgelist you want to make adjustments to

# Inspect elements
USAH_location_OSM$keyCheck
USAH_location_OSM$vIncluded
USAH_location_OSM$vExcluded # Note that these are not fully removed from network, just all links weighted = 0.0000000001, thus will appear in edgelist, igraph, adjMat, results, etc.
USAH_location_OSM$adjMat
USAH_location_OSM$edgelist
USAH_location_OSM$igraph
USAH_location_OSM$results
USAH_location_OSM$summary


# 5. APPLY 100 RESILIENT CITIES INDICATORS --------------------------------
# This is under the assumption no vertices are removed from the top levels
# such that if OSMtidy outputs have already been applied, the keyCheck, 
# included, excluded, summary list elements remain the same as in step 1

# Apply 100 Resilient Cities indicators to USAH
# USAH_location_100RCApplied = 
#   apply_100RCindicators(
#     USAH_input = USAH_location_osmApplied, ## Specify USAH you want to make adjustments to
#     indicators = indicators)

# Inspect elements
# USAH_location_100RC$keyCheck
# USAH_location_100RC$vIncluded
# USAH_location_100RC$vExcluded
# USAH_location_100RC$adjMat
# USAH_location_100RC$edgelist
# USAH_location_100RC$igraph
# USAH_location_100RC$results
# USAH_location_100RC$summary


# 6. SAVE OUTPUTS ---------------------------------------------------------
# Write list to RDS file
USAH_location_OSM %>% saveRDS(filenameTimestamp(
  prefix = "outputs/USAH_1.0_Edinburgh_baseline-OSM", extension = ".RDS")) ## Specify filename

# If you wanted to pull out just the results and write this to a CSV
USAH_location_OSM$results %>% write.csv(filenameTimestamp(
  prefix = "outputs/USAH_1.0_Edinburgh_baseline-OSM_results", extension = ".csv")) ## Specify filename


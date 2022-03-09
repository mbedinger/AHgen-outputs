# =========================================================================
# NUS_4_introduceCOVID.R

# Note: Part 4

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
library(janitor)
library(stringi)

source("functions/functions.R")

ls()


# 2. FUNCTIONS ------------------------------------------------------------
# Function to create new edges for specific location and week
apply_data <- function(
  USAH_template_baseline, USAH_location_baseline, data, edgeGuide, location, week) {
  
  # Extract all column names
  names <- names(data)
  
  # Find week column names
  namesWeeks <- names[str_detect(names, "week")]
  
  # Create key to subdivide indicator by sic_sector where relevant
  key <- 
    data %>% 
    select(sic_sector) %>% 
    unique %>% 
    setDT(keep.rownames = TRUE) %>% 
    rename(sic_n = rn)
  
  # Tidy COVID-19 data
  data <-  
    data %>% 
    full_join(key, by = "sic_sector") %>%
    mutate(
      id_sub = paste(id, sic_n, sep = "_"),
      city = case_when(
        local_authority == "Edinburgh" | nhs_hb == "NHS Lothian" ~ "Edinburgh", 
        local_authority == "Glasgow" | nhs_hb == "NHS Greater Glasgow & Clyde" ~ "Glasgow")) %>%
    select(id_sub, name, sic_sector, city, all_of(namesWeeks))
  
  # Read in edge guide for specific phase of pandemic (pre-lockdown, lockdown, Phase 1, etc.)
  edges <- edgeGuide
  
  # Filter edgeGuide by vExcluded physical objects to remove links irrelevant for location
  if(
    is_empty(
      USAH_location_baseline$vExcluded %>% filter(level == 5)) == FALSE) {
    
    # Create vector of objects not present in location
    PO_excluded <- 
      USAH_location_baseline$vExcluded %>% filter(level == 5) %>% pull(vName)

    # Filter new edges to remove objects not present in location
    edges <- edges %>% filter(!to %in% all_of(PO_excluded))
    
  }
  
  # Find which indicators from data are used in new edges for specific phase
  ind_used <- edges %>% select(id_sub) %>% unique %>% arrange(id_sub) %>% pull(id_sub)
  
  # Subset COVID-19 data based on indicators relevant for a specific edgelist/phase
  data_phase <- data %>% filter(id_sub %in% ind_used)
  
  # Replace NAs with 1
  data_phase <- data_phase %>% mutate_at(vars(namesWeeks), ~replace_na(., 1)) ## Revisit this! Some may need a bespoke baseline figure (not == 1)
  
  # Filter for location
  data_location <- data_phase %>% filter(city == location | is.na(city)) ## insert "location"
  
  # Select week
  data_week <- data_location %>% select(id_sub, week) ## insert "week"
  
  # Join to edgelist guide for phase
  edges <- 
    edges %>% 
    full_join(data_week, by = "id_sub") %>% 
    select(layer, from, to, type_partA, type_partB, type_partC, week) %>% ## insert "week"
    rename(weightNew = week)
  
  if(
    is_empty(
      USAH_location_baseline$vExcluded %>% filter(level == 4)) == FALSE) {
    
    # Create vector of objects not present in location
    ORP_excluded <- 
      USAH_location_baseline$vExcluded %>% filter(level == 4) %>% pull(vName)
    
    ORP_readded <- 
      edges %>% filter(from %in% all_of(ORP_excluded)) %>% pull(from)
    
    l3GF_l4ORP_readded <-
      USAH_template_baseline$edgelist %>% 
      filter(to %in% all_of(ORP_readded)) %>%
      add_column(type_partA = NA, type_partB = NA, type_partC = NA) %>%
      rename(weightNew = weight)
    
    # Add edges missing from location baseline, from template to new edges
    edges <- edges %>% rbind(l3GF_l4ORP_readded)
    
  }
  
  return(edges)
  
}

# Function to remove hanging vertices after initial adjustments have been made
weight_hangingVertices <- function(edgelist) {
  
  # Check for ORPs which have all links = 0.001 and set all their links to GFs to 0.001
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
## This is a function to summarise the number of vertices at each layer of an
## abstraction hierarchy; and the number of edges at each level of an 
## abstraction hierarchy
summarise_USAH <- function(vInfo, edgelist) {
  
  vertices <- 
    vInfo %>% group_by(level, levelName) %>% count %>% adorn_totals("row")
  edges <- 
    edgelist %>% group_by(layer) %>% count %>% adorn_totals("row")
  
  list("vertices" = vertices, "edges" = edges)
  
}

# Function to generate USAH_scenario output based on new hazard edgelist
apply_scenario <- function(USAH_template_baseline, edgelist_scenario) {
  
  # Create dataframe of only the included vertices
  edges_to <- edgelist_scenario %>% select(to, weight) %>% rename(vName = to)
  edges_from <- edgelist_scenario %>% select(from, weight) %>% rename(vName = from)
  edges_all <- edges_to %>% rbind(edges_from) %>% unique()
  edges_included <- edges_all %>% filter(weight > 0.0000000001) %>% select(-weight)
  edges_excluded <- edges_all %>% filter(weight == 0.0000000001) %>% select(-weight)
  all_excluded <- dplyr::setdiff(edges_excluded, edges_included) %>% pull(vName)
  
  # Create list object for output
  USAH_scenario <- list()
  
  # Overwrite USAH_input with updated scenario elements
  USAH_scenario$vIncluded <- 
    USAH_template_baseline$vIncluded %>% filter(!vName %in% all_of(all_excluded)) # Keep track of included/excluded nodes even if they are not fully removed (just edges set to 0.001)
  USAH_scenario$vExcluded <- 
    USAH_template_baseline$vIncluded %>% filter(vName %in% all_of(all_excluded)) # Keep track of included/excluded nodes even if they are not fully removed (just edges set to 0.001)
  USAH_scenario$adjMat <- 
    edgelist_scenario %>% edgelist_to_adjMat(vInfo = USAH_template_baseline$vIncluded) # Create scenario-specific adjMat
  USAH_scenario$edgelist <- 
    edgelist_scenario # Attach scenario-specific edgelist
  USAH_scenario$igraph <- 
    edgelist_scenario %>% edgelist_to_igraph(vInfo = USAH_template_baseline$vIncluded) # Create scenario-specific igraph
  USAH_scenario$results <- 
    getResults(igraph = USAH_scenario$igraph, vInfo = USAH_template_baseline$vIncluded) # Create scenario-specific results based on weighted vertex betweenness centrality
  USAH_scenario$summary <- 
    summarise_USAH(vInfo = USAH_template_baseline$vIncluded, # Create summary of vertices by level
                   edgelist = edgelist_scenario) # Create summary of edges by layer
  
  return(USAH_scenario)
  
}

# Function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 3. READ IN INPUT DATA ---------------------------------------------------
# Read in template baseline USAH
USAH_template_baseline = readRDS(
  "outputs/USAH_1.0_template_baseline_20210203.RDS") ## Specify template baseline USAH with generic vInfo etc.

# Read in location-specific baseline USAH
USAH_location_baseline = readRDS(
  "outputs/USAH_1.1_Edinburgh_baseline-OSM-COVID_20210322-131717.RDS") ## Specify location baseline USAH

# Read in COVID-19 data
data = read_csv("inputs/linkWeightings_timeSeries_20210322.csv") ## Specify COVID dataset name

# Read in edge guide for pre-lockdown (ISO weeks 11-12)
edgeGuide_prelockdown_all = read_xlsx("inputs/edgeGuide_pre-lockdown_all_20210322.xlsx") ## Specify

# Read in edge guide for lockdown in Edinburgh (ISO weeks 13-22)
edgeGuide_lockdown_Edinburgh = read_xlsx("inputs/edgeGuide_lockdown_Edinburgh_20210322.xlsx") ## Specify

# Read in edge guide for lockdown in Glasgow (ISO weeks 13-22)
# edgeGuide_lockdown_Glasgow = read_xlsx("inputs/edgeGuide_lockdown_Glasgow_20201215.xlsx") ## Specify

# Read in edge guide for Phase 1 (ISO weeks 23 - 25)
edgeGuide_phase1_all = read_xlsx("inputs/edgeGuide_Phase1_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 2a (ISO week 26)
edgeGuide_phase2a_all = read_xlsx("inputs/edgeGuide_Phase2_PartA_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 2b (ISO weeks 27-28)
edgeGuide_phase2b_all = read_xlsx("inputs/edgeGuide_Phase2_partB_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3a (ISO week 29)
edgeGuide_phase3a_all = read_xlsx("inputs/edgeGuide_Phase3_partA_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3b (ISO weeks 30-31)
edgeGuide_phase3b_all = read_xlsx("inputs/edgeGuide_Phase3_partB_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3c (ISO weeks 32-33)
edgeGuide_phase3c_all = read_xlsx("inputs/edgeGuide_Phase3_partC_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3d (ISO week 34)
edgeGuide_phase3d_all = read_xlsx("inputs/edgeGuide_Phase3_partD_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3e (ISO week 35)
edgeGuide_phase3e_all = read_xlsx("inputs/edgeGuide_Phase3_partE_all_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3f in Edinburgh (ISO weeks 36-38)
edgeGuide_phase3f_Edinburgh = read_xlsx("inputs/edgeGuide_Phase3_partF_Edinburgh_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3g in Edinburgh (ISO weeks 39-40)
edgeGuide_phase3g_Edinburgh = read_xlsx("inputs/edgeGuide_Phase3_partG_Edinburgh_20210322.xlsx") ## Specify

# Read in edge guide for Phase 3f in Glasgow (ISO weeks 36-40)
# edgeGuide_phase3f_Glasgow = read_xlsx("inputs/edgeGuide_Phase3_partF_Glasgow_20201215.xlsx") ## Specify


# 4. GENERATE ADJUSTED EDGES AND WEIGHTS FOR LOCATION AND WEEK ------------
# Stitch together data and edgeGuide to generate new edge weights for scenario
# This is basically edgesNew, but the additional "type" columns allow you to
# filter for different variables e.g. if you want to introduce and compare
# healthcare system impacts / economic interventions independently
scenario = 
  apply_data(
    USAH_template_baseline = USAH_template_baseline, 
    USAH_location_baseline = USAH_location_baseline, 
    data = data, 
    edgeGuide = edgeGuide_phase3g_Edinburgh, ## Specify edgeGuide
    location = "Edinburgh", ## Specify location
    week = "week_40") ## Specify week

# 5. SUBSET DIFFERENT COVID SCENARIOS -------------------------------------
# To allow for independent comparison of different influences e.g.
# healthcare system impacts / economic interventions
# For weekly IAA analysis, just run this section without any changes

# Check variable names to decide how to subset the scenarios
# scenario %>% select(type_partA) %>% unique
# scenario %>% select(type_partB) %>% unique
# scenario %>% select(type_partC) %>% unique

# All scenarios
all = scenario %>% select(-type_partA, -type_partB, -type_partC)

# Hazard
# hazard = 
#  scenario %>% 
#  filter(type_partA == "Hazard") %>% 
#  select(-type_partA, -type_partB, -type_partC)

# Healthcare
# healthcare = 
#  scenario %>% 
#  filter(type_partA == "Healthcare") %>% 
#  select(-type_partA, -type_partB, -type_partC)

# Hazard + Healthcare
# hazard_healthcare = 
#  scenario %>% 
#  filter(type_partA == "Hazard" | type_partA == "Healthcare") %>%
#  select(-type_partA, -type_partB, -type_partC)

# Choose which of these you want to use
edgesNew = all ## Specify


# 6. INTRODUCE EDGESNEW TO EDGELIST FOR A NEW SCENARIO --------------------
# Introduce edgesNew to edgelist_template to create edgelist_scenario
edgelist_scenario = 
  USAH_location_baseline$edgelist %>% # Take pre-pandemic edgelist
  weightEdges(edgesNew) %>% # Swap in edgesNew weights
  weight_hangingVertices() # For vertices with downward weights all = 0.001, make upward weights = 0.001

# Check on some edges which we know should be changed for the Edinburgh lockdown edgelist to confirm it's all working
# edgelist_scenario %>% filter(from == "Manufacture healthcare equipment and supplies")
# edgelist_scenario %>% filter(to == "Manufacture healthcare equipment and supplies")
# edgelist_scenario %>% filter(from == "Provide general healthcare services", to == "Airports (major)")
# edgelist_scenario %>% filter(to == "Act as community meeting space")
# edgelist_scenario %>% filter(from == "Dispense medicines")
# edgelist_scenario %>% filter(to == "Children's clubs")

# 7. GENERATE USAH_SCENARIO OUTPUT ----------------------------------------
# Generate output for USAH_scenario
USAH_scenario = 
  apply_scenario(
    USAH_template_baseline = USAH_template_baseline,
    edgelist_scenario = edgelist_scenario)

# Inspect list elements of output
USAH_scenario$vIncluded
USAH_scenario$vExcluded
USAH_scenario$adjMat
USAH_scenario$edgelist
USAH_scenario$igraph
USAH_scenario$results
USAH_scenario$summary


# 8. SAVE OUTPUTS ---------------------------------------------------------
# Write list to RDS file
USAH_scenario %>% saveRDS(filenameTimestamp(
  prefix = "testScenarios/USAH_1.1_Edinburgh_COVID-wk40", extension = ".RDS")) ## Specify filename

# If you wanted to pull out just the results and write this to a CSV
USAH_scenario$results %>% write.csv(filenameTimestamp(
  prefix = "testScenarios/USAH_1.1_Edinburgh_COVID-wk40", extension = ".csv")) ## Specify filename


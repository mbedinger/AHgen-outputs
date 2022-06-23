# =========================================================================
# NUS_3_introduceLocation_COVID.R

# Note: Part 3

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
# Function to check all vNames in edgeGuide are compatible with USAH vNames
check_edgeGuide <- function(edgeGuide, vInfo) {
  
  fromCheck <- edgeGuide %>% select(from) %>% rename(vName = from)
  toCheck <- edgeGuide %>% select(to) %>% rename(vName = to)
  check <- rbind(fromCheck, toCheck) %>% unique
  
  incorrectlyNamed <- 
    vInfo %>% 
    filter(level == 4 | level == 5) %>% 
    full_join(check, by = "vName") %>% 
    filter(is.na(level))
  
  return(incorrectlyNamed)
  
}

# Function to check for duplicate edges
check_duplicateEdges <- function(edgelist) {
  
  edges <- edgelist %>% select(from, to)
  
  ind <- duplicated(edges[,1:2])
  
  return(edges[ind,])
  
}

# Function to find edges in edgeGuides which are not in the baseline scenario
find_missingLinks <- function(edgeGuide, USAH_input) {
  
  edgesNew <- edgeGuide %>% select(layer, from, to)
  edgelist <- USAH_input$edgelist %>% select(layer, from, to)
  missingLinks <- dplyr::setdiff(edgesNew, edgelist)
  
  return(missingLinks)
  
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

# Function to generate new location baseline with missing edges relevant to COVID phases added
add_missingLinks <- 
  function(USAH_template_baseline, USAH_location_baseline, edgesAdd) {
    
    USAH_location_baseline$edgelist <- 
      USAH_location_baseline$edgelist %>% rbind(edgesAdd)
    
    USAH_location_baseline$adjMat <- 
      USAH_location_baseline$edgelist %>% 
      edgelist_to_adjMat(vInfo = USAH_template_baseline$vIncluded)
    
    USAH_location_baseline$igraph <- 
      USAH_location_baseline$edgelist %>% 
      edgelist_to_igraph(vInfo = USAH_template_baseline$vIncluded)
    
    USAH_location_baseline$results <- 
      getResults(igraph = USAH_location_baseline$igraph, 
                 vInfo = USAH_template_baseline$vIncluded)
    
    USAH_location_baseline$summary <- 
      summarise_USAH(vInfo = USAH_template_baseline$vIncluded, 
                     edgelist = USAH_location_baseline$edgelist)
    
    return(USAH_location_baseline)
    
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
  "outputs/USAH_1.0_Edinburgh_OSMonly_baseline_20210322-103456.RDS") ## Specify location baseline USAH

# Read in edge guide for pre-lockdown (ISO weeks 11-12)
edgeGuide_prelockdown_all = read_xlsx("inputs/edgeGuide_pre-lockdown_all_20210106.xlsx") ## Specify
# Read in edge guide for lockdown in Edinburgh (ISO weeks 13-22)
edgeGuide_lockdown_Edinburgh = read_xlsx("inputs/edgeGuide_lockdown_Edinburgh_20210322.xlsx") ## Specify
# Read in edge guide for lockdown in Glasgow (ISO weeks 13-22)
edgeGuide_lockdown_Glasgow = read_xlsx("inputs/edgeGuide_lockdown_Glasgow_20210106.xlsx") ## Specify
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
edgeGuide_phase3f_Glasgow = read_xlsx("inputs/edgeGuide_Phase3_partF_Glasgow_20210106.xlsx") ## Specify


# 4. GENERATE NEW LOCATION BASELINE BASED ON EDGEGUIDES -------------------
# First check the edgeGuide vNames perfectly match the USAH vNames
# Then check the edgeGuide for duplicate edges
# Both should return a tibble with 0 rows, if there are >0 rows adjustments need to be made to edgeGuide spreadsheet before use
# Then inspect missingLinks, which are present in the edgeGuide but not in the location baseline, that need to be added

# Check pre-lockdown all
edgeGuide_prelockdown_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_prelockdown_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_prelockdown_all = 
  edgeGuide_prelockdown_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check lockdown Edinburgh
edgeGuide_lockdown_Edinburgh %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_lockdown_Edinburgh %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_lockdown_Edinburgh =
  edgeGuide_lockdown_Edinburgh %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check lockdown Glasgow
# edgeGuide_lockdown_Glasgow %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
# edgeGuide_lockdown_Glasgow %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
# missingLinks_lockdown_Glasgow =
#   edgeGuide_lockdown_Glasgow %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 1 all
edgeGuide_phase1_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase1_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase1_all =
  edgeGuide_phase1_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 2a all
edgeGuide_phase2a_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase2a_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase2a_all =
  edgeGuide_phase2a_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 2b all
edgeGuide_phase2b_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase2b_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase2b_all =
  edgeGuide_phase2b_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3a all
edgeGuide_phase3a_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3a_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3a_all =
  edgeGuide_phase3a_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3b all
edgeGuide_phase3b_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3b_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3b_all =
  edgeGuide_phase3b_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3c all
edgeGuide_phase3c_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3c_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3c_all =
  edgeGuide_phase3c_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3d all
edgeGuide_phase3d_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3d_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3d_all =
  edgeGuide_phase3d_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3e all
edgeGuide_phase3e_all %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3e_all %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3e_all =
  edgeGuide_phase3e_all %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3f Edinburgh
edgeGuide_phase3f_Edinburgh %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3f_Edinburgh %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3f_Edinburgh =
  edgeGuide_phase3f_Edinburgh %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3g Edinburgh
edgeGuide_phase3g_Edinburgh %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
edgeGuide_phase3g_Edinburgh %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
missingLinks_phase3g_Edinburgh =
  edgeGuide_phase3g_Edinburgh %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Check Phase 3f Glasgow
# edgeGuide_phase3f_Glasgow %>% check_edgeGuide(vInfo = USAH_template_baseline$vIncluded)
# edgeGuide_phase3f_Glasgow %>% select(layer, from, to, COVID) %>% check_duplicateEdges()
# missingLinks_phase3f_Glasgow =
#   edgeGuide_phase3f_Glasgow %>% find_missingLinks(USAH_input = USAH_location_baseline)

# Compile all edges that need added to Edinburgh baseline, with weight = 0.001 to indicate these are not present in baseline
edgesAdd =
  missingLinks_prelockdown_all %>% 
  rbind(
    missingLinks_lockdown_Edinburgh, 
    missingLinks_phase1_all, 
    missingLinks_phase2a_all, 
    missingLinks_phase2b_all, 
    missingLinks_phase3a_all, 
    missingLinks_phase3b_all, 
    missingLinks_phase3c_all,
    missingLinks_phase3d_all, 
    missingLinks_phase3e_all, 
    missingLinks_phase3f_Edinburgh, 
    missingLinks_phase3g_Edinburgh) %>% 
  unique %>%
  add_column(weight = 0.0000000001)

# Generate new location baseline with missing edges relevant to COVID phases added
USAH_location_COVID = 
  add_missingLinks(
    USAH_template_baseline = USAH_template_baseline,
    USAH_location_baseline = USAH_location_baseline,
    edgesAdd = edgesAdd)

USAH_location_COVID$keyCheck
USAH_location_COVID$vIncluded
USAH_location_COVID$vExcluded
USAH_location_COVID$adjMat
USAH_location_COVID$edgelist
USAH_location_COVID$igraph
USAH_location_COVID$results
USAH_location_COVID$summary


# 5. SAVE OUTPUTS ---------------------------------------------------------
# Save new location baseline with missing edges relevant to COVID phases added
USAH_location_COVID %>% saveRDS(filenameTimestamp(
  prefix = "outputs/USAH_1.1_Edinburgh_baseline-OSM-COVID", extension = ".RDS")) ## Specify filename


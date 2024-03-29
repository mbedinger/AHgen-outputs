# =========================================================================
# EF_3_introduceFlood.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2021-11-30
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy is installed
devtools::install_github("avisserquinn/OSMtidy@dev")

# Load required packages
pacman::p_load(
  tidyverse, OSMtidy, openxlsx, readxl, data.table, 
  stats, igraph, tnet, janitor, ggraph)


# 0.2. FUNCTIONS ----------------------------------------------------------

# Load local AHgen functions; eventually will be part of AHgen
source("functions/functions.R")

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}

# Helper function to export Excel workbook
exportExcel <- function(inputList, filename) {
  
  require(openxlsx)
  
  wb <- openxlsx::createWorkbook()
  
  input <- inputList
  
  # sheetnames <- paste0("Sheet", seq_along(input))
  sheetnames <- names(input) %>% str_sub(1,20)
  lsn = length(sheetnames)
  snid = .create_unique_ids(lsn, char_len = 3)
  sheetnames <- paste0(1:lsn, "_", snid, "_", sheetnames)
  
  Map(function(data, nameofsheet){
    
    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeDataTable(wb, nameofsheet, data, rowNames = FALSE)
    
  }, input, sheetnames)
  
  openxlsx::saveWorkbook(wb, file = filename)
  
}

# OSMtidy internal function .create_unique_ids for exportExcel
.create_unique_ids <- function(n, seed_no = 1, char_len = 3){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}


# 1. PREPARE REAL FLOOD SCENARIO ------------------------------------------

###  A. READ IN & FORMAT DATA

# Read in the USAH_input you want to start with e.g. USAH_template_baseline
USAH_template_baseline = readRDS(
  "USAH_3.0_template_baseline_20210910.RDS") ## Specify master USAH with all vertex and edge information

# Read in the USAH_input you want to start with e.g. USAH_template_baseline
USAH_location_baseline = readRDS(
  "USAH_3.0_Glasgow_baseline_20210910.RDS") ## Specify baseline USAH you want to compare to

# Extract input edgelist
edgelist_location_baseline = USAH_location_baseline$edgelist

# Read in the OSMtidy count for the baseline
count_location_baseline = readRDS(
  "OSMtidy_count_3.0_Glasgow_baseline_20210906.RDS") ## Specify 

# Read in the OSMtidy count for the flood scenario
count_location_flood = readRDS(
  "OSMtidy_count_3.0_Glasgow_floodRiver200_20210924-145149.RDS") ## Specify


###  B. INTRODUCE EDGESNEW

# Compare OSMtidy outputs for a baseline location and flood scenario
# to find the proportion of physical objects that remain functional
countCompared <- 
  compareOSMtidy(
    baselineName = "3.0_Glasgow_baseline", ## Specify baseline name
    countOSMtidy_baseline = count_location_baseline, ## Specify baseline OSMtidy count
    scenarioName = "3.0_Glasgow_flood-river-1-200", ## Specify flood scenario name
    countOSMtidy_scenario = count_location_flood) ## Specify flood scenario OSMtidy count

# Create edgesFlood
edgesFlood = 
  gen_edgesNew(
    vInfo_template = USAH_template_baseline$vInfo,
    hazard = "flood",
    edgelist_input = edgelist_location_baseline, ## Specify input edgelist
    countCompared = countCompared, ## Specify countCompared
    proxyWeight = 0) ## Specify proxyWeight = 0

# Introduce edgesNew to create edgelist_location_flood
edgelist_location_flood = 
  USAH_location_baseline$edgelist %>%
  weightEdges(edgesNew = edgesFlood) %>% # Swap in edgesNew weights for flood scenario
  weight_hangingVertices(proxyWeight = 0) ## Specify proxyWeight = 0


# 3. GENERATE OUTPUT FOR USAH_SCENARIO ------------------------------------

# Generate output for USAH_scenario
USAH_scenario = 
  apply_scenario(
    USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
    USAH_input = USAH_location_baseline, ## Specify USAH_input you want to modify
    edgelist_scenario = edgelist_location_flood, ## Specify edgelist for new scenario
    proxyWeight = 0) ## Specify proxyWeight = 0

# Inspect elements
# Note that in current code when proxyWeight > 0, 
USAH_scenario$vIncluded # always as compared to template
USAH_scenario$vExcluded # always as compared to template
USAH_scenario$adjMat
USAH_scenario$edgelist
USAH_scenario$igraph
USAH_scenario$results
USAH_scenario$summary


# 4. SAVE OUTPUTS ---------------------------------------------------------


### USAH outputs should remain in AHgen-appl under the appropriate city folder ###
# This is to keep a record of the USAH outputs with network analysis results etc.

# Write USAH_scenario to RDS file
USAH_scenario %>% saveRDS(filenameTimestamp(
  prefix = "USAH_3.0_Glasgow_floodRiver200", ## Specify filepath and filename
  extension = ".RDS"))

# If you wanted to pull out USAH_scenario$results only and write this to a CSV
USAH_scenario$results %>% write.csv(filenameTimestamp(
  prefix = "USAH_3.0_Glasgow_floodRiver200_results", ## Specify filepath and filename
  extension = ".csv"))


### countCompared can remain in OSMtidy-appl under compared folder ###
# This is to keep a record of how different OSMtidy outputs compared to each other

# Write countCompared to RDS for future use
countCompared %>% saveRDS(filenameTimestamp(
  prefix = "OSMtidy_countCompared_3.0_Glasgow_floodRiver200", ## Specify filepath and filename
  extension = ".RDS"))

# Write countCompared to xlsx with multiple worksheets
exportExcel(
  inputList = countCompared, # Specify output object 
  filenameTimestamp(
    prefix = "OSMtidy_countCompared_3.0_Glasgow_floodRiver200", ## Specify filepath and filename
    extension = ".xlsx"))


### edgesFlood or other edges to change can be recorded in AHgen-appl under the flood folder ###
# This is to keep a record of what types of edges were modified for this flood scenario, comparable to an edgeGuide

# Write edgesFlood to RDS for future use
edgesFlood %>% saveRDS(filenameTimestamp(
  prefix = "USAH_edges_3.0_Glasgow_floodRiver200", ## Specify filepath and filename
  extension = ".RDS"))

# Write edgesFlood to xlsx for future use
edgesFlood %>% write.xlsx(filenameTimestamp(
  prefix = "USAH_edges_3.0_Glasgow_floodRiver200", ## Specify filepath and filename
  extension = ".xlsx"))
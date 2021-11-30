# =========================================================================
# HFEMSI_1_generateTemplate.R

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

# Load required packages
pacman::p_load(
  tidyverse, openxlsx, readxl, data.table, stats, igraph, tnet, janitor, installr)


# 0.2 EXTRA FUNCTIONS -----------------------------------------------------

# Load local AHgen functions; eventually will be part of AHgen
source("functions/functions.R")

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 1. READ IN DATA ---------------------------------------------------------

# Read in template USAH adjacency matrix
adjMat_template = read_adjMat(
  "USAH_3.0_template_baseline_adjMat_20210826.xlsx") ## Specify input filename

# Create template USAH vInfo (basic)
vInfo_template = adjMat_template %>% select(level, levelName, vName)

# Read in template USAH vInfo (full)
vInfo_template_full = read_xlsx(
  "USAH_3.0_template_baseline_vInfo_full_20210831.xlsx") ## Specify input filename

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "OSM-AH-key_3.0_20210906.csv") ## Specify input filename


# 2. CHECK INPUTS ---------------------------------------------------------

# Check vNames from different data inputs are compatible
checkNames(adjMat_template, vInfo_template_full, key)
# If some vNames do not match
# checkNames = checkNames(adjMat_template, vInfo_template_full, key)
# checkNames %>% write.csv("checkNames.csv")

# Confirm adjMat has completely symmetrical edges i.e. there are no input errors
adjMat_template %>% checkSymmetry
# If any edges are unsymmetrical
# checkSymmetry = adjMat_template %>% checkSymmetry

# Confirm adjMat has no vertices with redundant sets of edges i.e. no vertices
# are connected to the same set of other vertices between two levels
adjMat_template %>% checkRedundancy


# 3. GENERATE USAH_TEMPLATE_BASELINE OUTPUT -------------------------------

# Generate edgelist
edgelist_template = adjMat_template %>% adjMat_to_edgelist(vInfo = vInfo_template)

# Generate igraph
igraph_template = adjMat_template %>% adjMat_to_igraph(vInfo = vInfo_template)

# !! OPTIONAL !!
# If changes have been made to how objects have been assigned to subnetworks
# Then update assignment of subnetworks to connected vertices and 
# generate a new vInfo_template_full
vInfo_template_full = 
 update_subnetworks(
   edgelist_template = edgelist_template, 
   vInfo_template_full = vInfo_template_full) %>%
 dplyr::relocate(level, levelName, vName, definition, reference)

# Generate output for USAH_template_baseline
USAH_template_baseline = 
  list("vInfo" = vInfo_template_full, # Attach additional info for vertices
       "vIncluded" = vInfo_template, # Attach dataframe of included vertices (all for template USAH)
       "vExcluded" = list(), # Attach dataframe of excluded vertices (for template USAH this is empty)
       "adjMat" = adjMat_template, # Attach adjacency matrix for template USAH
       "edgelist" = edgelist_template, # Attach edgelist for template USAH
       "igraph" = igraph_template, # Attach igraph for template USAH
       "results" = getResults(igraph = igraph_template, vInfo = vInfo_template), # Create results for template USAH
       "summary" = summarise_ah(vIncluded = vInfo_template, # Create summary of vertices by level
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


# 4. SAVE OUTPUTS ---------------------------------------------------------

# Write output to RDS file
USAH_template_baseline %>% saveRDS(filenameTimestamp(
  prefix = "USAH_3.0_template_baseline", ## Specify filepath and filename
  extension = ".RDS"))

# If subnetwork assignment has been updated write out to xlsx for future use
USAH_template_baseline$vInfo %>% write.xlsx(filenameTimestamp(
  prefix = "USAH_3.0_template_baseline_vInfo_full", ## Specify filepath and filename
  extension = ".xlsx"))

# Write results to CSV file
USAH_template_baseline$results %>% write.csv(filenameTimestamp(
  prefix = "USAH_3.0_template_baseline_results", ## Specify filepath and filename
  extension = ".csv"))
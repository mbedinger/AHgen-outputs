# =========================================================================
# 3b - Introduce other hazards.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2021-07-19
#
# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-26
# =========================================================================

# This script demonstrates how to apply other hazards using an edge guide,
# i.e. an spreadsheet which identifies which edges would likely be affected
# by a specific hazard type.


# PREPARATION -------------------------------------------------------------

# 1. PREPARE THE ENVIRONMENT ----------------------------------------------

# Clear the RStudio environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory to the folder where the script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# and install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# install.packages("devtools")
# install.packages("pacman")

# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, installr, janitor, AHgen)


# 2. READ IN TEMPLATE DATA ------------------------------------------------

# Read in the USAH input you want to start with
USAH_template_baseline = 
  read_rds("data/template/3.0/USAH_3.0_template_baseline_20230719-104712.RDS") ## Specify file

# Extract input edgelist
edgelist_template_baseline = USAH_template_baseline$edgelist

# Set names for AH, version & location
AH_name = "USAH"
AH_version = "3.0"
AH_location = "template"



# DROUGHT SCENARIOS -------------------------------------------------------

# 3. DROUGHT - READ IN DATA -----------------------------------------------

# Set names for scenarios
scenario_drought1 = "drought-1"
scenario_drought2 = "drought-2"

# DROUGHT - OPTION 1
# Read in new weights for hypothetical drought hazard
# This edge guide includes all edges where functionality may be reduced due
# to a reduction in water availability
edgesDrought_hyp1 = 
  readxl::read_xlsx(
    "data/hazards/drought/edgeGuide_2.0_template_drought-hypothetical_allEdges_20210720.xlsx") %>% ## Specify file
  select(layer, from, to, weightNew) 

# Inspect
edgesDrought_hyp1

# DROUGHT - OPTION 2
# Alternatively, this simplified edge guide represents a hypothetical drought 
# hazard using only the Object-Related Processes where ~all~ edges downward 
# would be affected

# Read in data
vDrought_hyp2 = 
  readxl::read_xlsx(
    "data/hazards/drought/vGuide_2.0_template_drought-hypothetical_wholeVerticesOnly_20210720.xlsx") ## Specify file

# Inspect
vDrought_hyp2

# Create vector of affected Object-Related Processes
vDrought = vDrought_hyp2 %>% pull(drought_hypothetical_ORP)

# Create edgesDrought_hyp2
edgesDrought_hyp2 = 
  edgelist_template_baseline %>% 
  filter(from %in% vDrought) %>% # Filter vDrought relevant edges from edgelist_template_baseline
  mutate(weight = 0.5) %>% # Reweight affected edges
  rename(weightNew = weight) # Remove old weight column

# Inspect
edgesDrought_hyp2


# 4. DROUGHT - CHECK FOR DUPLICATE EDGES ----------------------------------

# Check for duplicate edges (that may have been accidentally added to a 
# manual edgelist for a hypothetical flood scenario) - confirms whether there 
# are any potential conflicts to sort out before introducing the new edge 
# weights for that scenario
edgesDrought_hyp1 %>% check_duplicates

# If any, inspect the duplicate edges with (potentially) different weights, e.g.
# edgesDrought_hyp %>% filter(from == "Undertake legal proceedings", 
# to == "Legal services")
# And correct input file then re-import before proceeding

# Repeat with drought edge guide (option 2)
edgesDrought_hyp2 %>% check_duplicates


# 5. DROUGHT - CHECK FOR INCONSISTENT EDGES -------------------------------

# Checking for outdated or inconsistent edges in the edge guide 
# that are inconsistent with the USAH_input edgelist you want to overwrite 
# both in terms of outdated linkages and outdated vertex names,
# this prevents adding anything odd to the structure of the network
edgesDrought_hyp1 %>% check_diff(edgelist = edgelist_template_baseline)

# If any, inspect the edgesNew that do not match and correct input file 
# then re-import before proceeding

# Repeat with drought edge guide (option 2)
edgesDrought_hyp2 %>% check_diff(edgelist = edgelist_template_baseline)


# 6. DROUGHT - INTRODUCE EDGESNEW TO EDGELIST -----------------------------

# Introduce edgesDrought_hyp1 to create edgelist_template_droughtA
edgelist_template_drought1 = 
  edgelist_template_baseline %>%
  weight_edges(edgesDrought_hyp1) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()

# Introduce edgesDrought_hyp2 to create edgelist_template_droughtA
edgelist_template_drought2 = 
  edgelist_template_baseline %>%
  weight_edges(edgesDrought_hyp2) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()


# 7. DROUGHT - GENERATE OUTPUTS -------------------------------------------

# Generate output for USAH_template_drought_hyp1
USAH_template_drought_hyp1 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify  
    edgelist_scenario = edgelist_template_drought1, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_drought1)

# Generate output for USAH_template_drought_hyp2
USAH_template_drought_hyp2 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify  
    edgelist_scenario = edgelist_template_drought2, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_drought2)

# Inspect removed nodes
USAH_template_drought_hyp1$vExcluded
USAH_template_drought_hyp2$vExcluded

# Inspect results
USAH_template_drought_hyp1$results
USAH_template_drought_hyp2$results



# FLOOD SCENARIOS ---------------------------------------------------------

# 3. FLOOD - READ IN DATA -------------------------------------------------

# As for the drought edge guides, there is both a fuller (hyp1) and
# simplified (hyp2) version of the flood edge guide

# Set names for scenarios
scenario_flood1 = "flood-1"
scenario_flood2 = "flood-2"

# FLOOD - OPTION 1
# Read in new weights for hypothetical flood hazard
edgesFlood_hyp1 = 
  readxl::read_xlsx(
    "data/hazards/flood/edgeGuide_2.0_template_flood-hypothetical_20210720.xlsx") %>% ## Specify file
  select(layer, from, to, weightNew)

# FLOOD - OPTION 2
edgesFlood_hyp2 = 
  readxl::read_xlsx(
    "data/hazards/flood/edgeGuide_2.0_template_flood-hypothetical-simple_20210803.xlsx") %>% ## Specify file
  select(layer, from, to, weightNew)


# 4. FLOOD - CHECK FOR DUPLICATE EDGES ------------------------------------

# Check for duplicate edges in flood edge guide(s)
edgesFlood_hyp1 %>% check_duplicates
edgesFlood_hyp2 %>% check_duplicates


# 5. FLOOD - CHECK FOR INCONSISTENT EDGES ---------------------------------

# Check for inconsistent edges in flood edge guide(s)
edgesFlood_hyp1 %>% check_diff(edgelist = edgelist_template_baseline)
edgesFlood_hyp2 %>% check_diff(edgelist = edgelist_template_baseline)


# 6. FLOOD - INTRODUCE EDGESNEW TO EDGELIST -------------------------------

# Introduce edgesFlood_hyp1 to create edgelist_template_flood
edgelist_template_flood1 = 
  edgelist_template_baseline %>%
  weight_edges(edgesFlood_hyp1) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()

# Introduce edgesFlood_hyp2 to create edgelist_template_flood2
edgelist_template_flood2 = 
  edgelist_template_baseline %>%
  weight_edges(edgesFlood_hyp2) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()


# 7. FLOOD - GENERATE OUTPUTS ---------------------------------------------

# Generate output for USAH_template_flood_hyp1
USAH_template_flood_hyp1 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify  
    edgelist_scenario = edgelist_template_flood1, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_flood1)

# Generate output for USAH_template_flood_hyp2
USAH_template_flood_hyp2 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify
    edgelist_scenario = edgelist_template_flood2, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_flood2)

# Inspect removed nodes
USAH_template_flood_hyp1$vExcluded
USAH_template_flood_hyp2$vExcluded

# Inspect results
USAH_template_flood_hyp1$results
USAH_template_flood_hyp2$results



# PANDEMIC SCENARIOS ------------------------------------------------------

# 3. PANDEMIC - READ IN DATA ----------------------------------------------

# As for the drought and flood edge guides, there is both a fuller (hyp1) and
# simplified (hyp2) version of the pandemic edge guide

# Set names for scenarios
scenario_pandemic1 = "pandemic-1"
scenario_pandemic2 = "pandemic-2"

# PANDEMIC - OPTION 1
# Read in new weights for hypothetical pandemic hazard
edgesPandemic_hyp1 = 
  readxl::read_xlsx(
    "data/hazards/covid/edgeGuide_2.0_template_pandemic-hypothetical_20210805.xlsx") %>% ## Specify filepath and filename
  select(layer, from, to, weightNew)

# PANDEMIC - OPTION 2
edgesPandemic_hyp2 = 
  readxl::read_xlsx(
    "data/hazards/covid/edgeGuide_2.0_template_pandemic-hypothetical-simple_20210805.xlsx") %>% ## Specify file
  select(layer, from, to, weightNew)


# 4. PANDEMIC - CHECK FOR DUPLICATE EDGES ---------------------------------

# Check duplicates for pandemic edge guide(s)
edgesPandemic_hyp1 %>% check_duplicates
edgesPandemic_hyp2 %>% check_duplicates


# 5. PANDEMIC - CHECK FOR INCONSISTENT EDGES ------------------------------

# Check for inconsistent edges in pandemic edge guide(s)
edgesPandemic_hyp1 %>% check_diff(edgelist = edgelist_template_baseline)
edgesPandemic_hyp2 %>% check_diff(edgelist = edgelist_template_baseline)


# 6. PANDEMIC - INTRODUCES EDGESNEW TO EDGELIST ---------------------------

# Introduce edgesPandemic_hyp1 to create edgelist_template_pandemic
edgelist_template_pandemic_removal1 = 
  edgelist_template_baseline %>%
  weight_edges(edgesPandemic_hyp1) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()

# Introduce edgesPandemic_hyp2 to create edgelist_template_pandemic2
edgelist_template_pandemic_removal2 = 
  edgelist_template_baseline %>%
  weight_edges(edgesPandemic_hyp2) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()


# 7. PANDEMIC - GENERATE OUTPUTS ------------------------------------------

# Generate output for USAH_template_pandemic_hyp1
USAH_template_pandemic_hyp_removal1 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify  
    edgelist_scenario = edgelist_template_pandemic_removal1, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_pandemic1)

# Generate output for USAH_template_pandemic_hyp2
USAH_template_pandemic_hyp_removal2 = 
  apply_scenario(
    AH_input = USAH_template_baseline, ## Specify USAH_input you want to modify  
    edgelist_scenario = edgelist_template_pandemic_removal2, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_pandemic2)

# Inspect removed nodes
USAH_template_pandemic_hyp_removal1$vExcluded
USAH_template_pandemic_hyp_removal2$vExcluded

# Inspect results
USAH_template_pandemic_hyp_removal1$results
USAH_template_pandemic_hyp_removal2$results



# SAVE OUTPUTS ------------------------------------------------------------

USAH_template_drought_hyp1 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_drought1)

USAH_template_drought_hyp2 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_drought2)

USAH_template_flood_hyp1 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_flood1)

USAH_template_flood_hyp2 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_flood2)

USAH_template_pandemic_hyp_removal1 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = scenario_pandemic1)

USAH_template_pandemic_hyp_removal2 %>% 
  export_AHgen(
    type = "USAH",
    directory = "data/template/3.0/", ## Specify directory
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario_pandemic2)
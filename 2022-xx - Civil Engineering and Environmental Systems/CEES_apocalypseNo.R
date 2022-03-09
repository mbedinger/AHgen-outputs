# =========================================================================
# apocalypseNo.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2021-07-19
#
# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2021-11-15
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(
  tidyverse, openxlsx, readxl, data.table, stats, igraph, tnet, janitor, ggraph)


# 0.2. FUNCTIONS ----------------------------------------------------------

# Load local AHgen functions; eventually will be part of AHgen
source("functions/functions.R")

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 1. READ IN DATA ---------------------------------------------------------

# TEMPLATE BASELINE 
# Read in the USAH_input you want to start with
USAH_template_baseline = readRDS(
  "USAH_2.0_template_baseline_20210802.RDS") ## Specify filepath and filename
# Extract input edgelist
edgelist_template_baseline = USAH_template_baseline$edgelist


# DROUGHT OPTION A - Read in new weights for hypothetical drought hazard
# edgesDrought_hypA = 
#   read_xlsx(
#     "edgeGuide_2.0_template_drought-hypothetical_allEdges_20210720.xlsx") %>% ## Specify filepath and filename
#   select(layer, from, to, weightNew) # Select only relevant columns


# DROUGHT OPTION B
# Alternatively, use a simplified hypothetical drought hazard using only the 
# Object-Related Processes where ~all~ edges downward would be affected
# Read in data
vDrought_hypB = 
  read_xlsx(
    "vGuide_2.0_template_drought-hypothetical_wholeVerticesOnly_20210720.xlsx") ## Specify filepath and filename
# Create vector of affected Object-Related Processes
vDrought = vDrought_hypB %>% pull(drought_hypothetical_ORP)
# Create edgesDrought_hypB
edgesDrought_hypB = 
  edgelist_template_baseline %>% 
  filter(from %in% vDrought) %>% # Filter vDrought relevant edges from edgelist_template_baseline
  mutate(weight = 0.5) %>% # Reweight affected edges
  rename(weightNew = weight) # Remove old weight column


# FLOOD - Read in new weights for hypothetical flood hazard
# edgesFlood_hyp = 
#   read_xlsx(
#     "edgeGuide_2.0_template_flood-hypothetical_20210720.xlsx") %>% ## Specify filepath and filename
#   select(layer, from, to, weightNew) # Select only relevant columns

# FLOOD - OPTION 2
edgesFlood_hyp2 = 
  read_xlsx(
    "edgeGuide_2.0_template_flood-hypothetical-simple_20210803.xlsx") %>% ## Specify filepath and filename
  select(layer, from, to, weightNew) # Select only relevant columns


# PANDEMIC - Read in new weights for hypothetical pandemic hazard
# edgesPandemic_hyp = 
#   read_xlsx(
#     "edgeGuide_2.0_template_pandemic-hypothetical_20210805.xlsx") %>% ## Specify filepath and filename
#   select(layer, from, to, weightNew) # Select only relevant columns

# PANDEMIC - OPTION 2
edgesPandemic_hyp2 = 
  read_xlsx(
    "edgeGuide_2.0_template_pandemic-hypothetical-simple_20210805.xlsx") %>% ## Specify filepath and filename
  select(layer, from, to, weightNew) # Select only relevant columns


# 2. CHECK FOR DUPLICATES IN EDGE GUIDES ----------------------------------

# Checking for duplicate edges (that may have been accidentally added to a 
# manual edgelist for a hypothetical flood scenario) confirms whether there are any potential 
# conflicts to sort out before introducing the new edge weights for that scenario
# edgesDrought_hypA %>% checkDuplicates

# If any, inspect the duplicate edges with (potentially) different weights, e.g.
# edgesDrought_hyp %>% filter(from == "Undertake legal proceedings", to == "Legal services")
# And correct input file then re-import before proceeding

# Repeat with drought edge guide (option B)
edgesDrought_hypB %>% checkDuplicates

# And flood edge guide(s)
# edgesFlood_hyp %>% checkDuplicates
edgesFlood_hyp2 %>% checkDuplicates

# And pandemic edge guide(s)
# edgesPandemic_hyp %>% checkDuplicates
edgesPandemic_hyp2 %>% checkDuplicates


# 3. CHECK FOR INCONSISTENT EDGES IN EDGE GUIDES --------------------------

# Checking for outdated or inconsistent edges in the edge guide 
# that are inconsistent with the USAH_input edgelist you want to overwrite 
# both in terms of outdated linkages and outdated vertex names,
# this prevents adding anything odd to the structure of the network
# edgesDrought_hypA %>% checkDiff(edgelist = edgelist_template_baseline)

# If any, inspect the edgesNew that do not match and correct input file 
# then re-import before proceeding

# Repeat with drought edge guide (option B)
edgesDrought_hypB %>% checkDiff(edgelist = edgelist_template_baseline)

# And flood edge guide(s)
# edgesFlood_hyp %>% checkDiff(edgelist = edgelist_template_baseline)
edgesFlood_hyp2 %>% checkDiff(edgelist = edgelist_template_baseline)

# And pandemic edge guide(s)
# edgesPandemic_hyp %>% checkDiff(edgelist = edgelist_template_baseline)
edgesPandemic_hyp2 %>% checkDiff(edgelist = edgelist_template_baseline)


# 4. INTRODUCE EDGESNEW TO EDGELIST ---------------------------------------

# Introduce edgesDrought_hypA to create edgelist_template_droughtA
# edgelist_template_droughtA = 
#   edgelist_template_baseline %>%
#   weightEdges(edgesDrought_hypA) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
#   weight_hangingVertices()

# Introduce edgesDrought_hypB to create edgelist_template_droughtA
edgelist_template_droughtB = 
  edgelist_template_baseline %>%
  weightEdges(edgesDrought_hypB) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()

# Introduce edgesFlood_hyp to create edgelist_template_flood
# edgelist_template_flood = 
#   edgelist_template_baseline %>%
#   weightEdges(edgesFlood_hyp) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
#   weight_hangingVertices()

# Introduce edgesFlood_hyp2 to create edgelist_template_flood2
edgelist_template_flood2 = 
  edgelist_template_baseline %>%
  weightEdges(edgesFlood_hyp2) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  weight_hangingVertices()

# Introduce edgesPandemic_hyp to create edgelist_template_pandemic
# edgelist_template_pandemic_removal = 
#   edgelist_template_baseline %>%
#   weightEdges(edgesPandemic_hyp) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
#   mutate(weight = replace(weight, weight == 0.0000000001, 0)) %>%
#   weight_hangingVertices(proxyWeight = 0)

# Introduce edgesPandemic_hyp2 to create edgelist_template_pandemic2
edgelist_template_pandemic_removal2 = 
  edgelist_template_baseline %>%
  weightEdges(edgesPandemic_hyp2) %>% ## Specify which edgesNew you are introducing (e.g. drought, flood)
  mutate(weight = replace(weight, weight == 0.0000000001, 0)) %>%
  weight_hangingVertices(proxyWeight = 0)


# 5. GENERATE OUTPUT FOR USAH_SCENARIO ------------------------------------

# Generate output for USAH_template_drought_hypA
# USAH_template_drought_hypA = 
#   apply_scenario(
#     USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
#     USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
#     edgelist_scenario = edgelist_template_droughtA) ## Specify edgelist for new scenario

# Generate output for USAH_template_drought_hypB
USAH_template_drought_hypB = 
  apply_scenario(
    USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
    USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
    edgelist_scenario = edgelist_template_droughtB) ## Specify edgelist for new scenario

# Generate output for USAH_template_flood_hyp
# USAH_template_flood_hyp = 
#   apply_scenario(
#     USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
#     USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
#     edgelist_scenario = edgelist_template_flood) ## Specify edgelist for new scenario

# Generate output for USAH_template_flood_hyp2
USAH_template_flood_hyp2 = 
  apply_scenario(
    USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
    USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
    edgelist_scenario = edgelist_template_flood2) ## Specify edgelist for new scenario

# Generate output for USAH_template_pandemic_hyp
# USAH_template_pandemic_hyp_removal = 
#   apply_scenario(
#     USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
#     USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
#     edgelist_scenario = edgelist_template_pandemic_removal, ## Specify edgelist for new scenario
#     proxyWeight = 0) ## Specify proxy weight for removal

# Generate output for USAH_template_pandemic_hyp2
USAH_template_pandemic_hyp_removal2 = 
  apply_scenario(
    USAH_template = USAH_template_baseline, ## Specify USAH_template_baseline (master version)
    USAH_input = USAH_template_baseline, ## Specify USAH_input you want to modify (in this case, also the master version)
    edgelist_scenario = edgelist_template_pandemic_removal2, ## Specify edgelist for new scenario
    proxyWeight = 0) ## Specify proxy weight for removal


# Inspect removed nodes
USAH_template_drought_hypB$vExcluded
# USAH_template_flood_hyp$vExcluded
USAH_template_flood_hyp2$vExcluded
# USAH_template_pandemic_hyp_removal$vExcluded
USAH_template_pandemic_hyp_removal2$vExcluded

# Inspect results
USAH_template_drought_hypB$results
# USAH_template_flood_hyp$results
USAH_template_flood_hyp2$results
# USAH_template_pandemic_hyp_removal$results
USAH_template_pandemic_hyp_removal2$results


# 6. COMPARE RESULTS ------------------------------------------------------

# PART 1

# Attach information to identify each scenario
step1a <- USAH_template_drought_hypB$results %>% mutate(Scenario = "Drought")
# step1b <- USAH_template_flood_hyp$results %>% mutate(Scenario = "Flood")
# step1c <- USAH_template_pandemic_hyp_removal$results %>% mutate(Scenario = "Pandemic")
step1d <- USAH_template_flood_hyp2$results %>% mutate(Scenario = "Flood 2")
step1e <- USAH_template_pandemic_hyp_removal2$results %>% mutate(Scenario = "Pandemic 2")

# Combine and reformat
step2 <- rbind(step1a, step1b, step1c, step1d, step1e) %>% select(-rank_overall)

# Join to baseline results you want to compare against and reformat
apocalypseNo <- 
  USAH_template_baseline$results %>%
  rename(base = centrality,
         baselineRankByLevel = rank_byLevel) %>%
  select(-rank_overall) %>%
  full_join(by = c("level", "levelName", "vName", "type"), step2) %>%
  rename(Node = vName,
         rankByLevel = rank_byLevel) %>%
  mutate(levelName = str_c(level, " - ", levelName),
         change = centrality - base,
         changePct = (centrality - base) / base, # changePct is before *100 for % value
         changeRank = rankByLevel - baselineRankByLevel) %>%
  mutate(levelName = factor(levelName, 
                            levels = c("1 - Functional purposes",
                                       "2 - Values and priority measures",
                                       "3 - Generalised functions",
                                       "4 - Object-related processes",
                                       "5 - Physical objects")),
         changePct = replace(changePct, changePct == Inf, NaN)) %>%
  select(level, levelName, Node, base, baselineRankByLevel, 
         Scenario, type, centrality, rankByLevel, change, changePct, changeRank)


# 7. SAVE OUTPUTS ---------------------------------------------------------

# Write output to RDS file
apocalypseNo %>% saveRDS(filenameTimestamp(
  prefix = "USAH_2.0_resultsCompared_apocalypseNo", ## Specify filepath and filename
  extension = ".RDS"))

# If subnetwork assignment has been updated write out to xlsx for future use
apocalypseNo %>% write.xlsx(filenameTimestamp(
  prefix = "USAH_2.0_resultsCompared_apocalypseNo", ## Specify filepath and filename
  extension = ".xlsx"))

# Write output to RDS file
USAH_template_drought_hypB %>% saveRDS(filenameTimestamp(
  prefix = "USAH_2.0_template_drought-hypothetical_wholeVerticesOnly", ## Specify filepath and filename
  extension = ".RDS"))

# Write output to RDS file
# USAH_template_flood_hyp %>% saveRDS(filenameTimestamp(
#   prefix = "USAH_2.0_template_flood-hypothetical", ## Specify filepath and filename
#   extension = ".RDS"))

# Write output to RDS file
USAH_template_flood_hyp2 %>% saveRDS(filenameTimestamp(
  prefix = "USAH_2.0_template_flood-hypothetical-simple", ## Specify filepath and filename
  extension = ".RDS"))

# Write output to RDS file
# USAH_template_pandemic_hyp_removal %>% saveRDS(filenameTimestamp(
#   prefix = "data/template/2.0/USAH_2.0_template_pandemic-hypothetical", ## Specify filepath and filename
#   extension = ".RDS"))

# Write output to RDS file
USAH_template_pandemic_hyp_removal2 %>% saveRDS(filenameTimestamp(
  prefix = "data/template/2.0/USAH_2.0_template_pandemic-hypothetical-simple", ## Specify filepath and filename
  extension = ".RDS"))
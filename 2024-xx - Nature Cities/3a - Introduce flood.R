# =========================================================================
# 3a - Introduce flood.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-26
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the RStudio environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory to the folder where this script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# and install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# install.packages("devtools")
# install.packages("pacman")

# Ensure latest version of OSMtidy & AHgen are installed
devtools::install_github("avisserquinn/OSMtidy@dev", dependencies = TRUE)
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, installr, janitor, OSMtidy, AHgen)


# 1. READ IN & FORMAT DATA ------------------------------------------------

# Set name, version, location, baseline, and scenario names
AH_name = "USAH"
AH_version = "3.0"
AH_location = "Manchester"
AH_baseline = "baseline"
AH_scenario = "floodRiver-100"

# Set directory for output
directory = "data/cities/Manchester/"

# Read in the USAH_input you want to start with e.g. USAH_template_baseline
USAH_template_baseline = 
  read_rds("data/template/3.0/USAH_3.0_template_baseline_20230719-104712.RDS") ## Specify master USAH with all vertex and edge information

# Read in the USAH_input you want to start with e.g. USAH_template_baseline
USAH_location_baseline = 
  read_rds("data/cities/Manchester/USAH_3.0_Manchester_baseline_20230719-160821.RDS") ## Specify baseline USAH you want to compare to

# Extract input edgelist
edgelist_location_baseline = USAH_location_baseline$edgelist

# Read in the OSMtidy count for the baseline
count_location_baseline = 
  read_rds("../OSMtidy-appl/outputs/Manchester/FINAL_count_3.0_Manchester_baseline_20210906.RDS") ## Specify 

# Read in the OSMtidy count for the flood scenario
count_location_flood = 
  read_rds("../OSMtidy-appl/outputs/Manchester/FINAL_count_3.0_Manchester_floodRiver-100_20221212-113122.RDS")


# 2. CREATE NEW EDGES -----------------------------------------------------

# Compare OSMtidy outputs for a baseline location and flood scenario
# to find the proportion of physical objects that remain functional
countCompared = 
  compareOSMtidy(
    baselineName = 
      paste0(AH_name, "_", AH_version, "_", AH_location, "_", AH_baseline), ## Specify baseline name
    countOSMtidy_baseline = count_location_baseline, ## Specify baseline OSMtidy count
    scenarioName = 
      paste0(AH_name, "_", AH_version, "_", AH_location, "_", AH_scenario), ## Specify flood scenario name
    countOSMtidy_scenario = count_location_flood) ## Specify flood scenario OSMtidy count
  
# Create edgesFlood
edgesFlood = 
  weight_hazard(
    vInfo_full = USAH_template_baseline$vInfo,
    edgelist = edgelist_location_baseline, ## Specify input edgelist
    hazard = "flood", ## Specify hazard type
    countCompared = countCompared) ## Specify countCompared

# !!! Make any manual adjustments !!!
# e.g. Removal centres link weight in Bristol 1-100 flood scenario
# & Day centres in Edinburgh 1-200 flood scenario
# See notes section for detailed checks


# Introduce edgesNew to create edgelist_location_flood
# Note that remove = FALSE is required so that we can track vExcluded
# before removing the edges with weight = 0 and converting to an igraph for analysis
edgelist_location_flood = 
  USAH_location_baseline$edgelist %>%
  weight_edges(edgesNew = edgesFlood, remove = FALSE) %>% # Swap in edgesNew weights for flood scenario
  weight_hangingVertices(remove = FALSE) # For vertices with downward weights all = 0, make upward weights = 0


# 3. GENERATE OUTPUT FOR USAH_SCENARIO ------------------------------------

# Generate output for USAH_scenario
USAH_scenario = 
  apply_scenario(    
    AH_input = USAH_location_baseline, ## Specify USAH_input you want to modify
    edgelist_scenario = edgelist_location_flood, ## Specify edgelist for new scenario
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)

# Inspect elements
USAH_scenario$vIncluded
USAH_scenario$vExcluded
USAH_scenario$adjMat
USAH_scenario$edgelist
USAH_scenario$igraph
USAH_scenario$results
USAH_scenario$summary


# 4. ADD SENSITIVITY ANALYSIS ---------------------------------------------

# A sensitivity analysis can help understand whether input errors 
# might substantially change your results. This section adds on results for 
# two additional scenarios: one that uses an edgelist where all link weights 
# that were affected are adjusted by +10%, and one adjusted by -10%.
# Note it is possible to change this percentage with the argument "pct" e.g.
# "pct = 0.2" for +/-20%.

# Generate sensitivity analysis
USAH_scenario_sensitivity = 
  apply_sensitivity(
    AH_input = USAH_scenario, 
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)

# Inspect elements
USAH_scenario_sensitivity[[1]]
USAH_scenario_sensitivity[[2]]
USAH_scenario_sensitivity[[3]]
USAH_scenario_sensitivity$summary$vertices
USAH_scenario_sensitivity$summary$edges
USAH_scenario_sensitivity$vExcluded
USAH_scenario_sensitivity$results


# 5. EXPORT ---------------------------------------------------------------

### USAH outputs should remain in AHgen-appl under the appropriate city folder ###
# This is to keep a record of the USAH outputs with network analysis results etc.
USAH_scenario_sensitivity %>% 
  export_AHgen(
    type = "USAH",
    directory = directory, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)

### countCompared can remain in OSMtidy-appl under compared folder ###
### Need to revisit export_countCompared to ensure this is in OSMtidy
# This is to keep a record of how different OSMtidy outputs compared to each other
countCompared %>% 
  export_countCompared(
    directory = "../OSMtidy-appl/outputs/~compared/", 
    version = version, 
    location = location, 
    baseline = baseline, 
    scenario = scenario)

### edgesFlood or other edges to change can be recorded in AHgen-appl under the flood folder ###
# This is to keep a record of what types of edges were modified for this 
# flood scenario, comparable to an edgeGuide
edgesFlood %>% 
  export_AHgen(
    type = "USAH_edges",
    directory = "data/hazards/flood/",
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)


# NOTES -------------------------------------------------------------------

## Bristol 1-100 flood scenario Removal Services to check
## https://www.google.com/maps/search/bristol+removal+services/@51.4581366,-2.6416926,11.92z

## 29 removal services are NOT in 1-100 Bristol flood extent
# Lovely Jubbly Removals, 29 Turtlegate Ave, Bristol BS13 8NN, not in 1-100 flood extent
# DL Removals, 540 Hareclive Rd, Bristol BS13 0LX, not in 1-100 flood extent
# Million Target Ltd, 49, Bristol BS13 9QT, not in 1-100 flood extent
# Langdon Removals Ltd, Block 12, Cater Business Park, Bristol BS13 7TT, not in 1-100 flood extent
# Bristol Man with a Van, 43 Headley Park Ave, Bristol BS13 7NR, not in 1-100 flood extent
# Abbah Removals, 50 Wrington Cres, Bristol BS13 7EP, not in 1-100 flood extent
# JC Man and Van Services, S Liberty Ln, Bristol BS3 2SU, not in 1-100 flood extent
# Bristol Removals, 2a Greville Rd, Southville, Bristol BS3 1LL, not in 1-100 flood extent
# Manley Van Removals, 8 Ilminster Ave, Bristol BS4 1LT, not in 1-100 flood extent
# Rock Steady Removals, 22 Newquay Rd, Bristol BS4 1EA, not in 1-100 flood extent
# KP Transport, 8 Greenwood Rd, Knowle, Bristol BS4 2SX, not in 1-100 flood extent
# Abacus Removals, 821 Bath Rd, Brislington, Bristol BS4 5NL, not in 1-100 flood extent
# Van Move It, 123 Raleigh Rd, Southville, Bristol BS3 1QU, not in 1-100 flood extent
# Kwik Move, Unit 3, Bonville Trading Estate, Bonville Rd, Brislington, Bristol BS4 5QU, not in 1-100 flood extent
# The Little Removals Company, Alpha Rd, Southville, Bristol BS3 1DH, not in 1-100 flood extent (only just)
# Excalibur Removals, Ridings Farm, Bridgwater Rd, Dundry, Bristol BS41 8JP, not in 1-100 flood extent
# Bristol Van Removals Ltd, Industrial Park, Unit 3 Netham Rd, Bristol BS5 9PQ, not in 1-100 flood extent
# Smartmove Removals Bristol, Wainbrook House, 1 Hudds Vale Rd, Bristol BS5 7HY, not in 1-100 flood extent
# SWFL Removals and Courier Service, Easton Business Centre, Felix Rd, Easton, Bristol BS5 0HE, not in 1-100 flood extent
# Man with Van Removals Bristol, 34 Cobourg Rd, Montpelier, Bristol BS6 5HX, not in 1-100 flood extent
# Big Van Man Removals, 21 Gordon Ave, Whitehall, Bristol BS5 7DY, not in 1-100 flood extent
# AJM Removals, 569 Fishponds Rd, Bristol BS16 3AF, not in 1-100 flood extent
# Macro Removals, 77 Gloucester Rd, Staple Hill, Bristol BS16 4SL, not in 1-100 flood extent
# Knights of Removals, White Lodge Rd, Bristol BS16 5ND, not in 1-100 flood extent
# Painless Removals, Henleaze House, Henleaze, Bristol BS9 4PN, not in 1-100 flood extent
# Bristol Man and Van, Hambrook Business Park, The Strm, Hambrook, Bristol BS16 1RQ, not in 1-100 flood extent
# We Like 2 Move It, 31 Wrington Cl, Little Stoke, Bristol BS34 6EU, not in 1-100 flood extent
# Wilkinson Removal & Storage, Unit 1b Grange Court Farm Units, Winterbourne, Bristol BS36 1RY, not in 1-100 flood extent
# Get Moved, 34 Mogg St, St Werburgh's, Bristol BS2 9UB, not in 1-100 flood extent (just)

## 3 removal services ARE in 1-100 Bristol flood extent
# Easymove Removals, Albert Cres, Bristol BS2 0SU, IS in 1-100 flood extent
# Move On Removals and Storage, 27-29 Baldwin St, Bristol BS1 1LT, right on the line - treated as IN 1-100 flood extent
# On the Dot Removals Bristol, 21 Horley Rd, St Werburgh's, Bristol BS2 9TL, IS in 1-100 flood extent

## 29/32 Removal services still functional, for a link weight of 0.90625

# Manual adjustments - Bristol 1-100 flood scenario
edgesFlood = 
  edgesFlood %>%
  mutate(weightNew = ifelse(to == 'Removal services', 0.90625, weightNew))

## Edinburgh 1-200 flood scenario Day centres to check
## https://www.edinburgh.gov.uk/directory/search?directoryID=10214&showInMap=&keywords=day+centre&postcode=&search=Search

##  All 4 Edinburgh Day centres NOT in 1-200 flood extent
# Currie Day Centre, Gibson Craig Hall, 142 Lanark Rd W, Edinburgh, Currie EH14 5NY, not in 1-200 flood extent
# Wednesday Centre Holyrood Abbey, Meadowbank Church, 83 London Rd, Edinburgh EH7 5TT, not in 1-200 flood extent
# Portobello Monday Centre, Baptist Church Hall, 185 Portobello High Street EH15 2EW, not in 1-200 flood extent
# Pleasance Day Centre, 7 West Adam St, Edinburgh EH8 9SX, not in 1-200 flood extent

## 4/4 Day centres still functional, for a link weight of 1

# Manual adjustments - Edinburgh 1-200 flood scenario
edgesFlood = 
  edgesFlood %>%
  filter(to != "Day centres")
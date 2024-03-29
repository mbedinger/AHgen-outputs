# =========================================================================
# 4 - Compare scenarios.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-26
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the RStudio environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory to the folder where the script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# and install Biobase (https://www.bioconductor.org/packages/release/bioc/html/Biobase.html) for subListExtract function
# install.packages("devtools")
# install.packages("pacman")
# if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("Biobase")


# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, tools, Biobase, AHgen)

# If it's been a while since installing Biobase or BiocManager and you receive 
# an error, try to install again as above


# COMPARE DIFFERENT LOCATIONS TO A TEMPLATE -------------------------------

# 1. READ IN DATA ---------------------------------------------------------

# Set name, version, benchmark / baseline scenario you are comparing against
# and scenarios you are assessing 
AH_name = "USAH"
AH_version = "3.0"
AH_benchmark = "template-baseline"
AH_scenario = "fiveCities-baselines"

# Read in the USAH_input you want to use as a point of comparison
USAH_benchmark =
  read_rds("data/template/3.0/USAH_3.0_template_baseline_20230719-104712.RDS")

# Create a designated folder (e.g. "~toCompare") where you've copied all individual
# scenario outputs you are comparing to the benchmark
# Read in all the file names from the designated folder
scenarioFilenames = 
  list.files(path = "./data/~toCompare/fiveCities_updatedStructure", 
             pattern = ".*USAH_.*\\.RDS", full.names = TRUE)

# Read in all scenario files to a list of lists
scenarios_toCompare = lapply(scenarioFilenames, function (x) readRDS(x))

# Create vector for file names without path and extension
scenarioNames = 
  list.files(path = "./data/~toCompare/fiveCities_updatedStructure", 
             pattern = ".*USAH_.*\\.RDS") %>% 
  tools::file_path_sans_ext()
scenarioNames

# Set names of allScenarios list elements to reflect correct scenario
names(scenarios_toCompare) = scenarioNames


# 2. COMPARE STRUCTURE ----------------------------------------------------

allScenarios_compared = 
  compare_AH(type = "USAH",
             AH_benchmark = USAH_benchmark, 
             scenarios_toCompare = scenarios_toCompare, 
             scenarioNames = scenarioNames)


# 3. EXPORT ---------------------------------------------------------------

# Write out to RDS
allScenarios_compared %>% 
  export_AHgen(
    type = "USAH_compared",
    directory = "data/~compared/", ## Specify directory
    AH_version = AH_version, 
    AH_benchmark = AH_benchmark, 
    AH_scenario = AH_scenario,
    xl = FALSE)



# COMPARE A HAZARD TO A BASELINE ------------------------------------------

# 1. READ IN DATA ---------------------------------------------------------

# Set name, version, benchmark / baseline scenario you are comparing against
# and scenarios you are assessing 
AH_name = "USAH"
AH_version = "3.0"
AH_benchmark = "Manchester-baseline"
AH_scenario = "Manchester-floodRiver-100"

# Read in the USAH_input you want to use as a point of comparison
USAH_benchmark =
  read_rds("data/cities/Manchester/USAH_3.0_Manchester_baseline_20230719-160821.RDS")

# Create a designated folder (e.g. "~toCompare") where you've copied all individual
# scenario outputs you are comparing to the benchmark
# Read in all the file names from the designated folder
scenarioFilenames = 
  list.files(path = "./data/~toCompare/Manchester_floodRiver-100", 
             pattern = ".*USAH_.*\\.RDS", full.names = TRUE)

# Read in all scenario files to a list of lists
scenarios_toCompare = lapply(scenarioFilenames, function (x) readRDS(x))

# Create vector for file names without path and extension
scenarioNames = 
  list.files(path = "./data/~toCompare/Manchester_floodRiver-100", 
             pattern = ".*USAH_.*\\.RDS") %>% 
  tools::file_path_sans_ext()

# Set names of allScenarios list elements to reflect correct scenario
names(scenarios_toCompare) = scenarioNames


# 2. COMPARE STRUCTURE ----------------------------------------------------

allScenarios_compared = 
  compare_AH(type = "USAH",
             AH_benchmark = USAH_benchmark, 
             scenarios_toCompare = scenarios_toCompare, 
             scenarioNames = scenarioNames)


# 3. EXPORT ---------------------------------------------------------------

allScenarios_compared %>% 
  export_AHgen(
    type = "USAH_compared",
    directory = "data/~compared/", ## Specify directory
    AH_version = AH_version, 
    AH_benchmark = AH_benchmark, 
    AH_scenario = AH_scenario,
    xl = FALSE)



# COMPARE HAZARDS ACROSS MULTIPLE LOCATIONS -------------------------------

# 1. READ IN DATA ---------------------------------------------------------

# Set names
AH_name = "USAH"
AH_version = "3.0"
AH_benchmark = "fiveCities-baseline"
AH_scenario = "fiveCities-floodRiver-100-200"

# Read in city flood comparisons
Bristol =   
  read_rds(
    "data/~compared/comparison_USAH_3.0_Bristol-baseline_Bristol-floodRiver-100_20230720-173152.RDS") ## Specify filename

Edinburgh = 
  read_rds(
    "data/~compared/comparison_USAH_3.0_Edinburgh-baseline_Edinburgh-floodRiver-200_20230720-173512.RDS") ## Specify filename

Glasgow = 
  read_rds(
    "data/~compared/comparison_USAH_3.0_Glasgow-baseline_Glasgow-floodRiver-200_20230720-173627.RDS") ## Specify filename

LondonCentral = 
  read_rds(
    "data/~compared/comparison_USAH_3.0_London-Central-baseline_London-Central-floodRiver-100_20230720-173814.RDS") ## Specify filename

Manchester = 
  read_rds(
    "data/~compared/comparison_USAH_3.0_Manchester-baseline_Manchester-floodRiver-100_20230720-173929.RDS") ## Specify filename


# 2. COMBINE --------------------------------------------------------------

fiveCities_floodRiver_combined = 
  Bristol %>%
  purrr::map2(Edinburgh, rbind) %>%
  purrr::map2(Glasgow, rbind) %>%
  purrr::map2(LondonCentral, rbind) %>%
  purrr::map2(Manchester, rbind)

fiveCities_floodRiver_combined$confidence =
  fiveCities_floodRiver_combined$results %>%
  summarise_confidence()


# 3. EXPORT ---------------------------------------------------------------

fiveCities_floodRiver_combined %>% 
  export_AHgen(
    type = "USAH_compared",
    directory = "data/~compared/", ## Specify directory
    AH_version = AH_version, 
    AH_benchmark = AH_benchmark, 
    AH_scenario = AH_scenario,
    xl = FALSE)
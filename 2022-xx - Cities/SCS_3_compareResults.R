# =========================================================================
# SCS_3_compareResults.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2022-02-28
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(tidyverse, openxlsx, readxl, data.table, stats,
               igraph, tnet, janitor, Matrix, mgcv, Biobase, tools, installr)

# If you have an issue, ensure Biobase is installed manually
# https://www.bioconductor.org/packages/release/bioc/html/Biobase.html
#  if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("Biobase")


# 0.2 EXTRA FUNCTIONS -----------------------------------------------------

# Load AHgen functions
source("functions/functions.R")

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 1. READ IN DATA ---------------------------------------------------------

# If it is not already in the environment, read in the USAH_input you want 
# to use as a point of comparison, e.g. USAH_template_baseline
USAH_baselineScenario =
  read_rds("USAH_3.0_template_baseline_20210910.RDS") ## Specify filename

# Create a separate folder toCompare where you've copied all individual
# scenario outputs you are comparing, and read in all the file names from there
scenarioFilenames = 
  list.files(path = "resultsCompared/toCompare/", pattern = ".*USAH_.*\\.RDS", full.names = TRUE)

# Create vector for file names without path and extension
## Can clean this up for labeling
scenarioNames = 
  list.files(path = "resultsCompared/toCompare/", pattern = ".*USAH_.*\\.RDS") %>% 
  file_path_sans_ext

# Read in all scenario files to a list of lists
allScenarios = lapply(scenarioFilenames, function (x) readRDS(x))

# Set names of allScenarios list elements to reflect correct scenario
names(allScenarios) = scenarioNames

# Create list output to attach comparisons to
allScenarios_compared = list()


# 2. COMPARE STRUCTURE ----------------------------------------------------

# Pull out only the $summary$vertices list elements from allScenarios and reformat
allScenarios_compared$vertices = 
  allScenarios %>% 
  subListExtract("summary") %>%
  subListExtract("vertices") %>%
  Map(cbind, ., scenarioName = scenarioNames) %>%
  lapply(function (x) {
      x %>%
      as.data.frame() %>%
      mutate(scenario_dummy = scenarioName) %>%
      separate(scenario_dummy, 
               c("remove", "version", "location", "scenario", "date"), 
               sep = "_") %>%
      select(-remove)
  }) %>%
  discard(function(x) nrow(x) == 1) %>%
  do.call(rbind, .) %>%
  remove_rownames() %>%
  mutate(levelName_viz = 
           case_when(
             level == 1 ~ str_c(level, " - ", levelName),
             level == 2 ~ str_c(level, " - ", levelName),
             level == 3 ~ str_c(level, " - ", levelName),
             level == 4 ~ str_c(level, " - ", levelName),
             level == 5 ~ str_c(level, " - ", levelName),
             level == "Total" ~ "Total"),
         levelName_viz = 
           factor(levelName_viz, 
                  levels = c("1 - Functional purposes",
                             "2 - Values and priority measures",
                             "3 - Generalised functions",
                             "4 - Object-related processes",
                             "5 - Physical objects",
                             "Total"))) %>%
  rename(n_vertices = n)


# Pull out only the $summary$edges list elements from allScenarios and reformat
allScenarios_compared$edges = 
  allScenarios %>% 
  subListExtract("summary") %>%
  subListExtract("edges") %>%
  Map(cbind, ., scenarioName = scenarioNames) %>%
  lapply(function (x) {
    x %>%
      as.data.frame() %>%
      mutate(scenario_dummy = scenarioName) %>%
      separate(scenario_dummy, 
               c("remove", "version", "location", "scenario", "date"), 
               sep = "_") %>%
      select(-remove)
  }) %>%
  discard(function(x) nrow(x) == 1) %>%
  do.call(rbind, .) %>%
  remove_rownames() %>%
  mutate(layerName_viz = 
           factor(layer, levels = c("l1FP_l2VPM",
                                    "l2VPM_l3GF",
                                    "l3GF_l4ORP",
                                    "l4ORP_l5PO",
                                    "Total"))) %>%
  rename(n_edges = n)


# 3. COMPARE VEXCLUDED ----------------------------------------------------

# Pull out only the $vExcluded list elements from allScenarios and reformat
allScenarios_compared$vExcluded = 
  allScenarios %>% 
  subListExtract("vExcluded") %>% 
  Map(cbind, ., scenarioName = scenarioNames) %>%
  lapply(function (x) {
    x %>%
      as.data.frame() %>%
      mutate(scenario_dummy = scenarioName) %>%
      separate(scenario_dummy, 
               c("remove", "version", "location", "scenario", "date"), 
               sep = "_") %>%
      select(-remove)
  }) %>%
  discard(function(x) nrow(x) == 1) %>%
  do.call(rbind, .) %>%
  remove_rownames() %>%
  rename(Node = vName) %>%
  mutate(levelName_viz = str_c(level, " - ", levelName),
         levelName_viz = 
           factor(levelName_viz, levels = c("1 - Functional purposes",
                                            "2 - Values and priority measures",
                                            "3 - Generalised functions",
                                            "4 - Object-related processes",
                                            "5 - Physical objects")))
         

# 4. COMPARE RESULTS ------------------------------------------------------

# Pull out only the $results list elements from allScenarios and reformat
resultsCompared_step1 = 
  allScenarios %>% 
  subListExtract("results") %>% 
  Map(cbind, ., scenarioName = scenarioNames) %>%
  lapply(function (x) {
    x %>% 
      mutate(scenario_dummy = scenarioName) %>%
      separate(scenario_dummy, 
               c("remove", "version", "location", "scenario", "date"), 
               sep = "_") %>%
      select(-remove)}) %>%
  do.call(rbind, .) %>%
  remove_rownames()


# Key points here: 

# baseline_value_amp and value_amp columns amplify the SBC_norm values by * 100000
# to make them easier to distinguish quickly by eye

# baseline_value_amp and value_amp columns leave all other metric types out
# so e.g. baseline_value_amp and baseline_value are the same for metric == EC
# because EC is already normalized on a 0-1 scale

# change_pct uses baseline_value_amp and value_amp
# change_pct is itself amplified * 100 to make it easier to distinguish by eye
# where a value of 7.5 = 7.5% (not 750%)

allScenarios_compared$results =
  USAH_baselineScenario$results %>%
  rename(baseline_value = value,
         baseline_rankByLevel = rank_byLevel,
         baseline_rankOverall = rank_overall) %>%
  full_join(resultsCompared_step1, 
            by = c("level", "levelName", "vName", "metric")) %>%
  rename(Node = vName) %>%
  mutate(levelName_viz = str_c(level, " - ", levelName),
         baseline_value_amp = ifelse(metric == "SBC_norm", baseline_value * 100000, baseline_value), # SBC_norm values are amplified here by 100000 to make them easier to distinguish by eye
         value_amp = ifelse(metric == "SBC_norm", value * 100000, value), # SBC_norm values are amplified here by 100000 to make them easier to distinguish by eye
         change_value_amp = value_amp - baseline_value_amp,
         change_pct = 
           ifelse(baseline_value == 0 & value == 0,
                  0,
                  ((value_amp - baseline_value_amp) / baseline_value_amp) * 100), # changePct is after * 100 for % value
         change_rankByLevel = baseline_rankByLevel - rank_byLevel, # changed direction on 2021-08-25 to reflect a lesser number = + change in rank
         change_rankOverall = baseline_rankOverall - rank_overall) %>% # changed direction on 2021-08-25 to reflect a lesser number = + change in rank
  mutate(levelName_viz = 
           factor(levelName_viz, levels = c("1 - Functional purposes",
                                            "2 - Values and priority measures",
                                            "3 - Generalised functions",
                                            "4 - Object-related processes",
                                            "5 - Physical objects"))) %>%
  select(level, levelName, Node, # basic identifiers
         baseline_value, baseline_value_amp, baseline_rankByLevel, baseline_rankOverall, # baseline results
         scenarioName, metric, value, value_amp, rank_byLevel, rank_overall, # scenario results
         change_value_amp, change_pct, change_rankByLevel, change_rankOverall, # change
         levelName_viz, version, location, scenario, date) # visualisation helpers



# 3. EXPORT ---------------------------------------------------------------

# Write out to RDS
allScenarios_compared %>% saveRDS(filenameTimestamp(
  prefix = "resultsCompared/comparison_3.0_fiveCities",  ## Specify filepath and filename
  extension = ".RDS"))

# Write out to xlsx
allScenarios_compared %>% write.xlsx(filenameTimestamp(
  prefix = "resultsCompared/comparison_3.0_fiveCities", ## Specify filepath and filename
  extension = ".xlsx"))

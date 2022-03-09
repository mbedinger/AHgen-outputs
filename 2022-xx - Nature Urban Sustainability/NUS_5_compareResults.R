# =========================================================================
# NUS_5_compareResults.R

# Note: Part 4

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2020-10-10
# Last updated: 2020-12-15
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
library(Biobase)
library(viridis)
library(ggrepel)
library(stringi)
library(stringr)
library(scales)

source("functions/functions.R")

ls()


# 2. FUNCTIONS ------------------------------------------------------------
# Function to compare baseline and each scenario, then reorganise into one
# dataframe from which plots can be made
compareResults <- function(baselineScenario, scenarioNames) {
  
  # Create empty list to add outputs to
  outputList <- list()
  
  # Function to label outliers
  is_outlier <- function(x) {
    return(
      x < quantile(as.numeric(x), 0.25, na.rm = TRUE) - 1.5 * IQR(as.numeric(x), na.rm = TRUE) | 
        x > quantile(as.numeric(x), 0.75, na.rm = TRUE) + 1.5 * IQR(as.numeric(x), na.rm = TRUE))
  }
  
  for (x in seq_along(scenarioNames)) { # for loop to iterate {lines} for each specific scenario
    
    step1a <-
      calcChangeNewBC( # calcChange will create a new dataframe that compares baseline (before) and scenario (after)
        before = USAH_template_baseline$results %>% filter(type == "EC") %>%
          mutate(centrality = centrality * 1000),
        after = allScenarios_results[[x]] %>% filter(type == "EC") %>% 
          mutate(centrality = centrality * 1000),
        metric = "centrality") %>%
      mutate(type = "EC", .before = 1) 
    step1b <-
      calcChange( # calcChange will create a new dataframe that compares baseline (before) and scenario (after)
        before = USAH_template_baseline$results %>% filter(type == "WVBC"), 
        after = allScenarios_results[[x]] %>% filter(type == "WVBC"),
        metric = "centrality") %>%
      mutate(type = "WVBC", .before = 1)
    step1 <- 
      rbind(step1a, step1b) %>% 
      rename(baseline_result_centrality = before,  # rename columns for clarity when dealing with multiple scenarios
             scenario_result_centrality = after, 
             scenario_result_centralityChange = absChange_afterMinusBefore,
             scenario_result_pctChange = pctChange)
    
    step2 <-
      allScenarios_results[[x]] %>%
      select(type, vName, rank_overall, rank_byLevel) %>%
      rename(scenario_result_rankOverall = rank_overall, 
             scenario_result_rankByLevel = rank_byLevel)
    
    output <-
      step1 %>%
      full_join(step2, by = c("type", "vName")) %>%
      gather(resultType, result, # put columns related to scenario results into "long format"
             scenario_result_centrality:scenario_result_rankByLevel) %>% 
      mutate_(scenario = x) %>% # add column to specify which scenario each result corresponds to ## need to find way to directly use character string not number
      group_by(resultType, level) %>% # group by resultType and level so that the following outlier function calculates this for each group
      mutate(outlierLabel = ifelse(is_outlier(result), vName, NA)) %>% # calculate outliers and where these exist assign an outlierLabel to be used in plots
      ungroup
    
    outputList[[x]] <- output # add output for specific scenario to the overall list
    
  }
  
  # Change names of outputList list elements to reflect correct scenario
  names(outputList) <- scenarioNames
  
  # Return outputList
  outputList
  
  # Bind outputList list elements into one dataframe
  outputDataframe <- outputList %>% do.call("rbind", .)
  
  # Workaround to replace scenario numbers with scenario names
  scenarioKey <- scenarioNames %>% as.data.frame %>% setDT(keep.rownames = TRUE) %>% rename(number = rn, name = ".")
  outputDataframe$scenario = scenarioKey$name[match(outputDataframe$scenario, scenarioKey$number)]
  
  ## Add locationLabel and scenarioLabel column for cleaner plots etc. ? May require location+baseline/hazard+week[COVID] if we are going to layer flood hazard + week x of COVID
  
  # Return outputDataframe
  outputDataframe
  
}

# Function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}

# 3. READ IN INPUT DATA ---------------------------------------------------
# If it is not already in the environment, read in the USAH_input you want 
# to use as a point of comparison, e.g. USAH_template_baseline
USAH_template_baseline = # Change name
  read_rds("outputs/USAH_Edinburgh_IAA_baseline_20210322-131717.RDS") ## Specify filename

edin_baseline_results <- USAH_template_baseline$results ## Have a quick look at results

# Create a separate folder "testScenarios" where you've copied all individual
# scenario outputs you are comparing, and read in all the file names 
# automatically from there
scenarioFilenames = list.files(
  path = "./testScenarios/", pattern = ".*USAH_.*\\.RDS", full.names = TRUE)

# Create vector for file names without path and extension
## Can clean this up for labeling
scenarioNames = list.files(
  path = "./testScenarios/", pattern = ".*USAH_.*\\.RDS") %>% file_path_sans_ext

# Read in all scenario files to a list of lists
allScenarios = lapply(scenarioFilenames, function (x) readRDS(x))

# Set names of allScenarios list elements to reflect correct scenario
names(allScenarios) = scenarioNames

# Pull out only the $results list elements from allScenarios
allScenarios_results = allScenarios %>% subListExtract("results")


# 4. COMPARE RESULTS ------------------------------------------------------
# Create dataframe of compared results
allScenarios_resultsCompared = 
  compareResults(
    baselineScenario = USAH_template_baseline$results, ## Specify filename
    scenarioNames = scenarioNames)
allScenarios_resultsCompared


# 5. SAVE OUTPUTS ---------------------------------------------------------
# Export as RDS if desired
allScenarios_resultsCompared %>% saveRDS(filenameTimestamp(
  prefix = "outputs/USAH_1.1_resultsCompared_Edinburgh_COVID", extension = ".RDS")) ## Specify filename

# Export as CSV if desired
allScenarios_resultsCompared %>% write.csv(filenameTimestamp(
  prefix = "outputs/USAH_1.1_resultsCompared_Edinburgh_COVID", extension = ".csv")) ## Specify filename
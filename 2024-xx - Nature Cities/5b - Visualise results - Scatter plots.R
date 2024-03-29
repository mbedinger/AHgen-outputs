# =========================================================================
# 5b - Visualise results - Scatter plots.R

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com), Dr David Morrison (dh48@hw.ac.uk), Dr Gordon Aitken (ga41@hw.ac.uk) & Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-09-06

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-25
# =========================================================================

  
# PREP --------------------------------------------------------------------

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# and install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# install.packages("devtools")
# install.packages("pacman")

# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, showtext, tidytext, ggh4x, AHgen)


# 0.2. STYLE --------------------------------------------------------------

# You can specify your font of choice in the plot functions from section 0.3
# by using the argument "family"
# I prefer a custom font which loosely matches Nature journals
# After downloading ttf files, load the custom font using the showtext package
font_add(family = "Harding", regular = "./aes/fonts/HardingText-Regular-Web.ttf")
showtext_auto()

# Colours can be loaded from AHgen example data e.g. colsFloodRiver_df or colsFloodRiver
# Or you can add custom colours by editing the plot functions in section 0.3

# DATA --------------------------------------------------------------------

# 1.1. READ IN DATA -------------------------------------------------------

# Set names for directory and study
directory = "./plots/"
study = "fiveCities-floodRiver-100-200"

# Read in data
# E.g. comparison of scenarios
allScenarios_compared =
  read_rds("data/~compared/comparison_USAH_3.0_fiveCities-baseline_fiveCities-floodRiver-100-200_20230720-174337.RDS") ## Specify filename


# 1.2. FORMAT DATA --------------------------------------------------------

# Reformat scenario column to rename into a more visually friendly format
# and assign to ordered levels

results = 
  allScenarios_compared$results %>%
  mutate(scenario = recode(scenario,
                           'floodRiver-100' = '1 in 100-year flood',
                           'floodRiver-200' = '1 in 200-year flood')) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("baseline", 
                                      "1 in 100-year flood", 
                                      "1 in 200-year flood"))) %>%
  full_join(colsFloodRiver_df)


# SCATTER PLOTS -----------------------------------------------------------

# If plotting 3 levels by 5 locations, width = 6 & height = 3 at dpi = 600 is a good start
# If plotting 2 levels by 5 locations, width = 6 & height = 2.5 at dpi = 600 is a good start
# If plotting 1 level by 5 locations, width = 4 & height = 3 at dpi = 600 is a good start

# 2.1. SCATTER PLOT - FACETED - EC VALUES ---------------------------------

# Create plot
scatter_all_ECvals = 
  vis_plotScatter(results = results, metricName = "EC", type = "values")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_ECvals"), extension = ".png"),
  plot = scatter_all_ECvals, width = 6, height = 3, dpi = 600)


# 2.2. SCATTER PLOT - FACETED - EC VALUES CHANGE --------------------------

# Create plot
scatter_all_ECvalsChange = 
  vis_plotScatter(results = results, metricName = "EC", type = "valuesChange")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_ECvalsChange"), extension = ".png"),
  plot = scatter_all_ECvalsChange, width = 6, height = 3, dpi = 600)


# 2.3. SCATTER PLOT - FACETED - EC PERCENT CHANGE -------------------------

# Create plot
scatter_all_ECpctChange = 
  vis_plotScatter(results = results, metricName = "EC", type = "percentChange")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_ECpctChange"), extension = ".png"),
  plot = scatter_all_ECpctChange, width = 6, height = 3, dpi = 600)


# 2.4. SCATTER PLOT - FACETED - SBC VALUES --------------------------------

# Create plot
scatter_all_SBCvals = 
  vis_plotScatter(results = results, metricName = "SBC_norm", type = "values")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_SBCvals"), extension = ".png"),
  plot = scatter_all_SBCvals, width = 6, height = 3, dpi = 600)


# 2.5. SCATTER PLOT - FACETED - SBC VALUES CHANGE -------------------------

# Create plot
scatter_all_SBCvalsChange = 
  vis_plotScatter(results = results, metricName = "SBC_norm", type = "valuesChange")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_SBCvalsChange"), extension = ".png"),
  plot = scatter_all_SBCvalsChange, width = 6, height = 3, dpi = 600)


# 2.6. SCATTER PLOT - FACETED - SBC PERCENT CHANGE ------------------------

# Create plot
scatter_all_SBCpctChange = 
  vis_plotScatter(results = results, metricName = "SBC_norm", type = "percentChange")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_all_SBCpctChange"), extension = ".png"),
  plot = scatter_all_SBCpctChange, width = 6, height = 3, dpi = 600)


# 2.7. SCATTER PLOT - VARIATIONS ------------------------------------------

# The above plots use default settings for some arguments e.g. levels or locations.
# If you want to vary this, you can filter for specific levels or locations.

# You can also opt to include 0-value data points with omit.zeros = FALSE,
# and/or include infinity-value data points (those arising from comparing 
# a node's change from a 0-value to a positive-value) with omit.inf = FALSE.
# These often occur for SBC_norm in Resources (level 5).

# When exporting, remember to adjust width & height for desired presentation.


# Smaller subset of levels and locations
# Create plot
scatter_variation1_ECvals = 
  vis_plotScatter(
    results = results,
    levels = c("Outcomes", "Tasks"), 
    locations = c("Edinburgh", "Glasgow"),
    metricName = "EC", 
    type = "values")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_variation1_ECvals"), extension = ".png"),
  plot = scatter_variation1_ECvals, width = 6, height = 2.5, dpi = 600)

# Single level and smaller subset of locations
# Create plot
scatter_variation2_ECvals = 
  vis_plotScatter(
    results = results,
    levels = c("Tasks"), 
    locations = c("Edinburgh", "Glasgow"),
    metricName = "EC", 
    type = "values")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_variation2_ECvals"), extension = ".png"),
  plot = scatter_variation2_ECvals, width = 5, height = 2.5, dpi = 600)

# Single level and single locations
# Create plot
scatter_variation3_ECvals = 
  vis_plotScatter(
    results = results, 
    levels = c("Tasks"), 
    locations = c("Edinburgh"),
    metricName = "EC", 
    type = "values")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_scatter_variation3_ECvals"), extension = ".png"),
  plot = scatter_variation3_ECvals, width = 4, height = 3, dpi = 600)
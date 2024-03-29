# =========================================================================
# 5a - Visualise results - Violin plots.R

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
pacman::p_load(tidyverse, showtext, ggh4x, see, AHgen)

    
# 0.2. STYLE --------------------------------------------------------------

# You can specify your font of choice in the plot functions from section 0.3
# by using the argument "family"
# I prefer a custom font which loosely matches Nature journals
# After downloading ttf files, load the custom font using the showtext package
font_add(family = "Harding", regular = "./aes/fonts/HardingText-Regular-Web.ttf")
showtext_auto()

# Colours can be loaded from AHgen example data e.g. colsFloodRiver_df or colsFloodRiver
# Or you can add custom colours by adapting the plot functions


# DATA --------------------------------------------------------------------

# 1.1. READ IN DATA -------------------------------------------------------

# Set names for directory and study
directory = "./plots/"
study = "fiveCities-floodRiver-100-200"

# Read in comparison of scenarios
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


# VIOLIN PLOTS - FACETED --------------------------------------------------

# 2.1. VIOLIN PLOT - FACETED HALF - EC VALUES -----------------------------

# Create plot
violinHalf_ECvals = 
  vis_plotViolin(results = results, metricName = "EC", family = "Harding")

# Export
ggsave(
  filenameTimestamp(prefix = paste0(directory, study, "_violinHalf_ECvals"), extension = ".png"),
  plot = violinHalf_ECvals, width = 4, height = 3, dpi = 600)


# 2.2. VIOLIN PLOT - FACETED OVERLAP - EC VALUES --------------------------

# Create plot
violinOverlap_ECvals = 
  vis_plotViolin(results = results, metricName = "EC", type = "overlap", family = "Harding")

# Export
ggsave(
  filenameTimestamp(prefix = paste0(directory, study, "_violinOverlap_ECvals"), extension = ".png"),
  plot = violinOverlap_ECvals, width = 4, height = 3, dpi = 600)


# 2.3. VIOLIN PLOT - FACETED HALF - SBC VALUES ----------------------------

# Create plot
violinHalf_SBCvals = 
  vis_plotViolin(results = results, metricName = "SBC_norm", family = "Harding")

# Export
ggsave(
  filenameTimestamp(prefix = paste0(directory, study, "_violinHalf_SBCvals"), extension = ".png"),
  plot = violinHalf_SBCvals, width = 4, height = 3, dpi = 600)


# 2.4. VIOLIN PLOT - FACETED OVERLAP - SBC VALUES -------------------------

# Create plot
violinOverlap_SBCvals = 
  vis_plotViolin(results = results, metricName = "SBC_norm", type = "overlap", family = "Harding")

# Export
ggsave(
  filenameTimestamp(prefix = paste0(directory, study, "_violinOverlap_SBCvals"), extension = ".png"),
  plot = violinOverlap_SBCvals, width = 4, height = 3, dpi = 600)
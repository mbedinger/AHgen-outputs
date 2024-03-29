# =========================================================================
# 5d - Visualise results - Confidence plots.R

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
pacman::p_load(tidyverse, showtext, ggh4x, tidytext, AHgen)


# 0.2. STYLE --------------------------------------------------------------

# You can specify your font of choice in the plot functions from section 0.3
# by using the argument "family"
# I prefer a custom font which loosely matches Nature journals
# After downloading ttf files, load the custom font using the showtext package
font_add(family = "Harding", regular = "./aes/fonts/HardingText-Regular-Web.ttf")
showtext_auto()

# Colours can be loaded from AHgen example data e.g. colsFloodRiver_df or colsFloodRiver
# Or you can add custom colours by editing the plot functions in section 0.3

# 1. READ IN DATA ---------------------------------------------------------

# Set names for directory and study
directory = "./plots/"
study = "fiveCities-floodRiver-100-200"

# Read in data
# E.g. comparison of scenarios
allScenarios_compared =
  read_rds("data/~compared/comparison_USAH_3.0_fiveCities-baseline_fiveCities-floodRiver-100-200_20230720-174337.RDS") ## Specify filename


# 2.1. CONFIDENCE PLOT - EC - BY SCENARIO & LEVEL -------------------------

# Create plot
confidence_EC_scenarioLevel = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "EC", 
    type = "scenarioLevel")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_EC_scenarioLevel"), 
    extension = ".png"),
  plot = confidence_EC_scenarioLevel, width = 4, height = 3, dpi = 600)


# 2.2. CONFIDENCE PLOT - SBC - BY SCENARIO & LEVEL ------------------------

# Create plot
confidence_SBC_scenarioLevel = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "SBC_norm", 
    type = "scenarioLevel")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_SBCnorm_scenarioLevel"), 
    extension = ".png"),
  plot = confidence_SBC_scenarioLevel, width = 4, height = 3, dpi = 600)


# 2.3. CONFIDENCE PLOT - EC - BY SCENARIO (ALL LEVELS) --------------------

# Create plot
confidence_EC_scenario = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "EC", 
    type = "scenario")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_EC_scenario"), 
    extension = ".png"),
  plot = confidence_EC_scenario, width = 4, height = 2, dpi = 600)


# 2.4. CONFIDENCE PLOT - SBC - BY SCENARIO (ALL LEVELS) -------------------

# Create plot
confidence_SBC_scenario = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "SBC_norm", 
    type = "scenario")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_SBCnorm_scenario"), 
    extension = ".png"),
  plot = confidence_SBC_scenario, width = 4, height = 2, dpi = 600)


# 2.5. CONFIDENCE PLOT - EC - BY LEVEL (ALL SCENARIOS) --------------------

# Create plot
confidence_EC_level = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "EC", 
    type = "level")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_EC_level"), 
    extension = ".png"),
  plot = confidence_EC_level, width = 2.5, height = 3, dpi = 600)


# 2.6. CONFIDENCE PLOT - SBC - BY LEVEL (ALL SCENARIOS) -------------------

# Create plot
confidence_SBC_level = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "SBC_norm", 
    type = "level")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_SBCnorm_level"), 
    extension = ".png"),
  plot = confidence_SBC_level, width = 2.5, height = 3, dpi = 600)


# 2.7. CONFIDENCE PLOT - EC - OVERALL -------------------------------------

# Create plot
confidence_EC_overall = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "EC", 
    type = "overall")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_EC_overall"), 
    extension = ".png"),
  plot = confidence_EC_overall, width = 3, height = 3, dpi = 600)


# 2.8. CONFIDENCE PLOTS - SBC - OVERALL -----------------------------------

# Create plot
confidence_SBC_overall = 
  vis_plotConfidence(
    results = allScenarios_compared, 
    metricName = "SBC_norm", 
    type = "overall")

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_confidence_SBCnorm_overall"), 
    extension = ".png"),
  plot = confidence_SBC_overall, width = 3, height = 3, dpi = 600)
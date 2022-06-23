# =========================================================================
# functionDependencies.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-07-14
#
# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2021-07-14
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(
  tidyverse, readxl)

source("functions/functions.R")

# 1. READ IN DATA ---------------------------------------------------------

# Read in xlsx
dependencies <- readxl::read_xlsx("functions/functionDependencies.xlsx")

dependencies %>% graph_from_data_frame()

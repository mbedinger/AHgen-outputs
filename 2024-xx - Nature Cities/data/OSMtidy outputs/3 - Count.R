# =========================================================================
# Count.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Created: 2020-09-17
#
# Last revised by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Last revised: 2022-12-12
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy package is installed
devtools::install_github("avisserquinn/OSMtidy@dev")

# Load required packages
pacman::p_load(tidyverse, openxlsx, OSMtidy, sf, data.table, lwgeom)


# 0.2 EXTRA FUNCTIONS -----------------------------------------------------

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}

# Helper function to export Excel workbook
exportExcel <- function(inputList, filename) {
  
  require(openxlsx)
  
  wb <- openxlsx::createWorkbook()
  
  input <- inputList
  
  # sheetnames <- paste0("Sheet", seq_along(input))
  sheetnames <- names(input) %>% str_sub(1,20)
  lsn = length(sheetnames)
  snid = .create_unique_ids(lsn, char_len = 3)
  sheetnames <- paste0(1:lsn, "_", snid, "_", sheetnames)
  
  Map(function(data, nameofsheet){
    
    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeDataTable(wb, nameofsheet, data, rowNames = FALSE)
    
  }, input, sheetnames)
  
  openxlsx::saveWorkbook(wb, file = filename)
  
}

# OSMtidy internal function .create_unique_ids for exportExcel
.create_unique_ids <- function(n, seed_no = 1, char_len = 5){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)
  
  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}


# 1. READ IN DATA ---------------------------------------------------------

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "OSM-AH-key_3.0_20210906.csv") ## Specify filename for OSMtidy-AHgen key

# Read in OSMtidy output RDS file
geoData = readRDS(
  "Manchester/Manchester_floodRiver-100_7_postProcessing_sf_20221202-194747.RDS") ## Specify filename for OSMtidy output

# Or read in OSMtidy output CSV file
# geoData = read_csv("filename.csv") ## Specify filename for OSMtidy output


# 2. COUNT ----------------------------------------------------------------

# Create summary counts for OSMtidy desc
# To return a breakdown only by desc
# use either countOSMtidy() or countOSMtidy(AHgen = FALSE) 
# count_OSMtidy <- geoData %>% countOSMtidy() 

# Create summary counts for OSMtidy desc, AHgen physical object
# To return a breakdown by desc, physicalObject, and category
# use countOSMtidy(AHgen = TRUE)
count_AHgen <- geoData %>% countOSMtidy(AHgen = TRUE)

# Inspect
count_AHgen$byPhysicalObject_all


# 3. SAVE OUTPUT ----------------------------------------------------------

# Write out RDS
count_AHgen %>% # Specify output object
  saveRDS(filenameTimestamp(
    prefix = "Manchester/FINAL_count_3.0_Manchester_floodRiver-100", ## Specify output filepath and filename
    extension = ".RDS")) ## Specify output filename

# Write out xlsx workbook with multiple worksheets
exportExcel(
  inputList = count_AHgen, # Specify output object 
  filenameTimestamp(
    prefix = "Manchester/FINAL_count_3.0_Manchester_floodRiver-100", ## Specify filepath and filename
    extension = ".xlsx"))


# NOTES -------------------------------------------------------------------

# Create basic template for count plots

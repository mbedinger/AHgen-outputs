# =========================================================================
# Recorded & historical flood processing.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2023-08-15
#
# Last revised by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Last revised: 2023-08-17
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy package is installed
devtools::install_github("avisserquinn/OSMtidy@dev")

# Load required packages
pacman::p_load(OSMtidy, sf, s2, progress, tidyverse, gdata, rgdal, leaflet)


# RECORDED FLOODS WITHIN A CITY COUNCIL BOUNDARY --------------------------

shp_recorded = st_read("shapefiles/Recorded_Flood_Outlines.shp")

# Newcastle City Council boundary
shp_Newcastle = st_read("shapefiles/Newcastle_City_Council.shp")

# Transform shapefiles ready for use
# If cutting out only the portion of a flood shapefile that is within a city boundary,
# then larger = flood shapefile, smaller = cityOutline
# If cutting out only the portion of city data that is within a flood extent shapefile,
# then larger = cityData, smaller = flood shapefile
shp_larger <- shp_recorded %>% st_transform(crs = 4326) ## Specify
shp_smaller <- shp_Newcastle %>% st_transform(crs = 4326) ## Specify

# Filtering flood extent shapefile to only parts that are within a city boundary
dlCookie <- # name the object you're making
  cookieCutter( # apply the cookieCutter function
    sf = shp_larger, # specify the larger shapefile with all the data
    sfToCut = shp_smaller, # specify the smaller shapefile you want to retrieve data for
    quiet = FALSE) # don't suppress warnings

# Ensure transformation to correct CRS projection
shp_dlCookie <- 
  dlCookie %>% 
  dplyr::mutate(type = st_geometry_type(geometry)) %>%
  st_transform(crs = 4326)

# Ensure you have specified filepath name and file/location name
path <- "shapefiles"
name <- "Newcastle_flood-recorded"

# Export the final output
# exportOSMtidy(dg = shp_dlCookie, directory = path, name = name, ext = ".shp") # Gives output as shapefile


# HISTORICAL FLOODS WITHIN A CITY COUNCIL BOUNDARY ------------------------

shp_hist = st_read("shapefiles/Historic_Flood_Map.shp")

# Newcastle City Council boundary
shp_Newcastle = st_read("shapefiles/Newcastle_City_Council.shp")

# Transform shapefiles ready for use
# If cutting out only the portion of a flood shapefile that is within a city boundary,
# then larger = flood shapefile, smaller = cityOutline
# If cutting out only the portion of city data that is within a flood extent shapefile,
# then larger = cityData, smaller = flood shapefile
shp_larger <- shp_hist %>% st_transform(crs = 4326) ## Specify
shp_smaller <- shp_Newcastle %>% st_transform(crs = 4326) ## Specify

# Filtering flood extent shapefile to only parts that are within a city boundary
dlCookie <- # name the object you're making
  cookieCutter( # apply the cookieCutter function
    sf = shp_larger, # specify the larger shapefile with all the data
    sfToCut = shp_smaller, # specify the smaller shapefile you want to retrieve data for
    quiet = FALSE) # don't suppress warnings

# Ensure transformation to correct CRS projection
shp_dlCookie <- 
  dlCookie %>% 
  dplyr::mutate(type = st_geometry_type(geometry)) %>%
  st_transform(crs = 4326)

# Ensure you have specified filepath name and file/location name
path <- "shapefiles"
name <- "Newcastle_flood-historical"

# Export the final output
# exportOSMtidy(dg = shp_dlCookie, directory = path, name = name, ext = ".shp") # Gives output as shapefile


# FINDING INFO FROM RECORDED FLOODS WITHIN HISTORICAL FLOODS --------------

# Observations of recorded floods from the (unvalidated) Recorded Flood Outlines within the boundary
shp_flood_recorded = 
  st_read("shapefiles/Newcastle_flood-recorded_sf_POLYGON_20230815-135910.shp")

# Observations of historical floods from the (final validated) Historic Flood Map within the boundary
shp_flood_hist = 
  st_read("shapefiles/Newcastle_flood-historical_sf_POLYGON_20230815-102005.shp")

# Function which creates a list element for each row of the historical flood shapefile
# outlinining which recorded flood outline observations were within the historical flood extent
# which allows you to access additional information regarding date, cause, accepted/rejected for historical floods etc.
floodCheck <- function(recorded, hist) {
  
  shp_recorded_plot = 
    shp_flood_recorded %>% 
    rownames_to_column() %>% 
    st_transform(crs = 4326)
  
  shp_hist_plot = 
    shp_flood_hist %>% 
    rownames_to_column() %>% 
    st_transform(crs = 4326)
  
  hist_row = shp_hist_plot %>% pull(rowname)
  
  output = list()
  
  for(x in hist_row) {
    
    shp_hist_plot_row = shp_hist_plot %>% filter(rowname == x)
    
    output[[paste0("hist_row_", x)]] = # name the object you're making
      cookieCutter( # apply the cookieCutter function
        sf = shp_recorded_plot, # specify the larger shapefile with all the data
        sfToCut = shp_hist_plot_row, # specify the smaller shapefile you want to retrieve data for
        quiet = FALSE) # don't suppress warnings
    
  }
  
  return(output)
  
}

floodCheck_Newcastle = floodCheck(recorded = shp_recorded_plot, hist = shp_hist_plot)

# saveRDS(floodCheck_Newcastle, "flood-recorded-historical_Newcastle_overlap.RDS")
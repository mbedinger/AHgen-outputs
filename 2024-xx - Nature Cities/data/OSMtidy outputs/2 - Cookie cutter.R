# =========================================================================
# Cookie cutter.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com), Dr Melissa Bedinger (dr.m.bedinger@gmail.com), Kerri McClymont (km39@hw.ac.uk)
# Created: 2020-10-10
#
# Last revised by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Last revised: 2022-12-02
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

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}

# Updated cookieCutter function
cookieCutter <- 
  function(sf, 
           sfToCut, 
           quiet = TRUE) {
    
    # https://github.com/r-spatial/sf/issues/1649
    # https://r-spatial.github.io/sf/articles/sf7.html
    # sf::sf_use_s2()
    sf::sf_use_s2(FALSE) # Until review for updated sf/s2 S^2 spherical space, revert to R^2 flat space
    
    internalFunction <- function(sf, sfToCut) {
      
      sfToCut <- sfToCut %>% dplyr::summarise()
      index <- sf %>% st_intersects(sfToCut) %>% lengths > 0 # Impacted by hazard
      cutout <- sf[index,] %>% st_intersection(sfToCut) # Subset (cut)
      return(cutout)
      
    }
    
    if(quiet == FALSE) { output <- internalFunction(sf, sfToCut) }
    
    if(quiet == TRUE) { output <-
      suppressWarnings(
        suppressMessages(
          internalFunction(
            sf, sfToCut
          )
        )
      )
    }
    
    return(output)
    
  }

# Updated exportOSMtidy function
exportOSMtidy <- function(dg, 
                          path,
                          name,
                          sf = TRUE,
                          ext) {
  
  require(sf)
  
  if(sf == FALSE) {
    
    dg <- 
      dg %>% 
      as.data.frame() %>% 
      dplyr::mutate(type = sf::st_geometry_type(geometry), # Add column for geometry type
             geometry = sf::st_as_text(geometry)) # Reformat geometry as character class for speedier wrangling
    
    prefix <- paste0(path, name, "_7_postProcessing_non-sf")
    
  } else if(sf == TRUE) {
    
    dg <- dg %>% as.data.frame()
    
    prefix <- paste0(path, name, "_7_postProcessing_sf")
    
  }
  
  if(ext == ".RDS") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".RDS")
    
    dg %>% dplyr::select(-type) %>% saveRDS(filename)
    
  }
  
  if(ext == ".csv") {
    
    filename <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    
    dg %>% dplyr::select(-type) %>% write_csv(filename)
    
  }
  
  if(ext == ".shp") {
    
    vec = c("POINT", "LINESTRING", "POLYGON")
    
    filename <- 
      
      lapply(vec, function(x) {
        
        filename <- paste0(prefix, "_", x, "_",
                           format(Sys.time(), "%Y%m%d-%H%M%S"), ".shp")
        
        dg %>% 
          dplyr::filter(stringr::str_detect(type, x)) %>% 
          dplyr::select(desc, geometry) %>% 
          st_write(filename, quiet = TRUE)
        
        return(filename)
        
      })
    
  }
  
  filename <- filename %>% unlist() 
  
  message(paste0("Files saved as: "))
  
  message(paste0("\n\t", filename))
  
}


# 1. READ IN DATA ---------------------------------------------------------

# Read in the OSMtidy output shapefile elements for specified city
shp_line <- st_read("Manchester/FINAL_Manchester_7_postProcessing_sf_LINESTRING_20210827-112229.shp")
shp_point <- st_read("Manchester/FINAL_Manchester_7_postProcessing_sf_POINT_20210827-112223.shp") 
shp_poly <- st_read("Manchester/FINAL_Manchester_7_postProcessing_sf_POLYGON_20210827-112230.shp")
# Combine OSMtidy shapefile elements
shp_cityData <- bind_rows(shp_line, shp_point, shp_poly)

# Read in the flood shapefile for specified city
shp_flood <- st_read("shapefiles/Manchester_floodRiver-200_20221202.shp")


# 2. CUT SMALLER DATA OUT OF LARGER DATASET -------------------------------

# Transform shapefiles ready for use
# If cutting out only the portion of a flood shapefile that is within a city boundary,
# then larger = flood shapefile, smaller = cityOutline
# If cutting out only the portion of city data that is within a flood extent shapefile,
# then larger = cityData, smaller = flood shapefile
shp_larger <- shp_cityData %>% st_transform(crs = 4326) ## Specify
shp_smaller <- shp_flood %>% st_transform(crs = 4326) ## Specify

# Filtering flood extent shapefile to only parts that are within a city boundary
dlCookie <- # name the object you're making
  cookieCutter( # apply the cookieCutter function
    sf = shp_larger, # specify the larger shapefile with all the data
    sfToCut = shp_smaller, # specify the smaller shapefile you want to retrieve data for
    quiet = FALSE) # don't suppress warnings
# Started running at 19.25

# Inspect the retrieved data
dlCookie
class(dlCookie)
unique(dlCookie$desc)

# Ensure transformation to correct CRS projection
shp_dlCookie <- 
  dlCookie %>% 
  dplyr::mutate(type = st_geometry_type(geometry)) %>%
  st_transform(crs = 4326)


# 3. SAVE OUTPUTS ---------------------------------------------------------

# Ensure you have specified filepath name and file/location name
path <- "Manchester/"
name <- "Manchester_floodRiver-100"

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(shp_dlCookie, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(shp_dlCookie, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(shp_dlCookie, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(shp_dlCookie, path, name, ext = ".shp") # Gives output as shapefile

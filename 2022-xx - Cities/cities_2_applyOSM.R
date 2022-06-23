# =========================================================================
# cities_2_applyOSM.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com), Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Kerri McClymont (km39@hw.ac.uk)
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
pacman::p_load(
  tidyverse, openxlsx, readxl, sf, data.table, stats, igraph, tnet, janitor, installr)


# 0.2 EXTRA FUNCTIONS -----------------------------------------------------

# Load local AHgen functions; eventually will be part of AHgen
source("functions/functions.R")

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 1. READ IN DATA ---------------------------------------------------------

# Read in USAH_template_baseline
USAH_template_baseline = read_rds(
  "USAH_3.0_template_baseline_20210910.RDS") ## Specify filename for USAH_template_baseline (master copy with all vertex and edge info)

# Read in OSMtidy output for location (if RDS file)
geoData = readRDS(
  "Manchester/FINAL_Manchester_7_postProcessing_non-sf_20210827.RDS") ## Specify filename for OSMtidy output

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "OSM-AH-key_3.0_20210906.csv") ## Specify filename for OSMtidy-AHgen key


# 2. DETECT, CHECK, & REVISE PHYSICAL OBJECTS TO BE EXCLUDED --------------

# Find which desc terms were detected in the OSM data
# If the input geoData is of simple feature class, this can take a while 
# due to reformatting geometry to character class which then allows us to filter
desc_detection = detect_desc(geoData = geoData, key = key)

# Inspect physical objects that were not detected in Open Street Map data 
# but are set by the key to always be included
checkAlwaysInclude = desc_detection$notDetected_alwaysInclude

# Inspect physical objects that were not detected in Open Street Map data
# and will be excluded
checkExclude = desc_detection$notDetected_exclude
# Google each physical object type + location name 
# to ensure this is not just missing in the Open Street Map data
checkExclude_objects = checkExclude %>% select(physicalObject) %>% unique() 

####

# Create a vector of physical object names that should be included
# but are currently set to be excluded for this location
## Specify for each location

# Bristol
manualInclude_vec_Brist <- 
  c("Animal shelters", # Multiple
    "Botanical gardens", # University of Bristol Botanic Garden
    "Broadband provider offices", # Virgin Media
    "Diplomatic buildings", # Indian Visa and Consular Centre
    "Electricity provider offices", # Multiple e.g. Bristol Energy
    "Ferry terminals", # Bristol Cruise Terminal
    "Fishing clubs", # Bathampton Angline, Lido, etc.
    "Gas provider offices", # Multiple e.g. Bristol Energy
    "Internet cafes", # At the Well etc.
    "Manufacturing (IT hardware)", # Cubik Innovation performs in-house manufacturing
    "Playgrounds (indoor)", # Jungle Mania etc.
    "Sexual health clinics", # Unity Sexual Health @ Central clinic
    "Springs and wells", # St. Edith's Well
    "Vehicle impounds", # Western Distribution Centre: https://www.bristol.gov.uk/parking/if-your-vehicle-has-been-towed-away
    "Water provider offices") # e.g. Bristol Water

# Edinburgh
manualInclude_vec_Ed <- 
 c("Aqueducts and viaducts", # Aqueducts in Slatford/Chesser
   "Camping grounds", # Mortonhall
   "Internet cafes", # Multiple e.g. Here Internet Cafe
   "Prisons and jails", # HMP Edinburgh in Stenhouse/Longstone
   "Tram or ferry stops", # Multiple tram stops
   "Vehicle impounds", # Edinburgh Car Pound in Leith
   "Waste collection service offices") # Multiple

# Glasgow
manualInclude_vec_Gla <- 
  c("Blood banks", # Permanent blood donation centre at Nelson Mandela Place
    "Day centres", # Multiple e.g. Muslim Day Care, Newlands Day Centre
    "Dining and catering services", # Multiple e.g. Millar Catering
    "Environmental consulting and quantity surveying firms", # Multiple e.g. Cornerstone Quantity Surveyors
    "Ferry terminals", # Ferry terminal near Glasgow Science Centre"
    "Fishing clubs",# Fishing for Leave FFL
    "Gas provider offices", # Combined with Scottish Power, also Scot Gas Group etc.
    "Harbours", # King George V Dock
    "Internet cafes", # Multiple e.g. Yeeha Internet Hub
    "Manufacturing (healthcare)", #Metix Limited is a medical equipment manufacturer
    "Piers and boardwalks", # Govan Pier
    "Removal services", # Multiple e.g. RG Removals Glasgow
    "Sexual health clinics", # Private Sexual Health Clinic Glasgow
    "Vehicle impounds", # Multiple e.g. at Polmadie
    "Waste collection service offices", # Multiple e.g. AnyJunk Glasgow
    "Water tanks and towers") # Cranhill Water Tower

# London Central
manualInclude_vec_LonCen <-
  c("Assisted living (social)", # Multiple rehab accommodation facilities e.g. PCP Rehab London
    "Botanical gardens", # Chelsea Physic Garden
    "Gas provider offices", # Multiple e.g. British Gas
    "Manufacturing (healthcare)", # Multiple e.g. Enteric Healthcare Technology Co-operative
    "Manufacturing (IT hardware)", # Multiple e.g. BOSH Group
    "Sexual health clinics", # Multiple e.g. Burrell St Sexual Health Clinic
    "Tram or ferry stops", # Multiple ferry stops along Thames
    "Waste collection service offices", # Multiple e.g. Cory Environmental Ltd.
    "Water provider offices", # Multiple e.g. Thames Water Heathwall
    "Water pumping stations") # Multiple e.g. Heathwall Pumping Station

# Manchester
manualInclude_vec_Man <- 
  c("Animal shelters", # Multiple
    "Citizens Advice Bureaus", #  in city centre at Albert House
    "Dining and catering services", # Multiple e.g. Manchester Flying Chef
    "Driving schools", # Multiple e.g. L Team Driving School
    "Electricity provider offices", # Multiple e.g. ESB Energy
    "Gas provider offices", # Multiple e.g. Gazprom Energy
    "Internet cafes", # Multiple e.g. Princess Internet Cafe
    "Manufacturing (consumer discretionary)", # E.g. Samson Hosiery
    "Manufacturing (energy)", # E.g. Mantec Engineering Ltd.
    "Manufacturing (healthcare)", # E.g. Mantec Engineering Ltd.
    "Manufacturing (materials)", # Multiple e.g. Crane Materials
    "Observatories", # Godlee Observatory
    "Playgrounds (indoor)", # E.g. Wacky Warehouse
    "Political offices", # E.g. North West Liberal Democrats
    "Publishing offices", # Multiple e.g. Phoenix Media and Publishing
    "Registrar offices", # Registry office in city centre
    "Removal services", # Multiple e.g. Removal Services Manchester
    "Security services", # Multiple e.g. Angleside Security Guarding
    "Sexual health clinics", # E.g. The Northern in Cheetham
    "Stables", # E.g. Wythenshawe Park Riding Stables Ltd.
    "Union offices", # Multiple e.g. UNISON in city centre
    "Vehicle impounds", # Manchester Vehicle Pound
    "Waste collection service offices", # Multiple e.g. Skip King Skip Hire
    "Zoos") # Multiple e.g. Wythenshawe Park

####

# Create a dataframe of rows to be removed from desc_detection$notDetected_exclude
# and added to desc_detection$notDetected_alwaysInclude
manualInclude =
  desc_detection$notDetected_exclude %>%
  filter(physicalObject %in% manualInclude_vec_Man) %>% ## Specify which manualInclude_vec to use
  mutate(includedBy = "manualCheck")

# Remove from desc_detection$notDetected_exclude
desc_detection$notDetected_exclude =
  desc_detection$notDetected_exclude %>%
  filter(!physicalObject %in% manualInclude_vec_Man) ## Specify which manualInclude_vec to use
desc_detection$notDetected_exclude

# Add to desc_detection$notDetected_alwaysInclude
desc_detection$notDetected_alwaysInclude = 
  desc_detection$notDetected_alwaysInclude %>%
  mutate(includedBy = "automatedKey") %>%
  rbind(manualInclude)
desc_detection$notDetected_alwaysInclude

# Inspect to ensure this has worked
checkAlwaysInclude = desc_detection$notDetected_alwaysInclude
checkExclude = desc_detection$notDetected_exclude


# 3. APPLY OSMTIDY DATA ---------------------------------------------------

# Apply OSMtidy data to USAH
USAH_location_OSM = 
  apply_geoData(
    geoData = geoData, 
    desc_detection = desc_detection, 
    vInfo_template = USAH_template_baseline$vIncluded, ## Specify vInfo from scenario you want to make adjustments to (usually template)
    edgelist_template = USAH_template_baseline$edgelist, ## Specify edgelist from scenario you want to make adjustments to (usually template)
    proxyWeight = 0) ## Specify proxyWeight

# Inspect elements
USAH_location_OSM$desc_detection
USAH_location_OSM$vIncluded
USAH_location_OSM$vExcluded
USAH_location_OSM$adjMat
USAH_location_OSM$edgelist
USAH_location_OSM$igraph
USAH_location_OSM$results
USAH_location_OSM$summary


# 4. SAVE OUTPUTS ---------------------------------------------------------

# Write output to RDS file
USAH_location_OSM %>% saveRDS(filenameTimestamp(
    prefix = "Manchester/USAH_3.0_Manchester_baseline", ## Specify filepath and filename
    extension = ".RDS"))

# Write results to CSV file
 USAH_location_OSM$results %>% write.csv(filenameTimestamp(
  prefix = "Manchester/USAH_3.0_Manchester_baseline_results", ## Specify filepath and filename
   extension = ".csv"))
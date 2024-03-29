# David Morrison
# aesthetics.R
# Created: 2021-09-30
# Last updated: 2021-09-30

# 1. PREPARE ENVIRONMENT --------------------------------------------------
rm(list = ls()); cat("\014") 

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

pacman::p_load(tidyverse, ggplot2)


# GGPLOT THEME AND FUNCTIONS ----------------------------------------------
## ggplot theme
source("myTheme.R")

## ggplot functions for dotplots, time series and rank
source("plotFunctions.R")


# COLOURS -----------------------------------------------------------------
## Greys for non-data points and features
myGreys <- 
  c("#1b2631", "#212f3c", "#283747",
    "#2e4053", "#34495e", "#5d6d7e",
    "#85929e", "#aeb6bf", "#d6dbdf", "#ebedef")

## Plot colour palettes
## Use the gf_cols and vpm_cols objects in scale_colour_manual()
gf_colour_data <- read.csv("gf_colours.csv")
vpm_colour_data <- read.csv("vpm_colours.csv")

gf_cols <- as.character(gf_colour_data$colour) 
names(gf_cols) <- as.character(gf_colour_data$vName)

vpm_cols <- as.character(vpm_colour_data$colour)
names(vpm_cols) <- as.character(vpm_colour_data$vName)


# ABBREVIATIONS -----------------------------------------------------------
## For use with abbreviate() nested in geom_text() or geom_text_repel()
## Be careful though! Sometimes the abbreviations and colours get mixed up so 
## triple-check plots. On occasion I've resorted to manually creating abbreviations
## in Inkscape after creating plot. 
vpm_abbr_hw <- c("Diverse livellihoods and employment" = "DLE", "Effective safeguards to human health and life" = "ESHHL",
                 "Minimal vulnerability" = "MV")

vpm_abbr_es <- c("Collective identity and community support" = "CICS", "Comprehensive security and rule of law" = "CSRL", 
                 "Socio-economic equality and equity" = "SeEE", "Sustainable economy" = "SE")

vpm_abbr_ie <- c("Effective provision of critical services" = "EPCS", "Environmental sustainability" = "EnvSu", "Reduced exposure and fragility" = "REF",
                 "Reliable communications and mobility" = "RCM")

vpm_abbr_ls <- c("Effective leadership and management" = "ELM", "Empowered stakeholders" = "EmSta", 
                 "Integrated development and planning" = "IDP")

gf_abbr_hw <- c("Animal welfare" = "AW", "Biological hazard regulation" = "BHR", "Clothing provision" = "CP",
                "Emergency services" = "EmSe", "Food provision" = "FP", "Housing provision" = "HP", "Public health" = "PH")

gf_abbr_es <- c("Ceremonies and services for major life events" = "CSMLE", "Employment provision" = "EP", "Financial services" = "FS",
                "Foster social cohesion" = "FSC", "Goods and services provision" = "GSP", "Historical and cultural values contribution" = "HCV",
                "Observance of religion" = "OR", "Physical security" = "PS", "Recreational activities" = "RA", "Social interaction" = "SI",
                "Tourism" = "Tou")

gf_abbr_ie <- c("Clean air" = "CA", "Clean water" = "CW", "Distribution of goods (logistics)" = "DoG", "Energy supply" = "EnSu",
                "Environmental and geohazard regulation" = "EGR", "Environmental conservation" = "EC", "Hydrometeorological hazard regulation" = "HHR",
                "Road conditions and saftey" = "RCS", "Sanitation provision" = "SP", "Technological hazard regulation" = "THR",
                "Travel (people, not goods)" = "Tra", "Waste management" = "WM")

gf_abbr_ls <- c("Communications systems" = "CS", "Community activites and engagement" = "CAE", "Governance" = "G",
                "Law and Order" = "LaO", "Learning and education" = "LaE", "Planning activities" = "PA", "Societal hazard regulation" = "SHR")

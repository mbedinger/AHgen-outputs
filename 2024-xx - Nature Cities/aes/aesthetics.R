# =========================================================================
# aesthetics.R

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com), David Morrison (dh48@hw.ac.uk)
# Created: 2021-09-30

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-06-20
# =========================================================================

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(
  tidyverse, ggplot2, purrr, stringr, pixmap, tibble, treemap, rsvg, png, 
  jpeg, tiff, bmp, dplyr, plotly, magrittr, plotwidgets, imager, colorfindr, 
  RColorBrewer, colorRampPalette)

# GGPLOT THEME AND FUNCTIONS ----------------------------------------------
## ggplot theme
source("myTheme.R")

## ggplot functions for dotplots, time series and rank
source("plotFunctions.R")



# SELECT COLORS - AUTOMATED PALETTE FOR LOWER LEVELS ----------------------
library(RColorBrewer)
display.brewer.pal("YlBlGn", n = 8)
brewer.pal("Dark2", n = 8)
rainbow(37)
fun_color_range <- 
  colorRampPalette(c(
    "#FF0000", "#FF5300", "#66A61E", "#0098FF", "#0045FF","#8A00FF", "#FF00F8","#FF0029")) 
my_colors <- fun_color_range(37)
my_colors

[1] "#FF0000" "#FF1200" "#FF2400" "#FF3700" "#FF4900" "#EE5C03" "#CC6E09" "#AA8110"
[9] "#889317" "#66A61E" "#4FA250" "#389F81" "#229CB3" "#0B99E5" "#008EFF" "#007CFF"
[17] "#0069FF" "#0057FF" "#0045FF" "#0335FF" "#0626FF" "#0917FF" "#0C07FF" "#1B00FF"
[25] "#3700FF" "#5200FF" "#6E00FF" "#8A00FF" "#A300FD" "#BD00FB" "#D700FA" "#F100F8"
[33] "#FF00E1" "#FF00B3" "#FF0085" "#FF0057" "#FF0029"


# SELECT COLORS - OUTCOMES & TASKS ----------------------------------------

## LATEST 100RC COLORS

### ECONOMY AND SOCIETY
fun_color_range <- colorRampPalette(c("#FFFFFF", "#F80043", "#F12694", "#81021F", "#000000")) 
my_colors <- fun_color_range(60)
my_colors
# Had to customise these for enough variation
# Tasks
"#FB85A5"
"#FA638C"
"#F81E59"
"#F41570"
"#F1228B" 
"#DE1F80" 
"#C71868" 
"#B01150" 
"#980043"
"#920730"
"#81021F"
# Outcomes
"#FA638C"
"#F41570"
"#DE1F80" 
"#B01150"

### HEALTH AND WELLBEING
fun_color_range <- colorRampPalette(c("#FD8971", "#FC4E2A", "#7F2704")) 
my_colors <- fun_color_range(10)
my_colors
# Tasks
"#FC6E51"
"#E74723"
"#D2411D"
"#BD3A17"
"#A83410"
"#932D0A"
"#7F2704"
# Outcomes
"#D2411D"
"#A83410"
"#7F2704"

### INFRASTRUCTURE AND ECOSYSTEMS
fun_color_range <- colorRampPalette(c("#FFFFFF", "#2171B5", "#810F7C")) 
my_colors <- fun_color_range(30)
my_colors
# Added #FFFFFF to get a couple of lighter blues
# Tasks
"#72A5D0"
"#4487C0"
"#2968AF"
"#3B56A5"
"#434DA0"
"#4C449B"
"#553B95"
"#5E3290"
"#66298B"
"#6F2086"
"#781781"
"#810F7C"
# Outcomes
"#2968AF"
"#434DA0"
"#66298B"
"#810F7C"


### LEADERSHIP AND STRATEGY
display.brewer.pal("Greys", n = 9)
brewer.pal("Greys", n = 9)
fun_color_range <- colorRampPalette(c("#90EE91", "#33B864", "#335E3B")) 
my_colors <- fun_color_range(7)
my_colors
# Had to customise these for enough variation
# Tasks
"#33B864"
"#459D62"
"#238B45" 
"#23793F"
"#23693A"
"#245834"
"#24472F"
# Outcomes
"#238B45"
"#23693A"
"#24472F"


## NOW MOVED TO VINFO-FULL XLSX


# -------------------------------------------------------------------------

# ARCHIVED COLORS
# Load 100RC category colours within-level for VPM & GF
## Plot colour palettes
## Use the gf_cols and vpm_cols objects in scale_colour_manual()
gf_colour_data <- read.csv("aes/gf_colours.csv")
vpm_colour_data <- read.csv("aes/vpm_colours.csv")

gf_cols <- as.character(gf_colour_data$colour) 
names(gf_cols) <- as.character(gf_colour_data$vName)

vpm_cols <- as.character(vpm_colour_data$colour)
names(vpm_cols) <- as.character(vpm_colour_data$vName)


# NEW DEFAULT COLORS

### ECONOMY AND SOCIETY

#
fun_color_range <- colorRampPalette(c("#FFFFFF", "#F80043", "#F12694", "#81021F", "#000000")) 
my_colors <- fun_color_range(60)
my_colors

# Had to customise these for enough variation

"#FB85A5"
"#FA638C"
"#F81E59"
"#F41570"
"#F1228B" 
"#DE1F80" 
"#C71868" 
"#B01150" 
"#980043"
"#920730"
"#81021F"

"#FA638C"
"#F41570"
"#DE1F80" 
"#B01150"


### HEALTH AND WELLBEING
# OPTION 1
fun_color_range <- colorRampPalette(c("#FC4E2A", "#7F2704")) 
my_colors <- fun_color_range(7)
my_colors
"#FC4E2A"
"#E74723"
"#D2411D"
"#BD3A17"
"#A83410"
"#932D0A"
"#7F2704"

"#D2411D"
"#A83410"
"#7F2704"


# OPTION 2
display.brewer.pal("Oranges", n = 9)
brewer.pal("Oranges", n = 9)

fun_color_range <- colorRampPalette(c("#F16913", "#7F2704")) 
my_colors <- fun_color_range(7)
my_colors
"#F16913"
"#DE5D10"
"#CB530E"
"#B8480B"
"#A53D09"
"#923206"
"#7F2704"

"#CB530E"
"#A53D09"
"#7F2704"


# OPTION 3 - NAH
display.brewer.pal("YlOrRd", n = 9)
brewer.pal("YlOrRd", n = 9)

fun_color_range <- colorRampPalette(c("#FC4E2A", "#800026")) 
my_colors <- fun_color_range(7)
my_colors

"#FC4E2A" 
"#E74129" 
"#D23428" 
"#BE2728" 
"#A91A27" 
"#940D26" 
"#800026"

"#E74129"
"#BE2728" 
"#940D26" # think this could be too red/pink


# OPTION 4 - NAH
display.brewer.pal("OrRd", n = 9)
brewer.pal("OrRd", n = 9)

fun_color_range <- colorRampPalette(c("#EF6548", "#7F0000")) 
my_colors <- fun_color_range(7)
my_colors
"#EF6548"
"#DC543C"
"#C94330"
"#B73224"
"#A42118"
"#91100C"
"#7F0000"


### INFRASTRUCTURE AND ECOSYSTEMS

# OPTION 1
fun_color_range <- colorRampPalette(c("#2171B5", "#810F7C")) 
my_colors <- fun_color_range(12)
my_colors

"#2171B5"
"#2968AF"
"#325FAA"
"#3B56A5"
"#434DA0"
"#4C449B"
"#553B95"
"#5E3290"
"#66298B"
"#6F2086"
"#781781"
"#810F7C"

"#2968AF"
"#434DA0"
"#66298B"
"#810F7C"


# OPTION 2 - NAH
display.brewer.pal("BuPu", n = 9)
brewer.pal("BuPu", n = 9)

fun_color_range <- colorRampPalette(c("#8C6BB1", "#4D004B")) 
my_colors <- fun_color_range(12)
my_colors

"#8C6BB1"
"#8661A7"
"#80579E"
"#7A4D95"
"#75448B"
"#6F3A82"
"#693079"
"#632670"
"#5E1D66"
"#58135D"
"#520954"
"#4D004B"


# OPTION 3 - NAH
display.brewer.pal("YlGnBu", n = 9)
brewer.pal("YlGnBu", n = 9)

fun_color_range <- colorRampPalette(c("#369AB0", "#081D58")) 
my_colors <- fun_color_range(12)
my_colors

"#369AB0"
"#318EA8"
"#2D83A0"
"#297798"
"#256C90"
"#216188"
"#1C5580"
"#184A78"
"#143F70"
"#103368"
"#0C285F"
"#081D58"


# OPTION 4 - NAH
display.brewer.pal("PuBu", n = 9)
brewer.pal("PuBu", n = 9)

fun_color_range <- colorRampPalette(c("#3690C0", "#023858")) 
my_colors <- fun_color_range(12)
my_colors

"#3690C0"
"#3188B6"
"#2C80AD"
"#2778A3"
"#23709A"
"#1E6890"
"#196087"
"#14587D"
"#105074"
"#0B486A"
"#063F61"
"#023858"


# OPTION 5 - NAH
display.brewer.pal("Blues", n = 9)
brewer.pal("Blues", n = 9)

fun_color_range <- colorRampPalette(c("#2171B5", "#08306B")) 
my_colors <- fun_color_range(12)
my_colors
"#2171B5"
"#1E6BAE"
"#1C65A7"
"#1A5FA0"
"#17599A"
"#155393"
"#134D8C"
"#114785"
"#0E417F"
"#0C3B78"
"#0A3571"
"#08306B"


### LEADERSHIP AND STRATEGY

# OPTION 1
display.brewer.pal("Greys", n = 9)
brewer.pal("Greys", n = 9)

fun_color_range <- colorRampPalette(c("#238B45", "#525252")) 
my_colors <- fun_color_range(7)
my_colors

"#238B45" 
"#23793F"
"#23693A"
"#245834"
"#24472F"
"#24362A"
"#252525"

"#238B45"
"#23693A"
"#24472F"


# OPTION 2 - NAH
display.brewer.pal("Greens", n = 9)
brewer.pal("Greens", n = 9)

fun_color_range <- colorRampPalette(c("#238B45", "#00441B")) 
my_colors <- fun_color_range(7)
my_colors

"#238B45"
"#1D7F3E"
"#177337"
"#116730"
"#0B5B29"
"#054F22"
"#00441B"

"#238B45"
"#116730"
"#00441B"


# -------------------------------------------------------------------------


# NEW DEFAULT COLORS
# Read in data (Nature Urban Sustainability paper images) to find & 
# select some colors
# colors = get_colors("archived/Outcomes example 4.png", exclude_col = "white")
# make_palette(colors, n = 40)

# Outcomes - by 100RC category

# Economy and society: 4
"#EEC4CE" #
"#E297AC" #
"#D1698E" #
"#C3406C" #


# Health and wellbeing: 3
"#FFD701" #
"#F6A719" #
"#EE7732" #

# Infrastructure and ecosystems: 4
"#87CEFA" #
"#B88EF7" #
"#6A3CDE" #
"#0137BB" # manually adjusted

# Leadership and strategy: 3
"#AEFA2F" #
"#56CC5B" #
"#026949" # manually adjusted

# Tasks - by 100RC category
# pretty much all manually adjusted

# Economy and society: 11
"#FDDAE5" #
"#FFC0CB" #
"#FDB1C2" # 
"#FBA3BA" #
"#F995B1" #
"#F883A6" #
"#F679A1" #
"#F46B98" #
"#F35C8F" #
"#EF417F" #
"#CE3377" # manually adjusted  

# Health and wellbeing: 7
"#FAE86C" # manually adjusted, Animal welfare
"#FFD701" # Biological hazard regulation
"#F9B710" # Clothing provision
"#F39620" # Emergency services
"#F19058" # Food provision
"#EE7732" # manually adjusted, Housing provision
"#DC7732" # manually adjusted, Public health

# Infrastructure and ecosystems: 12
"#87CEFA" #
"#3490C7" #
"#0177BB" #
"#2269C5" #
"#314FCC" #
"#AEACF8" #
"#CFA8F3" # manually adjusted
"#A879F6" # manually adjusted
"#946FF4" #
"#9C33F0" #
"#7437E1" #
"#4927D7" # manually adjusted

# Leadership and strategy: 7
"#DBFDA9" #
"#AEFA2F" # from outcomes
"#59EB57" #
"#56CC5B" # from outcomes
"#048949" # manually adjusted from outcomes
"#026949" # from outcomes
"#005149" # from outcomes


# SELECT COLORS - LEVELS --------------------------------------------------

## NOW MOVED TO VINFO-FULL XLSX
# Set default level colours

WRCcols_fp <- "#CCB1B9"
WRCcols_vpm <- "#607EBC"
WRCcols_gf <- "#77B7A6"
WRCcols_orp <- "#64324D"
WRCcols_po <- "#413D4C"

WRCcols <- c(WRCcols_fp, WRCcols_vpm, WRCcols_gf, WRCcols_orp, WRCcols_po)

WRCcols_top3 <- c(WRCcols_fp, WRCcols_vpm, WRCcols_gf)
WRCcols_bottom3 <- c(WRCcols_gf, WRCcols_orp, WRCcols_po)

# WRCcols <- readRDS("aes/WRCcols.RDS") # only 5 here
# extraCols <- c("#0D0A0B", "#FABE46")
# WRCcols <- c(WRCcols, extraCols)


# SELECT COLORS - GREYS ---------------------------------------------------

myGreys <- 
  c("#1b2631", "#212f3c", "#283747",
    "#2e4053", "#34495e", "#5d6d7e",
    "#85929e", "#aeb6bf", "#d6dbdf", "#ebedef")
# myGreys <- readRDS("aes/myGreys.RDS")


# SELECT COLORS, SHAPES & SIZES - FLOODS ----------------------------------

# Set default colours for baseline vs flood scenarios
# Choose distinct colours from colourblind-friendly viridis palette
show_col(viridis_pal(option = "mako")(12))

cols_baseline <- "#85929e"
cols_floodRiver_100 <- "#56B4E9"
cols_floodRiver_200 <- "#0072B2"
cols_flood <- c(cols_baseline, cols_floodRiver_100, cols_floodRiver_200)

# Set default point shapes for scatter plots
shapes_baseline <- 21
shapes_floodRiver_100 <- 19
shapes_floodRiver_200 <- 19
shapes_flood <- c(shapes_baseline, shapes_floodRiver_100, shapes_floodRiver_200)

# Set default point sizes for scatter plots
sizes_baseline <- 0.6
sizes_floodRiver_100 <- 0.7
sizes_floodRiver_200 <- 0.7
sizes_flood <- c(sizes_baseline, sizes_floodRiver_100, sizes_floodRiver_200)


# ABBREVIATIONS -----------------------------------------------------------
## NOW MOVED TO VINFO-FULL XLSX
## For use with abbreviate() nested in geom_text() or geom_text_repel()
## Be careful though! Sometimes the abbreviations and colours get mixed up so 
## triple-check plots. On occasion I've resorted to manually creating abbreviations
## in Inkscape after creating plot. 

vpm_abbr_hw <- c("Diverse livellihoods and employment" = "DLE", 
                 "Effective safeguards to human health and life" = "ESHHL",
                 "Minimal vulnerability" = "MV")

vpm_abbr_es <- c("Collective identity and community support" = "CICS", 
                 "Comprehensive security and rule of law" = "CSRL", 
                 "Socio-economic equality and equity" = "SeEE", 
                 "Sustainable economy" = "SE")

vpm_abbr_ie <- c("Effective provision of critical services" = "EPCS", 
                 "Environmental sustainability" = "EnvSu", 
                 "Reduced exposure and fragility" = "REF",
                 "Reliable communications and mobility" = "RCM")

vpm_abbr_ls <- c("Effective leadership and management" = "ELM", 
                 "Empowered stakeholders" = "EmSta", 
                 "Integrated development and planning" = "IDP")

gf_abbr_hw <- c("Animal welfare" = "AW", 
                "Biological hazard regulation" = "BHR", 
                "Clothing provision" = "CP",
                "Emergency services" = "EmSe", 
                "Food provision" = "FP", 
                "Housing provision" = "HP", 
                "Public health" = "PH")

gf_abbr_es <- c("Ceremonies and services for major life events" = "CSMLE", 
                "Employment provision" = "EP", 
                "Financial services" = "FS",
                "Foster social cohesion" = "FSC", 
                "Goods and services provision" = "GSP", 
                "Historical and cultural values contribution" = "HCV",
                "Observance of religion" = "OR", 
                "Physical security" = "PS", 
                "Recreational activities" = "RA", 
                "Social interaction" = "SI",
                "Tourism" = "Tou")

gf_abbr_ie <- c("Clean air" = "CA", 
                "Clean water" = "CW", 
                "Distribution of goods (logistics)" = "DoG", 
                "Energy supply" = "EnSu",
                "Environmental and geohazard regulation" = "EGR", 
                "Environmental conservation" = "EC", 
                "Hydrometeorological hazard regulation" = "HHR",
                "Road conditions and saftey" = "RCS", 
                "Sanitation provision" = "SP", 
                "Technological hazard regulation" = "THR",
                "Travel (people, not goods)" = "Tra", 
                "Waste management" = "WM")

gf_abbr_ls <- c("Communications systems" = "CS", 
                "Community activites and engagement" = "CAE", 
                "Governance" = "G",
                "Law and Order" = "LaO", 
                "Learning and education" = "LaE", 
                "Planning activities" = "PA", 
                "Societal hazard regulation" = "SHR")
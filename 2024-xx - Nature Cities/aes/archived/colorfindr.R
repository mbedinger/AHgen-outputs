# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(
  purrr, stringr, pixmap, tibble, treemap, rsvg, png, jpeg,
  tiff, bmp, dplyr, plotly, magrittr, plotwidgets, imager, colorfindr)

# Read in data
colors = get_colors("archived/Outcomes example 4.png", exclude_col = "white")
make_palette(colors, n = 40)


# Outcomes

"#EEC4CE" #
"#E297AC" #
"#D1698E" #
"#C3406C" #

"#FFD701" #
"#F6A719" #
"#EE7732" #

"#87CEFA" #
"#B88EF7" #
"#6A3CDE" #
"#0137BB" # manually adjusted

"#AEFA2F" #
"#56CC5B" #
"#026949" # manually adjusted


# Tasks
# pretty much all manually adjusted
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


"#FAE86C" # manually adjusted, Animal welfare
"#FFD701" # Biological hazard regulation
"#F9B710" # Clothing provision
"#F39620" # Emergency services
"#F19058" # Food provision
"#EE7732" # manually adjusted, Housing provision
"#DC7732" # manually adjusted, Public health


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


"#DBFDA9"
"#AEFA2F" # from outcomes
"#59EB57"
"#56CC5B" # from outcomes
"#048949" # manually adjusted from outcomes
"#026949" # from outcomes
"#005149" # from outcomes
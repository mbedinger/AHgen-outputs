# =========================================================================
# meansEnds_agreement.R

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Last revised: 2020-02-27

# Note: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) added clarifying
#       comments and formatting, and specified loading of {reshape2}
#       to this file on 2021-10-01.
# =========================================================================

# PREPARE ENVIRONMENT -----------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc()

# Load packages
pacman::p_load(tidyverse, reshape2)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()


# -------------------------------------------------------------------------
# DATA --------------------------------------------------------------------
# -------------------------------------------------------------------------

# Load all csv files in the working directory into the environment and view
files = list.files(recursive = TRUE, pattern = ".csv", full.names = TRUE)
files

# Trim names
vecRounds = files %>% str_sub(., start = -15, end = -9); vecRounds
vecNames = files %>% str_sub(., start = 23, end = 25) %>% str_trim; vecNames


# Read in the data
dl = lapply(1:length(files), function(x) {
  
  dt = 
    read_csv(files[[x]], col_types = cols()) %>%
    as_tibble() %>%
    mutate_all(as.character) %>%
    mutate(round = vecRounds[[x]], name = vecNames[[x]]) %>%
    select(round, name, everything())
  # Ignore warnings, just about a few character representation issues
}); dl


# -------------------------------------------------------------------------
# AGREEMENT ---------------------------------------------------------------
# -------------------------------------------------------------------------
dl2 = 
  dl %>%
  modify(. %>% 
           reshape2::melt(measure.vars = 4:ncol(.)) %>%
           as_tibble() %>%
           mutate(variable = as.character(variable))) %>%
  bind_rows() %>%
  spread(name, value) %>%
  mutate(rowSum_zero = rowSums(. == 0),
         rowSum_one = rowSums(. == 1),
         agreeLink = rowSum_one / 4 * 100,
         agreeNoLink = rowSum_zero / 4 * 100) %>%
  split(., .$round) %>%
  modify(. %>% select(-round)); dl2

dl2

columnNames = 
  tibble(listLevel = c("Round 1", "Round 2", "Round 3", "Round 4"),
         column1 = c("temp1a", "temp1b", "temp1c", "temp1d"), # These need to be filled in
         column2 = c("temp2a", "temp2b", "temp2c", "temp2d"))  # These need to be filled in
columnNames = columnNames[columnNames$listLevel %in% names(dl2),]; columnNames

dl3 = 
  lapply(1:length(dl2), function(x) { 
    
    vec = columnNames[x,2:3]
    dt = dl2[[x]] 
    vec = c(vec, names(dt)[-(1:2)])
    
    dt %>% setNames(vec)
    
  }) %>% setNames(names(dl2)); dl3

dl4 = 
  dl3 %>% 
  modify(. %>% 
           select(1:2, agreeLink) %>% 
           spread(2, agreeLink) # Warning is fine, it is just about some character representation
         ); dl4


# -------------------------------------------------------------------------
# OUTPUT ------------------------------------------------------------------
# -------------------------------------------------------------------------
pacman::p_load(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Round 1")
addWorksheet(wb, "Round 2")
addWorksheet(wb, "Round 3")
addWorksheet(wb, "Round 4")

writeData(wb, "Round 2", dl4$`Round 2`)
writeData(wb, "Round 3", dl4$`Round 3`)
writeData(wb, "Round 4", dl4$`Round 4`)

saveWorkbook(wb, "meansEnds_agreement.xlsx", TRUE)

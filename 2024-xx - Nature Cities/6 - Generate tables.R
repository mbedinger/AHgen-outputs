# =========================================================================
# 6 - Generate tables.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-09-06

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2023-07-25
# =========================================================================


# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, AHgen, OSMtidy)


# 1. READ IN DATA ---------------------------------------------------------

# Read in comparison of scenarios
allScenarios_compared =
  read_rds("data/~compared/comparison_USAH_3.0_fiveCities-baseline_fiveCities-floodRiver-100-200_20230720-174337.RDS") ## Specify filename


# 2. VERTICES -------------------------------------------------------------

tbl_vertices = 
  table_vertices(
    vSummary = allScenarios_compared$vertices, 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 3. EDGES ----------------------------------------------------------------

tbl_edges = 
  table_edges(
    eSummary = allScenarios_compared$edges, 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 4. EXCLUDED VERTICES ----------------------------------------------------

tbl_vExcluded = 
  table_vExcluded(
    vExcluded_benchmark = "NA", 
    vExcluded_input = allScenarios_compared$vExcluded, 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 5. RANK BY DEGREE(S) ----------------------------------------------------

tbl_degree_all = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)

tbl_degrees_purposes = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    levels = "Purposes", 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)

tbl_degrees_outcomes = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    levels = "Outcomes", 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)

tbl_degrees_tasks = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    levels = "Tasks", 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)

tbl_degrees_processes = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    levels = "Processes", 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)

tbl_degrees_resources = 
  table_rankDegree(
    results = allScenarios_compared$results, 
    levels = "Resources", 
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 6. RANK BY EIGENVECTOR CENTRALITY ---------------------------------------

tbl_EC = 
  table_rankEC(
    results = allScenarios_compared$results,
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 7. RANK BY STABLE BETWEENNESS CENTRALITY --------------------------------

tbl_SBC = 
  table_rankSBC(
    results = allScenarios_compared$results,
    singleScenario = FALSE, 
    compareLocations = TRUE, 
    compareScenarios = TRUE)


# 8. ALL TABLES -----------------------------------------------------------

tbl_all = 
  tables_AHgen(
    vSummary = allScenarios_compared$vertices,
    eSummary = allScenarios_compared$edges,
    vExcluded_benchmark = "NA", 
    vExcluded_input = allScenarios_compared$vExcluded, 
    results = allScenarios_compared$results,
    singleScenario = FALSE, compareLocations = TRUE, compareScenarios = TRUE)


# 9. EXPORT ---------------------------------------------------------------

# Export
exportExcel(
  inputList = tables_output_single, 
  filenameTimestamp(
    prefix = "data/cities/Glasgow/tables_3.0_Glasgow_baseline", ## Specify filepath and filename
    extension = ".xlsx"))


# NOTES -------------------------------------------------------------------

# IDEAS FOR FURTHER TABLES
# summary of adjusted link weights?
# OSMtidy "resource redundancy"
# EC % CHANGE RANK TABLE -----------------------------------------------
table_rankEC_pctChange_multi_style1 = 
  allScenarios_compared$results %>%
  filter(metric == "EC") %>%
  filter(level <= 3) %>%
  group_by(location, level) %>% ## swap location for scenario or make interactive
  mutate(rank_byLevel_pctChange = dense_rank(desc(abs(change_pct)))) %>%
  ungroup() %>%
  mutate(value_table = ## could adapt these to separate values into multiple columns if needed
           paste0(Node, 
                  " (", 
                  change_pct %>% round(2), "%, ", 
                  baseline_value_amp %>% round(5), " to ", value_amp %>% round(5), 
                  ")")) %>%
  select(location, levelName_viz, value_table, rank_byLevel_pctChange) %>% ## swap location for scenario or make interactive
  pivot_wider(names_from = location, values_from = value_table) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  arrange(levelName_viz, rank_byLevel_pctChange) %>%
  rename(Level = levelName_viz,
         `Ranked Within Level by % Change in Eigenvector Centrality` = rank_byLevel_pctChange)


table_rankEC_pctChange_multi_style2 = 
  allScenarios_compared$results %>%
  filter(location != "template") %>%
  filter(metric == "EC") %>%
  filter(level <= 3) %>%
  group_by(location, level) %>% ## swap location for scenario or make interactive
  mutate(rank_byLevel_pctChange = 
           dense_rank(desc(change_pct %>% round(2) %>% abs))) %>%
  ungroup() %>%
  mutate(pct_group = 
           case_when(
             (change_pct %>% abs) > 100 ~ "1 - Greater Than 100% Change",
             (change_pct %>% abs) <= 100 & (change_pct %>% abs) > 75 ~ 
               "2 - Between 75-100% Change",
             (change_pct %>% abs) <= 75 & (change_pct %>% abs) > 50 ~
               "3 - Between 50-75% Change",
             (change_pct %>% abs) <= 50 & (change_pct %>% abs) > 25 ~
               "4 - Between 25-50% Change",
             (change_pct %>% abs) <= 25 & (change_pct %>% abs) > 0 ~
               "5 - Between 0-25% Change",
             TRUE ~ "6 - No Change"),
         value_table = 
           paste0(Node, 
                  " (", 
                  change_pct %>% round(2), "%, ", 
                  baseline_value_amp %>% round(5), " to ", value_amp %>% round(5), 
                  ")")) %>%
  select(location, levelName_viz, value_table, rank_byLevel_pctChange, pct_group) %>% ## swap location for scenario or make interactive
  pivot_wider(names_from = location, values_from = value_table) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  replace(.=="NULL", NA) %>%
  arrange(levelName_viz, rank_byLevel_pctChange) %>%
  rename(Level = levelName_viz,
         `Range of % Change` = pct_group,
         `Ranked Within Level by % Change in Eigenvector Centrality` = 
           rank_byLevel_pctChange)


table_rankEC_pctChange_multi_style3 = 
  allScenarios_compared$results %>%
  filter(location != "template") %>%
  filter(metric == "EC") %>%
  filter(level <= 3) %>%
  group_by(location, level) %>% ## swap location for scenario or make interactive
  mutate(quantile = ntile((change_pct %>% round(2) %>% abs), 5)) %>%
  ungroup() %>%
  mutate(quantile_group = 
           case_when(
             quantile == 5 ~ "1 - Most % Change in Location-Level",
             quantile == 4 ~ "2 - More % Change in Location-Level",
             quantile == 3 ~ "3 - Middle % Change in Location-Level",
             quantile == 2 ~ "4 - Lesser % Change in Location-Level",
             quantile == 1 ~ "5 - Least % Change in Location-Level",
             TRUE ~ "6 - No Quantifiable Change"),
         value_table = 
           paste0(Node, " (", change_pct %>% round(2), "%)")) %>%
  select(location, levelName_viz, value_table, quantile_group) %>%
  pivot_wider(names_from = location, values_from = value_table) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
  arrange(levelName_viz, quantile_group) %>%
  rename(Level = levelName_viz,
         `Quantile Group` = quantile_group)


# SBC % CHANGE RANK TABLE ----------------------------------------------
table_rankSBC_pctChange_multi_style1 =
  allScenarios_compared$results %>%
  filter(location != "template") %>%
  filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
  filter(level >= 3) %>%
  group_by(location, level) %>% ## swap location for scenario or make interactive
  mutate(rank_byLevel_pctChange = dense_rank(desc(abs(change_pct %>% round(2))))) %>%
  ungroup() %>%
  filter(rank_byLevel_pctChange < 26) %>%
  mutate(Node = ## could adapt these to separate values into multiple columns if needed
           paste0(Node, 
                  " (", 
                  change_pct %>% round(2), "%, ", 
                  baseline_value_amp %>% round(), " to ", value_amp %>% round(), 
                  ")")) %>%
  select(location, levelName_viz, Node, rank_byLevel_pctChange) %>% ## swap location for scenario or make interactive
  pivot_wider(names_from = location, values_from = Node) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
  mutate(rank_byLevel_pctChange = rank_byLevel_pctChange %>% as.numeric) %>%
  replace(.=="NULL", NA) %>%
  arrange(levelName_viz, rank_byLevel_pctChange) %>%
  rename(Level = levelName_viz,
         `Ranked Within Level by % Change in Stable Betweenness Centrality` = 
           rank_byLevel_pctChange)


table_rankSBC_pctChange_multi_style2 =
  allScenarios_compared$results %>%
  filter(location != "template") %>%
  filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
  filter(level >= 3) %>%
  group_by(location, level) %>% ## swap location for scenario or make interactive
  mutate(rank_byLevel_pctChange = dense_rank(desc(change_pct %>% round(2) %>% abs))) %>%
  ungroup() %>%
  filter(rank_byLevel_pctChange < 26) %>%
  mutate(pct_group = 
           case_when(
             (change_pct %>% abs) > 100 ~ "1 - Greater Than 100% Change",
             (change_pct %>% abs) <= 100 & (change_pct %>% abs) > 75 ~ 
               "2 - Between 75-100% Change",
             (change_pct %>% abs) <= 75 & (change_pct %>% abs) > 50 ~
               "3 - Between 50-75% Change",
             (change_pct %>% abs) <= 50 & (change_pct %>% abs) > 25 ~
               "4 - Between 25-50% Change",
             (change_pct %>% abs) <= 25 & (change_pct %>% abs) > 0 ~
               "5 - Between 0-25% Change",
             TRUE ~ "6 - No Change"),
         value_table = 
           paste0(Node, 
                  " (", 
                  change_pct %>% round(2), "%, ", 
                  baseline_value_amp %>% round(), " to ", value_amp %>% round(), 
                  ")")) %>%
  select(location, levelName_viz, value_table, rank_byLevel_pctChange, pct_group) %>% ## swap location for scenario or make interactive
  pivot_wider(names_from = location, values_from = value_table) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
  mutate(rank_byLevel_pctChange = rank_byLevel_pctChange %>% as.numeric) %>%
  replace(.=="NULL", NA) %>%
  arrange(levelName_viz, pct_group, rank_byLevel_pctChange) %>%
  rename(Level = levelName_viz,
         `Ranked Within Level by % Change in Stable Betweenness Centrality` = 
           rank_byLevel_pctChange,
         Group = pct_group)


## WORK IN PROGRESS ##
table_rankSBC_pctChange_multi_style3 = 
  allScenarios_compared$results %>%
  filter(location != "template") %>%
  filter(metric == "SBC_norm") %>% ## note: values included in table are for SBC_norm
  filter(level >= 3) %>%
  group_by(location, levelName_viz) %>% 
  mutate(rank_byLevel_pctChange = dense_rank(desc(change_pct %>% round(2) %>% abs)),
         quantile = ntile((change_pct %>% abs %>% round(2)), 5)) %>% # not sure if quantiles or percentiles or percentile_rank would be best here
  ungroup() %>%
  filter(rank_byLevel_pctChange < 26) %>%
  mutate(quantile_group = 
           case_when(
             quantile == 5 & change_pct == Inf ~ 
               "1 - Infinite % Change in Location-Level (Baseline Value = 0)",
             quantile == 5 & change_pct != Inf ~ 
               "2 - Most % Change in Location-Level",
             quantile == 4 ~ 
               "3 - More % Change in Location-Level",
             quantile == 3 ~ 
               "4 - Middle % Change in Location-Level",
             quantile == 2 ~ 
               "5 - Lesser % Change in Location-Level",
             quantile == 1 & change_pct != 0 ~ 
               "6 - Least % Change in Location-Level",
             quantile == 1 & change_pct == 0 ~ "NA - No Change"),
         value_table = 
           ifelse(quantile_group == 
                    "1 - Infinite % Change in Location-Level (Baseline Value = 0)",
                  paste0(Node, " (", change_pct %>% round(2), "%, ", 
                  baseline_value_amp %>% round(), " to ", value_amp %>% round(), ")"),
                  paste0(Node, " (", change_pct %>% round(2), "%)"))) %>%
  select(location, levelName_viz, value_table, rank_byLevel_pctChange, quantile_group) %>%
  pivot_wider(names_from = location, values_from = value_table) %>% ## if comparing scenarios instead of locations (e.g. baseline vs. flood) use "scenario" instead of "location" column
  mutate_all(~str_remove_all(as.character(.x), 'c\\(')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"\\)')) %>%
  mutate_all(~str_remove_all(as.character(.x), '\"')) %>%
  mutate(rank_byLevel_pctChange = rank_byLevel_pctChange %>% as.numeric) %>%
  replace(.=="NULL", NA) %>%
  arrange(levelName_viz, quantile_group, rank_byLevel_pctChange) %>%
  rename(Level = levelName_viz, Group = quantile_group)
## DEPRECATED FUNCTION REPLACED BY APPLY_DATA??
## FIND ANY SCRIPTS WHERE THIS IS STILL IN USE
# Function to generate weightNew for a specific COVID scenario
gen_edgesNew <- function(scenario_context, scenario_week) {
  
  edges_min <- 
    scenario_context %>% 
    group_by(layer, from, to) %>%
    summarise(min = min(!!scenario_week), .groups = "drop_last") %>%
    ungroup
  
  edges_mean <-
    scenario_context %>% 
    group_by(layer, from, to) %>%
    summarise(mean = mean(!!scenario_week), .groups = "drop_last") %>%
    ungroup
  
  edges_max <-
    scenario_context %>% 
    group_by(layer, from, to) %>%
    summarise(max = max(!!scenario_week), .groups = "drop_last") %>%
    ungroup
  
  edges_merged <- 
    edges_min %>%
    full_join(edges_mean, by = c("layer", "from", "to")) %>%
    full_join(edges_max, by = c("layer", "from", "to")) %>%
    mutate(weightNew = ifelse(min == 0, min, mean))
  
  outputList <- list()
  
  outputList$conflicts <- edges_merged %>% filter(min != mean)
  
  outputList$edgesNew <- edges_merged %>% select(-min, -mean, -max)  
  
  return(outputList)
  
}
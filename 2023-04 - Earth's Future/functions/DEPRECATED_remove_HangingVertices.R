## DEPRECATED FUNCTION REPLACED BY weight_hangingVertices??
## FIND ANY SCRIPTS WHERE THIS IS STILL IN USE
# Function to remove hanging vertices after initial adjustments have been made
remove_HangingVertices <- function(edgelist_scenario, vInfo_template) {
  
  repeat {
    
    # Find PO vertices to be excluded
    PO_scenario_down <- 
      edgelist_scenario %>% filter(layer == "l4ORP_l5PO") %>% select(to) %>% 
      unique %>% rename(vName = to)
    PO_template_down <- 
      vInfo_template %>% as.data.frame %>% filter(level == 5) %>% select(vName)
    PO_excluded_down <- 
      dplyr::setdiff(PO_template_down, PO_scenario_down) %>% pull(vName)
    
    
    # Find ORP vertices to be excluded (upward)
    ORP_scenario_up <- 
      edgelist_scenario %>% filter(layer == "l4ORP_l5PO") %>% select(from) %>% 
      unique %>% rename(vName = from)
    ORP_template_up <- 
      vInfo_template %>% as.data.frame %>% filter(level == 4) %>% select(vName)
    ORP_excluded_up <- 
      dplyr::setdiff(ORP_template_up, ORP_scenario_up) %>% pull(vName)
    
    # Find ORP vertices to be excluded (downward)
    ORP_scenario_down <- 
      edgelist_scenario %>% filter(layer == "l3GF_l4ORP") %>% select(to) %>% 
      unique %>% rename(vName = to)
    ORP_template_down <- 
      vInfo_template %>% as.data.frame %>% filter(level == 4) %>% select(vName)
    ORP_excluded_down <- 
      dplyr::setdiff(ORP_template_down, ORP_scenario_down) %>% pull(vName)
    
    
    # Find GF vertices to be excluded (upward)
    GF_scenario_up <- 
      edgelist_scenario %>% filter(layer == "l3GF_l4ORP") %>% select(from) %>% 
      unique %>% rename(vName = from)
    GF_template_up <- 
      vInfo_template %>% as.data.frame %>% filter(level == 3) %>% select(vName)
    GF_excluded_up <- 
      dplyr::setdiff(GF_template_up, GF_scenario_up) %>% pull(vName)
    
    # Find GF vertices to be excluded (downward)
    GF_scenario_down <- 
      edgelist_scenario %>% filter(layer == "l2VPM_l3GF") %>% select(to) %>% 
      unique %>% rename(vName = to)
    GF_template_down <- 
      vInfo_template %>% as.data.frame %>% filter(level == 3) %>% select(vName)
    GF_excluded_down <- 
      dplyr::setdiff(GF_template_down, GF_scenario_down) %>% pull(vName)
    
    
    # Find VPM vertices to be excluded (upward)
    VPM_scenario_up <- 
      edgelist_scenario %>% filter(layer == "l2VPM_l3GF") %>% select(from) %>% 
      unique %>% rename(vName = from)
    VPM_template_up <- 
      vInfo_template %>% as.data.frame %>% filter(level == 2) %>% select(vName)
    VPM_excluded_up <- 
      dplyr::setdiff(VPM_template_up, VPM_scenario_up) %>% pull(vName)
    
    # Find VPM vertices to be excluded (downward)
    VPM_scenario_down <- 
      edgelist_scenario %>% filter(layer == "l1FP_l2VPM") %>% select(to) %>% 
      unique %>% rename(vName = to)
    VPM_template_down <- 
      vInfo_template %>% as.data.frame %>% filter(level == 2) %>% select(vName)
    VPM_excluded_down <- 
      dplyr::setdiff(VPM_template_down, VPM_scenario_down) %>% pull(vName)
    
    
    # Find FP vertices to be excluded (upward)
    FP_scenario_up <- 
      edgelist_scenario %>% filter(layer == "l1FP_l2VPM") %>% select(from) %>% 
      unique %>% rename(vName = from)
    FP_template_up <- 
      vInfo_template %>% as.data.frame %>% filter(level == 1) %>% select(vName)
    FP_excluded_up <- 
      dplyr::setdiff(FP_template_up, FP_scenario_up) %>% pull(vName)
    
    
    # Combine excluded vectors
    vExcluded <- 
      c(PO_excluded_down, ORP_excluded_down, ORP_excluded_up, GF_excluded_down, 
        GF_excluded_up, VPM_excluded_down, VPM_excluded_up, FP_excluded_up)
    
    
    # Remove excluded vertices from edgelist_scenario
    edgelist_scenario <- 
      edgelist_scenario %>% 
      filter(!from %in% all_of(vExcluded), !to %in% all_of(vExcluded))
    
    # Count remaining ORP vertices
    nVertices_ORP_up <- 
      edgelist_scenario %>% filter(layer == "l4ORP_l5PO") %>% select(from) %>% 
      unique %>% count
    nVertices_ORP_down <- 
      edgelist_scenario %>% filter(layer == "l3GF_l4ORP") %>% select(to) %>% 
      unique %>% count
    
    # Count remaining GF vertices
    nVertices_GF_up <- 
      edgelist_scenario %>% filter(layer == "l3GF_l4ORP") %>% select(from) %>% 
      unique %>% count
    nVertices_GF_down <- 
      edgelist_scenario %>% filter(layer == "l2VPM_l3GF") %>% select(to) %>% 
      unique %>% count
    
    # Count remaining VPM vertices
    nVertices_VPM_up <- 
      edgelist_scenario %>% filter(layer == "l2VPM_l3GF") %>% select(from) %>%
      unique %>% count
    nVertices_VPM_down <- 
      edgelist_scenario %>% filter(layer == "l1FP_l2VPM") %>% select(to) %>% 
      unique %>% count
    
    # If remaining vertices counts ==, then stop cycle
    if(nVertices_ORP_up == nVertices_ORP_down && 
       nVertices_GF_up == nVertices_GF_down &&
       nVertices_VPM_up == nVertices_VPM_down) 
      break
    
  }
  
  return(edgelist_scenario)
  
}
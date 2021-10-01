function_merge_vInfo <- function(dt, vInfo) {
  
  vInfo %>% 
    select(level, levelName, vName) %>% # 2021-03-02 deleted column named "type" from selection
    right_join(dt, by = "vName")
  
}

function_igraphResultFormatting <- function(igraphresult, name) { 
  
  igraphresult %>% 
    as.data.frame %>% 
    rownames_to_column %>% 
    setNames(c("vName", name)) %>%
    as_tibble()
  
}
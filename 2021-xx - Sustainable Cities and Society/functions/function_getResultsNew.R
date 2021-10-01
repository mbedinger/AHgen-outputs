getResults <- function(igraph, vInfo) {
  
  resultsWVBC <- 
    igraph %>% 
    calcWVBC(vInfo) %>%
    mutate(rank_overall = dense_rank(desc(WVBC))) %>% 
    group_by(level) %>% 
    mutate(rank_byLevel = dense_rank(desc(WVBC))) %>%
    ungroup() %>%
    arrange(level, vName) %>%
    rename(centrality = WVBC) %>%
    mutate(type = "WVBC", .before = 1)
  
  resultsEC <- (igraph %>% eigen_centrality())$vector
  
  resultsEC <- 
    resultsEC %>% 
    as.data.frame %>%
    rownames_to_column() %>% 
    setNames(c("vName", "centrality")) %>%
    inner_join(vInfo) %>%
    mutate(rank_overall = dense_rank(desc(centrality))) %>% 
    group_by(level) %>% 
    mutate(rank_byLevel = dense_rank(desc(centrality))) %>%
    ungroup() %>%
    arrange(level, vName) %>%
    mutate(type = "EC", .before = 1)
  
  rbind(resultsEC, resultsWVBC) %>% 
    mutate(type = fct_inorder(type)) %>% 
    select(type, level, levelName, vName, centrality, contains("rank"))
  
}
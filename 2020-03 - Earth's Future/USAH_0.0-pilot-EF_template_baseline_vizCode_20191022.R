# =========================================================================
# USAH_template_baseline_0.0-pilot-EF_vizCode_20191022.R

# Condensed into a single script from formerly titled scripts:
# function_subnewtorkKey.R
# function_visualisation.R
# AH master script - with subnetworks.R
# AH papers.R
# AH visualisation master script - with subnetworks.R
# Original code: 
# Referenced in: https://doi.org/10.1029/2019EF001389

# Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Dr Melissa Bedinger (dr.m.bedinger@gmail.com)

# Created: 21-05-2019
# Last revised: 22-10-2019
# Condensed: 2021-07-23
# =========================================================================

rm(list = ls()); cat("\014")

pacman::p_load(tidyverse, igraph, tnet, visNetwork, RColorBrewer, viridis, data.table, ggsci, scales)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Create Outputs folder in directory


# function_subnetworkKey.R ------------------------------------------------

function_subnetworkKey <- function(resultsFile, subNetworkColumnName) {
  
  # Create dummy edge list for subnetwork assignment
  resultsNodeName <- resultsFile$nodeName
  edges$fromlayer = resultsFile$layer[match(edges$from, resultsNodeName)] # Edges from
  edges$tolayer = resultsFile$layer[match(edges$to, resultsNodeName)] # Edges to
  
  # Layer 5 key from input data
  key <- list(key_L5 = resultsFile %>% filter(layer == 5) %>% select('nodeName', subNetworkColumnName))
  
  # Generate water key by loop
  for(i in 5:2) {
    
    temp <- key[[paste0("key_L", i)]] %>% as.data.frame
    
    key[[paste0("key_L", i-1)]] <- 
      edges %>%
      mutate(x = temp[, subNetworkColumnName][match(edges$to, temp$'nodeName')]) %>%
      as_tibble() %>%
      setNames(c("from", "to", "weight", "fromlayer", "tolayer", subNetworkColumnName)) %>%
      filter(tolayer == i) %>%
      select(from, subNetworkColumnName) %>%
      distinct() %>%
      arrange(from, desc(!! rlang::sym(subNetworkColumnName))) %>%
      filter(!duplicated(from)) %>%
      rename(`nodeName` = from)
    
  }
  
  key
  
  key = key %>% do.call("rbind", .)
  
  resultsFile %>%
    select(-subNetworkColumnName) %>%
    left_join(key, by = "nodeName")
  
}





# function_visualisation.R ------------------------------------------------

visualisationData <- function(input = results, edges = edges) { 
  
  Nodes <- 
    input %>%
    select(nodeName, layerName, layer, subnetwork) %>%
    as_tibble() %>%
    mutate(id = 1:n(),
           id = str_pad(id, width = 2, side = "left", pad = "0"),
           id = paste0("s", id)) %>%
    
    rename(level = layer) %>% 
    mutate(value = log(6-level),
           title = nodeName,
           label = "") %>% 
    as_tibble
  
  Links <- 
    edges %>% 
    rename(fromLabel = from, toLabel = to) %>%
    merge(Nodes %>% select(fromLabel = nodeName, from = id)) %>%
    merge(Nodes %>% select(toLabel = nodeName, to = id)) %>%
    filter(from %in% Nodes$id & to %in% Nodes$id) %>%
    as_tibble() %>%
    filter(from %in% Nodes$id & to %in% Nodes$id)
  
  list(nodes = Nodes, links = Links)
  
}

visualisation <- function(nodeData = Nodes, linkData = Links,
                          defaultColours = defaultColours, plotTitle,
                          fontSize = 35, interactive = FALSE,
                          height = "500px", width = "933px") {
  
  if(interactive == FALSE) { x = nodeData$subnetwork %>% unique; x = x[!str_detect(x, "Other")] }
  if(interactive != FALSE) { x = "All nodes" }
  
  visNetwork(nodeData, linkData, 
             height = height, 
             width = width,
             background = "white", 
             main = list(text = plotTitle, 
                         style = 
                           "font-family:Arial;
                         text-align:Center;
                         font-size: 21px")) %>%
    
    visNodes(color = list(border = defaultColours$dark,
                          background = defaultColours$light,
                          highlight = list(background = defaultColours$highlight,
                                           border = defaultColours$dark),
                          hover = list(background = defaultColours$highlight,
                                       border = defaultColours$dark)),
             font = list(size = fontSize)
    ) %>%
    
    visEdges(arrows = "to;from",
             color = list(color = defaultColours$dark,
                          select = defaultColours$dark,
                          highlight = defaultColours$dark,
                          hover = defaultColours$dark)) %>%
    
    visIgraphLayout(type = "full", 
                    layout = "layout_with_sugiyama", 
                    vgap = 25,
                    layers = nodeData$level,
                    physics = FALSE) %>%
    
    
    visInteraction(dragNodes = TRUE, 
                   dragView = TRUE, 
                   zoomView = TRUE,
                   hover = TRUE, 
                   tooltipDelay = 0, 
                   multiselect = TRUE, 
                   selectConnectedEdges = TRUE, 
                   
                   tooltipStyle = 
                     "position:fixed;
                   visibility:hidden;
                   padding:5px;
                   white-space:nowrap;
                   font-family:Arial;
                   font-size:18px;
                   background-color:transparent") %>%
    
    
    visOptions(highlightNearest = list(enabled = TRUE, 
                                       degree = list(from = 4, to = 4), 
                                       algorithm = "hierarchical",
                                       hideColor = defaultColours$hideColor), 
               selectedBy = list(variable = "subnetwork", 
                                 style = "font-family:Arial",
                                 hideColor = defaultColours$hideColor,
                                 selected = x, multiple = TRUE)
    ) 
  
}

pickColours <- 
  
  function(name, alpha = 0.1) { 
    
    file <- read_csv("colours.csv", col_types = cols())
    x <- which(file$subnetwork == name)
    file <- file[x,]
    
    list(dark = file[["dark"]], light = file[["light"]], 
         highlight = file[["highlight"]], 
         hideColor = paste0("rgba(200,200,200,", alpha, ")"))
    
  }





# AH master script - with subnetworks.R -----------------------------------

# DATA - INPUT
fileName <- "MASTER_INPUT_CSV_2019_06_05.csv" # now named USAH_template_baseline_0.0-pilot-EF_20190605

dt = 
  read_csv(fileName, col_types = cols()) %>%
  rename(nodeName = `NODE NAME`, layerName = `LAYER NAME`, layer = `LAYER`)

maxCol = ncol(dt)
dt <- dt %>% select(-(309:maxCol))
names(dt)

dt %>%
  select(nodeName, layerName, layer, everything()) %>%
  filter(nodeName == "Ensure provision of clean water") %>%
  melt(measure.vars = 4:308) %>%
  as_tibble() %>%
  filter(value == 1) %>%
  as.data.table # 35 occurrences of ensure provision of clean water


# ABSTRACTION HIERARCHY
# AH from igraph
abstractionHierarchy = 
  dt %>%
  select(2:278) %>%
  as.matrix() %>%
  graph.adjacency(mode = "undirected", weighted = TRUE)
abstractionHierarchy

# Edges
edges <- abstractionHierarchy %>% get.data.frame() %>% as_tibble(); edges
write_csv(edges, "Outputs/masterEdges.csv")

# DATA - SUBNETWORK
subnetwork <- 
  read_csv("groups.csv", col_types = cols()) %>%
  filter(!str_detect(groupTidy, "paper")) %>% # Papers not to be included here
  filter(priority == 1) %>%
  mutate(subnetworkNumber = group_indices(., subnetwork)) %>%
  arrange(subnetwork)
subnetwork %>% as.data.table

dtRowNames <- 
  read.csv(fileName, header = TRUE, row.names = 1, check.names = FALSE) %>% 
  select(-(278:maxCol)) # Should that be 279, not 278?

# APPLY
# Applied to all subnetworks

# source("function_subnetworkKey.R")

num <- subnetwork$subnetworkNumber %>% unique; num
names <- subnetwork$subnetwork %>% unique

resultsAll <- 
  
  lapply(num, function(x) {
    
    # Column names for subnetwork
    vector1 <- (subnetwork %>% filter(subnetworkNumber == x))$group
    vector2 <- (subnetwork %>% filter(subnetworkNumber == x))$groupTidy
    
    # Results file
    results <-
      dt[,c("nodeName", "layerName", "layer", vector1)] %>% # Subnetwork specification
      setNames(c("nodeName", "layerName", "layer", vector2))
    results
    
    temp <- results
    
    for(i in 1:length(vector2)) {
      
      temp <- temp %>% function_subnetworkKey(., vector2[i])
      
    }
    
    results <- temp

    # BETWEENESS CENTRALITY
    # ~ UWVBC, UNWEIGHTED VERTEX BETWEENNESS CENTRALITY
    results <- 
      results %>%
      left_join(
        abstractionHierarchy %>%
          betweenness(directed = FALSE, weights = NA) %>%
          enframe %>%
          setNames(c("nodeName", "unweightedBaseline")), by = "nodeName") %>%
      mutate(unweightedBaseline = round(unweightedBaseline, digits = 4))
    
    # ~ WVBC, WEIGHTED VERTEX BETWEENNESS CENTRALITY
    results <- 
      results %>%
      left_join(
        dtRowNames %>% 
          betweenness_w(directed = NULL, alpha = 1) %>%
          as_tibble() %>%
          mutate("nodeName" = dtRowNames %>% rownames()) %>%
          rename(weightedPostHazard = betweenness) %>%
          select(-node), by = "nodeName") %>%
      mutate(weightedPostHazard = round(weightedPostHazard, digits = 4))
    
    # FINAL OUTPUT
    results %>% mutate(subnetworkNumber = x)
    
  }) %>%
  
  setNames(names)

resultsAll

# OUTPUTS
lapply(num, function(x) { 
  
  suffix = (subnetwork %>% filter(subnetworkNumber == x))$subnetwork[[1]]
  
  fileName <- paste0("Outputs/masterResults_subnetwork_", suffix, ".csv")
  
  resultsAll[[x]] %>% write.csv(fileName)
  
}) # Ignore NULL output, files will have saved

rm(names)
ls()
rm(list = ls()[-c(2,4,9)])
  
save.image("Outputs/masterEnvironment_allSubnetworks.RData")

# Then run AH papers.R





# AH papers.R -------------------------------------------------------------

# DATA - INPUT
load("Outputs/masterEnvironment_allSubnetworks.RData") # From AH master script

fileName <- "MASTER_INPUT_CSV_2019_06_05.csv" # now named USAH_template_baseline_0.0-pilot-EF_20190605

dt = 
  read_csv(fileName, col_types = cols()) %>%
  rename(nodeName = `NODE NAME`, layerName = `LAYER NAME`, layer = `LAYER`)

maxCol = ncol(dt)

dt %>%
  names

dl <- 
  dt %>%
  select(nodeName, layerName, layer, 309:maxCol) %>%
  melt(measure.vars = 4:10) %>%
  as_tibble() %>%
  
  split(., list(.$variable))
dl

dl <- lapply(1:length(dl), function(x) {
  
  dl[[x]] %>%
    spread(variable, value) %>%
    mutate(unweightedBaseline = NA, weightedPostHazard = NA,
           subnetworkNumber = length(resultsAll) + x) %>%
    as_tibble() 
  
}) %>%
  setNames(names(dl))

temp <- c(resultsAll, dl)

rm(dl, dt, fileName, maxCol)

load("Outputs/masterEnvironment_allSubnetworks.RData")

resultsAll <- temp
rm(temp)

save.image("Outputs/masterEnvironment_allSubnetworks.RData")





# AH visualisation master script - with subnetworks.R ---------------------

# DATA
# Use with the existing project to ensure correct directories
# Or, specify using:
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("Outputs/masterEnvironment_allSubnetworks.RData")

# source("function_visualisation.R")

names(resultsAll)

dt %>%
  select(nodeName, layerName, layer, everything()) %>%
  filter(nodeName == "Ensure provision of clean water") %>%
  melt(measure.vars = 4:308) %>%
  as_tibble() %>%
  filter(value == 1) %>%
  as.data.table # 35 occurrences of ensure provision of clean water

# ADD PAPERS
# Added via AH papers.R

# DATA PREP

subnetworkCultural <- 
  resultsAll[[1]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(community_leisure_recreation == 1 | cultural_historical == 1 | religion == 1, 
                             "Cultural", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkCultural

subnetworkEnergy <- 
  resultsAll[[2]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(electricity == 1 | gas == 1, "Energy", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkEnergy

subnetworkEconomic <- 
  resultsAll[[3]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(employment_economic == 1 | financial == 1 | taxes_grants_etc == 1, 
                             "Economic and finance", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkEconomic

subnetworkHealthcare <- 
  resultsAll[[4]] %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(careVulnerable == 1 | health_medical == 1, "Healthcare", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkHealthcare

subnetworkNature <- 
  resultsAll[[5]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(nature == 1, "Nature", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkNature

subnetworkTransport <- 
  resultsAll[[6]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(transport_all == 1, "Transport", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkTransport

subnetworkWaste <- 
  resultsAll[[7]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>%
  mutate(subnetwork = ifelse(waste_refuce == 1, "Waste management", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkWaste

subnetworkWater <-
  resultsAll[[8]] %>% 
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(water_all == 1, "Water", "Other")) %>%
  visualisationData(input = ., edges = edges)
subnetworkWater

# Water colours
resultsAll %>% names

subnetworkWaterBlack <- 
  resultsAll$`Water - Black` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(water_black == 1, "Water - Black", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkWaterBlue <- 
  resultsAll$`Water - Blue` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(water_blue == 1, "Water - Blue", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkWaterGreen <- 
  resultsAll$`Water - Green` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(water_green == 1, "Water - Green", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkWaterGrey <- 
  resultsAll$`Water - Grey` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(water_grey == 1, "Water - Grey", "Other")) %>%
  visualisationData(input = ., edges = edges)

# All nodes
subnetworkAll <- subnetworkCultural
subnetworkAll$nodes$subnetwork <- "Abstraction hierarchy"

# Papers
subnetworkPaperAli <- 
  resultsAll$`ALI ET AL. (2017)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`ALI ET AL. (2017)` == 1, "Ali et al. 2017", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperAmelung <- 
  resultsAll$`AMELUNG ET AL. (2017)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`AMELUNG ET AL. (2017)` == 1, "Amelung et al. 2017", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperChung <- 
  resultsAll$`CHUNG ET AL. (2017)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`CHUNG ET AL. (2017)` == 1, "Chung et al. 2017", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperKabaya <- 
  resultsAll$`KABAYA ET AL. (2019)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`KABAYA ET AL. (2019)` == 1, "Kabaya et al. 2019", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperLiu <- 
  resultsAll$`LIU ET AL. (2018)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`LIU ET AL. (2018)` == 1, "Liu et al. 2018", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperRauch <- 
  resultsAll$`RAUCH ET AL. (2017)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`RAUCH ET AL. (2017)` == 1, "Rauch et al. 2017", "Other")) %>%
  visualisationData(input = ., edges = edges)

subnetworkPaperTran <- 
  resultsAll$`TRAN (2016)` %>%
  select(-ncol(.)) %>% select(-ncol(.)) %>% select(-ncol(.)) %>% 
  mutate(subnetwork = ifelse(`TRAN (2016)` == 1, "Tran 2016", "Other")) %>%
  visualisationData(input = ., edges = edges)

names <- ls(pattern = "subnetwork"); names

subnetworks <- lapply(1:length(names), function(x) { get(names[[x]]) }) %>% setNames(names)
subnetworks %>% summary

rm(list = names)

# NODES PER LEVEL
nodesPerLevel <- 
  lapply(2:length(subnetworks), function(x) subnetworks[[x]]$nodes %>% filter(subnetwork != "Other")) %>%
  rbindlist %>%
  as_tibble %>%
  group_by() %>%
  select(-label) %>%
  
  group_by(subnetwork, level, layerName) %>%
  
  summarise(nodeCount = n()) %>%
  unite("level", c("level", "layerName"), sep = "-") %>%
  spread(level, nodeCount)
nodesPerLevel %>% write.csv("Outputs/nodesPerLevel.csv")
nodesPerLevel

tempX <- 
  tibble(subnetwork = nodesPerLevel$subnetwork,
         group = c("paper", "paper", "paper", "sub", "sub", "sub", "sub",
                   "paper", "paper", "sub", "paper", "paper", "sub", "sub", "sub",
                   rep("subSub",4))) %>%
  merge(nodesPerLevel) %>%
  select(subnetwork,group,6,7) %>%
  melt(measure.vars = 3:4) %>%
  as_tibble() %>%
  mutate(group = factor(group, levels = c("sub", "subSub", "paper"), labels = c("Subnetworks", "Water subnetworks", "Papers"))) %>%
  merge(read_csv("levelColours.csv"), all = TRUE) %>%
  as_tibble() %>%
  arrange(subnetwork)
colours = (tempX %>%
             group_by(subnetwork) %>%
             slice(1) %>%
             select(subnetwork, colour))$colour
(tempX %>%
    mutate(subnetwork = factor(subnetwork)) %>%
    ggplot(aes(x = variable, y = value, colour = subnetwork, group = subnetwork)) +
    geom_point(size = 4) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    scale_y_continuous(minor_breaks = seq(0,60,2), breaks = seq(0,60,10)) +
    facet_wrap(~group) +
    scale_colour_manual(values = colours)) %>%
  
  ggsave("Outputs/nodesPerLevel.png", ., height = 29.7, width = 21, units = "cm")

subnetworks$subnetworkAll$nodes %>%
  filter(nodeName == "Ensure provision of clean water")

check <- 
  subnetworks$subnetworkAll$links %>%
  filter(toLabel == "Ensure provision of clean water" | fromLabel == "Ensure provision of clean water")

c(check$toLabel, check$fromLabel) %>% unique

# DATA PREP - RESUME

temp <- 
  read_csv("renameNodes.csv", col_types = cols())
temp

subnetworks <- 
  
  lapply(1:length(subnetworks), function(x) { 
    
    data = subnetworks[[x]]
    data$nodes = data$nodes %>% merge(temp) %>% select(-nodeName) %>% rename(nodeName = newNodeName) %>%
      mutate(title = nodeName) %>% as_tibble()
    
    data$links = data$links %>% merge(temp %>% rename(toLabel = nodeName)) %>% select(-toLabel) %>%
      rename(toLabel = newNodeName) %>% as_tibble %>% select(-layerName)
    data$links = data$links %>% merge(temp %>% rename(fromLabel = nodeName)) %>% select(-fromLabel) %>%
      rename(fromLabel = newNodeName) %>% as_tibble %>% select(-layerName)
    
    return(data)
    
  }) %>%
  
  setNames(names(subnetworks))



subnetworks$subnetworkAll$nodes %>%
  filter(nodeName == "Clean<br>water")

(subnetworks$subnetworkAll$links %>% filter(from == "s25"))$toLabel

# VISUALISATIONS

names

titles = paste0(str_sub(names, 1, 10), ": ", str_sub(names, 11)) %>% str_to_title
titles[[1]] <- "Abstraction hierarchy"

titles[str_detect(titles, "Water")][-1] <- paste0("Subnetwork: ", c("Black", "Blue", "Green", "Grey"), " water")
titles

papers <- (nodesPerLevel %>% filter(str_detect(subnetwork, "et al") | subnetwork == "Tran 2016"))$subnetwork %>% as.vector
papers
titles[str_detect(titles, "Paper")]
##

titles[str_detect(titles, "Paper")] <- paste0("Paper subnetwork: ", papers)
titles

titles[[13]] <- "Indicators, Tran 2016"
titles[[7]] <- "Discpline-specific model, Ali et al. 2017"
titles[[12]] <- "Multi-criteria analysis, Rauch et al. 2017"
titles[[11]] <- "Nexus approach, Liu et al. 2018"

figures <- 
  
  lapply(1:length(names), function(x) { 
    
    name = names(subnetworks)[[x]]
    
    visualisation(subnetworks[[x]]$nodes, 
                  subnetworks[[x]]$links, 
                  defaultColours = pickColours(name, alpha = 0.25), 
                  plotTitle = titles[[x]])
    
  }) %>% setNames(names)
figures %>% summary

# OUTPUTS

figures$subnetworkAll
figures$subnetworkCultural
figures$subnetworkEconomic
figures$subnetworkEnergy
figures$subnetworkHealthcare
figures$subnetworkNature
figures$subnetworkTransport
figures$subnetworkWaste
figures$subnetworkWater
figures$subnetworkWaterBlack
figures$subnetworkWaterBlue
figures$subnetworkWaterGreen
figures$subnetworkWaterGrey
figures$subnetworkPaperAli
figures$subnetworkPaperAmelung
figures$subnetworkPaperChung
figures$subnetworkPaperKabaya
figures$subnetworkPaperLiu
figures$subnetworkPaperRauch
figures$subnetworkPaperTran

# HTML SAVE
setwd("Outputs"); getwd()

lapply(1:length(titles), function(x) { figures[[x]] %>% visExport %>% visSave(., file = paste0(names[[x]], ".html")) })
# Ignore NULL, files saving

setwd("../"); getwd()

# STATIC FIGURES
setwd("Outputs")

lapply(1:length(titles), function(x) { 
  
  data = subnetworks[[x]]
  data$nodes = data$nodes %>% mutate(label = title,
                                     label = str_replace(label, "<br>", "\n"))
  
  name = names(subnetworks)[[x]]
  
  visualisation(data$nodes, 
                data$links,
                defaultColours = pickColours(name, alpha = 0.25), 
                plotTitle =  titles[[x]],
                fontSize = 20) %>%
    
    #visExport() %>%
    
    visSave(., file = paste0("static_", names[[x]], ".html"))
  
})

setwd("../")

# Coloured waters and papers
lapply(c(which(str_detect(names(subnetworks), "Water")),
         which(str_detect(names(subnetworks), "Paper"))), function(x) { 
           
           data = subnetworks[[x]]
           data$nodes = data$nodes %>% mutate(label = title,
                                              label = str_replace(label, "<br>", "\n"))
           
           name = names(subnetworks)[[x]]
           
           visualisation(data$nodes, 
                         data$links,
                         defaultColours = pickColours(name, alpha = 0.25), 
                         plotTitle =  titles[[x]],
                         fontSize = 40,
                         width = paste0(933*1.25, "px"),
                         height = paste0(500*1.25, "px")) %>%
             
             visExport() %>%
             
             visSave(., file = paste0("static_", names[[x]], ".html"))
           
         })

# INTERACTIVE VERSION FOR DOI
paperIndex <- subnetworks %>% names
subnetworks[paperIndex[c(7,8,9,10,11,12,13)]] <- NULL

temp <- 
  
  lapply(2:length(subnetworks), function(x) { 
    
    data = subnetworks[[x]]$nodes
    data[[paste0("temp", x)]] = data$subnetwork
    data[,9] %>% unlist %>% as.vector
    
  })
temp

dt <- 
  subnetworks[[1]]$nodes %>% 
  mutate(subnetwork = do.call(paste, c(temp, sep = ", ")),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other, ", ""),
         subnetwork = str_replace(subnetwork, "Other", ""),
         temp = str_ends(subnetwork, ", "),
         subnetwork = ifelse(temp == TRUE, str_sub(subnetwork,end = -3), subnetwork),
         subnetwork = ifelse(subnetwork == "", "No subnetworks", subnetwork),
         subnetwork = paste0(subnetwork, ",All nodes"))
dt

# Visualisation
interactive <- 
  visualisation(dt, subnetworks[[1]]$links, 
                pickColours("subnetworkAll", alpha = 0), "Abstraction hierarchy",
                interactive = TRUE) %>%
  visExport()
interactive

setwd("Outputs"); visSave(interactive, "interactiveVersion_forDOI.html"); setwd("../")
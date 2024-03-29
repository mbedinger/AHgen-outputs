# Function for generating POT plot:
myDotplot <- function(myData, myPlotTitle) {
  ggplot(data = myData, aes(x = result, 
                            y = fct_reorder(vName, baseline_result_bc, .desc = F),
                            fill = result)) +
    
    #facet_col(facets = vars(indicators), scales = "free_y", space = "free") +
    
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "#B4BDC5", size=0.5) +
    
    geom_point(stat='identity', size = 9, shape = 21, stroke = 0) +
    
    geom_text(aes(x = result, label = ifelse(result > 0, paste0("+", round(result,0)), round(result,0)), colour = "red" ),
              fontface = "bold", family = "Arial",
              size = 4) +
    
    labs(title = myPlotTitle) +
    
    theme(text = element_text(colour = "#2E4053"),
          plot.title = element_text(face = "bold", size = 14, colour = "#2E4053"),
          axis.text.y = element_text(colour = "#2B3947", size = 11),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y.left = element_text(colour = "#2B3947"),
          strip.text = element_text(face = "bold", size = 12, colour = "#2B3947"),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "transparent", size = 2),
          #panel.grid.major.x = element_line(colour = "#B4BDC5", size = 2),
          panel.grid.major.y = element_line(colour = "#1B2631", linetype = 2, size = 0.5),
          panel.spacing = unit(0.25, "cm"),
          legend.position = "none")
} 

# Function for generating Time Series plot:
myTimeSeries <- function(myData, indicator, resultMetric, nodeLayer, myTitle, yAxisLabel) {
  
  plotBase <- 
    myData %>%
    filter(indicators == indicator,
           resultType == resultMetric,
           levelName == nodeLayer) %>%
    ggplot(aes(x = as.factor(week), y = result, group = vName)) +
    geom_blank() 
  
  timeSeriesPlot <-
    plotBase +
    #geom_line(aes(colour = vName), alpha = 0.8, size = 1.5) +
    # geom_vline(xintercept = 30, linetype="dashed", 
    #            color = myGreys[1], size = 0.5) + #phase 3b
    # geom_text(aes(x = 29.5, label = "phase 3b", y = 5), colour = myGreys[2], angle= 90, size = 3) +
    # 
    # geom_vline(xintercept = 32, linetype="dashed", 
    #            color = myGreys[1], size = 0.5) + #phase 3c
    # geom_text(aes(x = 31.5, label = "phase 3c", y = 5), colour = myGreys[2], angle= 90, size = 3) +
    # 
    # geom_vline(xintercept = 34, linetype="dashed", 
    #            color = myGreys[1], size = 0.5) + #phase 3d
  # geom_text(aes(x = 33.5, label = "phase 3d", y = 5), colour = myGreys[2], angle= 90, size = 3) +
  # 
  # geom_vline(xintercept = 35, linetype="dashed", 
  #            color = myGreys[1], size = 0.5) + #phase 3e
  # geom_text(aes(x = 34.5, label = "phase 3e", y = 5), colour = myGreys[2], angle= 90, size = 3) +
  # 
  # geom_vline(xintercept = 36, linetype="dashed", 
  #            color = myGreys[1], size = 0.5) + #phase 3f
  # geom_text(aes(x = 35.5, label = "phase 3f", y = 5), colour = myGreys[2], angle= 90, size = 3) +
  # 
  # geom_vline(xintercept = 39, linetype="dashed", 
  #            color = myGreys[1], size = 0.5) + #phase 3g
  # geom_text(aes(x = 38.5, label = "phase 3g", y = 5), colour = myGreys[2], angle= 90, size = 3) +
  # geom_point(aes(colour = vName), size = 3.0, show.legend =  TRUE) +
  labs(title = myTitle) + #INDICATOR \n Node Layer
    theme(text = element_text(colour = "#2E4053"),
          plot.title = element_text(face = "bold", size = 12, colour = "#2E4053"),
          plot.subtitle = element_text(face = "bold", size = 10, colour = "#2E4053"),
          axis.text.y = element_text(colour = "#2B3947", size = 10),
          axis.text.x = element_text(colour = "#2B3947", size = 10),
          axis.title.y = element_text(colour = "#2B3947", size = 11),
          axis.title.x = element_text(colour = "#2B3947", size = 11),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5, colour = "#2B3947" ),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "transparent", size = 2),
          panel.grid.major.x = element_line(colour = "#aeb6bf"),
          panel.grid.major.y = element_line(colour = "#aeb6bf"),
          panel.spacing = unit(0.5, "cm"),
          legend.position = "bottom",
          legend.text=element_text(size= 12),
          panel.border = element_rect(fill = "transparent", colour = "white", size = 5)) +
    #scale_x_discrete(minor_breaks = FALSE, breaks = 11:40, labels = 11,12,"Lockdown") + 
    #scale_y_continuous(breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000), limits = c(0,4000)) +
    #scale_fill_manual(values=c(indicatorColours[1])) +
    #scale_shape_manual(values=c(21,22,23,24)) +
    labs(x = "ISO Week Number", y = yAxisLabel, colour = "Node") 
  
}

myRank <- function(myData, resultMetric, whichWeeks, whichNodes, nodeLayer, myTitle, yAxisLabel) {
  
  plotBase <- 
    myData %>%
    filter(resultType == resultMetric,
           levelName == nodeLayer,
           week %in% whichWeeks,
           vName %in% whichNodes) %>%
    # ggplot(aes(x = as.factor(week), y = reorder(result, desc(result)), group = vName)) +
    ggplot(aes(x = as.factor(week), y = as.numeric(result, length = 1), group = vName)) +
    geom_blank() 
  
  rankPlot <-
    plotBase +
    geom_line(aes(colour = indicators), alpha = 0.8, size = 1.5) +
    geom_point(aes(colour = indicators), size = 3.0, show.legend =  TRUE) +
    labs(title = myTitle) + #INDICATOR \n Node Layer
    theme(text = element_text(colour = "#2E4053"),
          plot.title = element_text(face = "bold", size = 12, colour = "#2E4053"),
          plot.subtitle = element_text(face = "bold", size = 10, colour = "#2E4053"),
          axis.text.y = element_text(colour = "#2B3947", size = 10),
          axis.text.x = element_text(colour = "#2B3947", size = 10),
          axis.title.y = element_text(colour = "#2B3947", size = 11),
          axis.title.x = element_text(colour = "#2B3947", size = 11),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5, colour = "#2B3947" ),
          strip.background = element_blank(),
          panel.background = element_rect(fill = "white", colour = "transparent", size = 2),
          panel.grid.major.x = element_line(colour = "#aeb6bf"),
          panel.grid.major.y = element_line(colour = "#aeb6bf"),
          panel.spacing = unit(0.5, "cm"),
          legend.position = "bottom",
          panel.border = element_rect(fill = "transparent", colour = "white", size = 5)) +
    #scale_fill_manual(values=c(indicatorColours[1])) +
    #scale_shape_manual(values=c(21,22,23,24)) +
    labs(x = "ISO Week Number", y = yAxisLabel, colour = "Node") 
  
}
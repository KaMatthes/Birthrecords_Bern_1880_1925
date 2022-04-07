.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.1.2/library"))

  

  library(survey)
  library(reshape2)
  library(viridis)
  library(lubridate)
  library(cowplot)
  library(rmarkdown)
  library(dplyr)
  library(kableExtra)
  library(broom)
  library(arsenal)
  library(scales)
  library(quantreg)
  library(tidyverse)
  library(zoo)
  library(sjPlot)
  library(car)
  library(RColorBrewer)
  library(ggplot2)
  library(ISOweek)
  library(mgcv)
  library(tsoutliers)
  library(strucchange)
  library(readxl)

  # original data
  data.bern <- "BernAll_V5.csv"
  data.grippe <- "GrippeBern.csv"
  
  
  data.bfs.stillborn <- readxl::read_excel(paste0("data_raw/Totgeburten_Schweiz.xlsx")) %>%
    mutate(year =as.factor(year))
  
  # Parameter zum Plotten
  mypalette <- viridis(7, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  mypalette2 <- viridis(14, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  mypalette3 <- brewer.pal(n = 10, name = "Spectral")
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
  
  size_axis <-20
  strip_text <- 20
  size_axis_title <- 20
  lwd_size_stillbirth <- 5
  lwd_size <- 0.8
  pch_type <- 19
  lwdline <- 1
  size_legend <- 15
  size_legend_title<- 15
  pd <-position_dodge(width=0.8)
  plot_title <- 25
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
  
  
  # 
  # source(paste0("R/GrippeExposure.R"))
  # source(paste0("R/data.R"))
  # 
  # load data
  load(paste0("data/databern.RData"))

  source(paste0("R/data_plot.R"))
  source(paste0("R/QuantileRegression1914_1922.R"))
  source(paste0("R/QuantileRegression1880_1900.R"))
  source(paste0("R/LogRegression_Stillborn.R"))
  source(paste0("R/LogRegression_Stillborn1880_1900.R"))
  source(paste0("R/LogRegression_Gestage.R"))
  source(paste0("R/LogRegression_Gestage1880_1900.R"))
  source(paste0("R/plot_kw.R"))

  
  render(paste0("R/Report_1914_1922.Rmd"), output_file = paste0("../output/",today(),"_Report_1914_1922.html"))
  render(paste0("R/Report_1880_1900.Rmd"), output_file = paste0("../output/",today(),"_Report_1880_1900.html"))
  
  render(paste0("R/Report_descriptive.Rmd"), output_file = paste0("../output/",today(),"_Report_descriptive.html"))
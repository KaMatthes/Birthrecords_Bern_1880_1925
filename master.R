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
  
  # original data
  data.bern <- "BernAll_V4.csv"
  data.grippe <- "GrippeBern.csv"
  
  
  data.bfs.stillborn <- read.csv(paste0("input/Totgeburten_Schweiz.csv"), header=TRUE, sep=";")%>%
    rename(year = `ï..year`) %>%
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
  
  # load data
  # load(paste0("input/databern.RData"))
  
  source(paste0("Rcode/data.R"))
  source(paste0("Rcode/data_missing.R"))
  # source(paste0("Rcode/data_missing_red.R"))
  source(paste0("Rcode/data_plot.R"))
  source(paste0("Rcode/QuantileRegression1914_1922.R")) # n

  source(paste0("Rcode/QuantileRegression1880_1900.R"))
  source(paste0("Rcode/QuantileRegression_years.R"))
  source(paste0("Rcode/QuantileRegression_years_flu.R"))
  source(paste0("Rcode/QuantileRegression_ExposureWeeks_flu.R"))
  source(paste0("Rcode/LogRegression_Stillborn.R"))
  source(paste0("Rcode/LogRegression_Stillborn_year.R"))
  source(paste0("Rcode/LogRegression_Stillborn_ExposureWeeks.R"))
  source(paste0("Rcode/LogRegression_Gestage.R"))
  source(paste0("Rcode/LogRegression_Gastage_year.R"))
  source(paste0("Rcode/LogRegression_Gastage_ExposureWeeks.R"))
  
  
  source(paste0("Rcode/plot_kw.R")) # n
  source(paste0("Rcode/PoissonRegression_Gestage.R"))
  source(paste0("Rcode/PoissonRegression_Stillborn.R"))
  
  # anzahl_missing <- sapply(databern, function(x) sum(is.na(x)))
  # anzahl_missing_per <- round(anzahl_missing/nrow(databern)*100,2)
  
  
  # Erstelle Htlm
  # render(paste0("Rcode/Report.Rmd"), output_file = paste0("/Users/katarina/Dropbox/Masterarbeit Vivienne/Analysis/output/",today(),"_Report.html"))
  
  # 
  # render(paste0("Rcode/Report.Rmd"), output_file = paste0("C:/Users/kmatth/Dropbox/Masterarbeit Vivienne/Analysis/output/",today(),"_Report.html"))
  
  render(paste0("Rcode/20220315_Report_flu.Rmd"), output_file = paste0("C:/Users/kmatth/Dropbox/Masterarbeit Vivienne/Analysis/output/",today(),"_Report_flu.html"))
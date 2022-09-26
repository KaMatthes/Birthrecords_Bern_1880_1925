.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.2.1/library"))

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
  library(ggsci) 
  library(ggbeeswarm)
  library(ggeffects)
  library(xlsx)
  library(export)

  # original data
  data.bern <- "Bern_birth.csv"
  data.flu <- "Flu_Bern.csv"
  
  # Parameter for figures
  mypalette2 <- viridis(14, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  mypalette4 <- brewer.pal(n = 9, name = "Set1") 
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00","#A6761D" ,"#7570B3")
  mypalette3 <- brewer.pal(n = 10, name = "Spectral")
  mypalette7 <-pal_jco()(10)
   
  size_axis <-12
  strip_text <- 10
  size_axis_title <- 12
  lwd_size_stillbirth <- 5
  lwd_size <- 0.8
  pch_type <- 19
  lwdline <- 1
  size_legend <- 15
  size_legend_title<- 15
  pd <-position_dodge(width=0.8)
  fatten_size <- 2.5
  plot_title <- 25

  
  # load data
  load("data/databern.RData")
  
  # prepare data
  
  used.data <- databern %>%
    filter(multiple==0) %>%
    filter(!(weight < 1000)) %>%
    filter(!(matage > 50))  %>%
    filter(!(matage <14)) %>%
    filter(!(gest <30)) %>%
    mutate(
      year = as.factor(year),
      boy = as.factor(boy),
      stillborn = as.factor(stillborn),
      city = as.factor(city),
      insurance = as.factor(insurance),
      married = as.factor(married),
      matbody2 = as.factor(matbody2),
      occupation2 = as.factor(occupation2),
      matheight2  = as.factor(matheight2 ),
      malnutrition2 = as.factor(malnutrition2),
      Gest_group = factor(Gest_group, levels=c("normal","early")),
      parity = factor(parity, levels=c("1","2","3",">=4")),
      city = factor(city, levels=c("1","0")),
      married = factor(married, levels=c("1","0")),
      boy = factor(boy, levels=c("1","0")),
      birth_season = factor( birth_season, levels=c("Spring","Summer","Fall","Winter")),
      parity = factor(parity, levels = c("1","2","3",">=4")),
      matbody2 = factor(matbody2, levels=c("2","1","3")),
      matheight2 = factor(matheight2, levels=c("2","1","3")),
      malnutrition2 = factor(malnutrition2, levels=c("0","1")),
      occupation2 = factor(occupation2, levels=c("4","1","2","3","5","6","7")),
      Exposure_sum_dummy = as.factor(Exposure_sum_dummy)
    )
  
  
# R scripts
  source(paste0("R/Table1.R"))
  source(paste0("R/Figure1.R"))
  source(paste0("R/Figure2.R"))
  source(paste0("R/Figure3.R"))
  source(paste0("R/Supplement_Figure2.R"))
  source(paste0("R/Supplement_Figure3to6.R"))
  source(paste0("R/Supplement_Figure7and9.R"))
  source(paste0("R/Supplement_Figure8.R"))
  source(paste0("R/Supplement_Table3.R"))
  source(paste0("R/Supplement_Table4.R"))
  source(paste0("R/Supplement_Table5to7.R"))
  source(paste0("R/Supplement_Table8.R"))
  
  
  
  
function_plot_timeseries <- function(Timespan, Var) {
  
  if (Timespan =="weekly") {
 
   data_kw <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    filter(stillborn=="0") %>%
    dplyr::select(year, birth_isoweek,weight, gest) %>%
    arrange(year, birth_isoweek) %>%
    group_by(year, birth_isoweek) %>%
    mutate(weight_mean = mean(weight),
           gest_mean = mean(gest)) %>%
    ungroup() %>%
    mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
           Year_week = paste0(year,"_",birth_isoweek),
           birth_isoweek_W = paste0("W",birth_isoweek),
           birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
           birth_time = ISOweek2date( birthweek_year)) %>%
    distinct(Year_week, .keep_all = TRUE) %>%
    group_by(year) %>%
    mutate(mean_year = mean(weight)) %>%
    ungroup() %>%
    arrange(year, birth_isoweek)
   
 if(Var=="Weight") { 
   data_w_ts <- data_kw %>%
     filter(!birth_isoweek==53) %>%
     dplyr::select(weight_mean)%>%
     ts(frequency = 52, start = 1914)
   
   bp_w_ts <- breakpoints(data_w_ts ~ 1)
   
   data_w_ts %>%
     decompose(type = "additive") %>%
     autoplot(range.bars = FALSE) +
     geom_vline(xintercept = 1919.500, linetype="dashed")
 }
   
else if(Var=="Gest") {
   data_w_ts <- data_kw %>%
     filter(!birth_isoweek==53) %>%
     dplyr::select(gest_mean)%>%
     ts(frequency = 52, start = 1914)
   
   bp_w_ts <- breakpoints(data_w_ts ~ 1)
   
   data_w_ts %>%
     decompose(type = "additive") %>%
     autoplot(range.bars = FALSE) 
#      geom_vline(xintercept = 1919.500, linetype="dashed")

   }
  }
  
  
  else if (Timespan=="monthly") {
    
    data_month <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn=="0") %>%
      dplyr::select(year, birth_month,weight, gest) %>%
      arrange(year, birth_month) %>%
      group_by(year, birth_month) %>%
      mutate(weight_mean = mean(weight),
             gest_mean = mean(gest)) %>%
      ungroup() %>%
      mutate(Year_week = paste0(year,"_",birth_month),
             birth_time_month = as.Date(paste0(year,"-",sprintf("%02d", birth_month),"-01"))) %>%
      distinct(Year_week, .keep_all = TRUE) 
    
    if(Var=="Weight") { 
    data_m_ts <- data_month %>%
      dplyr::select(weight_mean)%>%
      ts(frequency = 12, start = 1914)

    bp_m_ts <- breakpoints(data_m_ts ~ 1)
    
      data_m_ts %>%
      decompose(type = "additive") %>%
      autoplot(range.bars = FALSE) +
      geom_vline(xintercept = 1919.333, linetype="dashed") 
      
    }
    
    else if(Var=="Gest") {
      
      data_m_ts <- data_month %>%
        dplyr::select(gest_mean)%>%
        ts(frequency = 12, start = 1914)
      
      bp_m_ts <- breakpoints(data_m_ts ~ 1)
      
      data_m_ts %>%
        decompose(type = "additive") %>%
        autoplot(range.bars = FALSE)
        # geom_vline(xintercept = 1919.333, linetype="dashed") 
    }
  }
  
  # else if (Timespan=="weekly") {
  #   
  #   data_weekly <- used.data %>%
  #     mutate(year_num = as.integer(as.character(year)),
  #            birth_weekday = wday(dmy(birthday2),week_start = 1)) %>%
  #     filter(year_num >1913) %>%
  #     filter(stillborn=="0") %>%
  #     select(year, birth_month,weight,birthday2,birth_weekday) %>%
  #     arrange(year, birth_weekday) %>%
  #     group_by(year, birth_weekday) %>%
  #     mutate(weight_mean = mean(weight)) %>%
  #     ungroup() %>%
  #     mutate(Year_week_day = paste0(year,"_",birth_weekday),
  #            birth_time_month = as.Date(paste0(year,"-",sprintf("%02d", birth_month),"-01"))) %>%
  #     distinct(Year_week_day, .keep_all = TRUE) 
  #   
  #   data_d_ts <-data_weekly%>%
  #     select(weight_mean)%>%
  #     ts(frequency = 7, start = 1914)
  #   
  #   bp_d_ts <- breakpoints(data_d_ts ~ 1)
  #   
  #   data_d_ts %>%
  #     decompose(type = "additive") %>%
  #     autoplot(range.bars = FALSE) +
  #     geom_vline(xintercept = 1919.333, linetype="dashed") 
  #   
  # }
  
}
  

     
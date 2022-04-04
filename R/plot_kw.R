function_plot_kw <- function(Timespan) {
  
  if (Timespan =="weekly") {
 
   data_kw <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1911) %>%
    filter(stillborn=="0") %>%
    select(year, birth_isoweek,weight) %>%
    arrange(year, birth_isoweek) %>%
    group_by(year, birth_isoweek) %>%
    mutate(weight_mean = mean(weight)) %>%
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
  
   data_w_ts <- data_kw %>%
     filter(!birth_isoweek==53) %>%
     select(weight_mean)%>%
     ts(frequency = 52, start = 1912)
   
   bp_w_ts <- breakpoints(data_w_ts ~ 1)
   
   data_w_ts %>%
     decompose(type = "additive") %>%
     autoplot(range.bars = FALSE) +
     geom_vline(xintercept = 1919.500, linetype="dashed")
  

  }
  
  
  else if (Timespan=="monthly") {
    
    data_month <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1911) %>%
      filter(stillborn=="0") %>%
      select(year, birth_month,weight) %>%
      arrange(year, birth_month) %>%
      group_by(year, birth_month) %>%
      mutate(weight_mean = mean(weight)) %>%
      ungroup() %>%
      mutate(Year_week = paste0(year,"_",birth_month),
             birth_time_month = as.Date(paste0(year,"-",sprintf("%02d", birth_month),"-01"))) %>%
      distinct(Year_week, .keep_all = TRUE) %>%
      mutate(Color_pandemic = ifelse(year == "1918"  | year == "1919", "Pandemic", "No-Pandemic"))
    
    data_m_ts <- data_month %>%
      select(weight_mean)%>%
      ts(frequency = 12, start = 1912)

    bp_m_ts <- breakpoints(data_m_ts ~ 1)
    
      data_m_ts %>%
      decompose(type = "additive") %>%
      autoplot(range.bars = FALSE) +
      geom_vline(xintercept = 1919.333, linetype="dashed") 
      
  }
  
}
  

     
function_plot_timeseries_stillborn <- function(Timespan, Var) {
  
  if (Timespan =="weekly") {
    
    data_month <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      dplyr::select(year, birth_isoweek, stillborn) %>%
      arrange(year, birth_isoweek) %>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year)) %>%
      group_by(year,birth_isoweek) %>%
      count()
 
   data_kw <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    dplyr::select(year, birth_isoweek,weight, stillborn) %>%
     mutate(stillborn = as.character(stillborn),
            stillborn = as.integer(stillborn)) %>%
    arrange(year, birth_isoweek) %>%
    group_by(year, birth_isoweek) %>%
    mutate(stillborn_sum = sum(stillborn))%>%
    ungroup() %>%
    mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
           Year_week = paste0(year,"_",birth_isoweek),
           birth_isoweek_W = paste0("W",birth_isoweek),
           birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
           birth_time = ISOweek2date( birthweek_year)) %>%
    distinct(Year_week, .keep_all = TRUE) %>%
    # group_by(year) %>%
    # mutate(mean_year = mean(weight)) %>%
    # ungroup() %>%
    arrange(year, birth_isoweek) %>%
     full_join(data_month) %>%
     mutate(prop = (stillborn_sum/n)*100)
     
   data_w_ts <- data_kw %>%
     filter(!birth_isoweek==53) %>%
     dplyr::select(prop)%>%
     ts(frequency = 52, start = 1914)
  
   bp_w_ts <- breakpoints(data_w_ts ~ 1)
   
   ts_stillborn <- data_w_ts %>%
     decompose(type = "additive") %>%
     autoplot(range.bars = FALSE)  +
     # geom_vline(xintercept = 1919.288, linetype="dashed") +
     annotate("rect",xmin=1918.462,xmax=1918.981,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
     xlab("Year") +
     ggtitle("Time series - Proportion Stillborn") +
     theme_bw() +
     theme( axis.text = element_text(size=  size_axis,),
            axis.title = element_text(size=  size_axis_title))
   
   cowplot::save_plot("output/ts_stillborn.pdf",ts_stillborn ,base_height=10,base_width=15)
 
  }
  
  
  else if (Timespan=="monthly") {
    
    data_month <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      dplyr::select(year, birth_month,weight, stillborn) %>%
      arrange(year, birth_month) %>%
      group_by(year, birth_month) %>%
      count()
    
    data_kw <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      dplyr::select(year, birth_month,weight, stillborn) %>%
      mutate(stillborn = as.character(stillborn),
             stillborn = as.integer(stillborn)) %>%
      arrange(year, birth_month) %>%
      group_by(year, birth_month) %>%
      mutate(stillborn_sum = sum(stillborn))%>%
      ungroup() %>%
      mutate(Year_week = paste0(year,"_",birth_month),
             birth_time_month = as.Date(paste0(year,"-",sprintf("%02d", birth_month),"-01"))) %>%
      distinct(Year_week, .keep_all = TRUE) %>%
      full_join(data_month) %>%
      mutate(prop = (stillborn_sum/n)*100)
    
    data_m_ts <- data_kw %>%
      dplyr::select(prop)%>%
      ts(frequency = 12, start = 1914)

    bp_m_ts <- breakpoints(data_m_ts ~ 1)
    
      data_m_ts %>%
      decompose(type = "additive") %>%
      autoplot(range.bars = FALSE) 
      # geom_vline(xintercept = 1919.333, linetype="dashed") 
      
   
  }
  
}
  

     
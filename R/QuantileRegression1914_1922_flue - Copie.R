function_quantile_regression_1914_1922 <- function(varExp) {

# data
 if( varExp == "Exposure_sum") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      droplevels
    
    formula<-as.formula( paste("weight ~ year +boy+parity+Gest_group+city+matage+ married+",eval(substitute(varExp))))
    qr1 <- rq(formula , data=datared , tau =c(0.1, 0.5, 0.9))
    
    # plot(summary(qr1))
    
    qr1_sum <- summary(qr1,se = "boot", bsmethod= "xy")
    
    qr1_std_error01 <- qr1_sum[[1]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.1)
    
    qr1_std_error05 <-qr1_sum[[2]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.5)
    
    qr1_std_error09 <-qr1_sum[[3]][3]$coefficients[,2]%>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.9)
    
    qr1_std_error <- rbind(qr1_std_error01, qr1_std_error05,qr1_std_error09)
    
    data_qr1 <- qr1$coefficients %>%
      as.data.frame()%>%
      mutate(Variables=row.names(.)) %>%
      gather(., condition, measurement, `tau= 0.1`:`tau= 0.9`,factor_key=TRUE)%>%
      mutate(quantile = as.numeric(substr(condition,6,8))) %>%
      select(-condition) %>%
      left_join(qr1_std_error) %>%
      mutate(CIl = measurement-1.96*sd_error,
             CIu = measurement+1.96*sd_error,
             quantile = as.factor(quantile))
    
    
    data_sum <- data_qr1 %>%
      filter(Variables=="Exposure_sum1" | Variables=="Exposure_sum2" | Variables=="Exposure_sum3")%>%
      mutate(Variables=as.factor(Variables))%>%
      add_row(Variables = c("Exposure_sum0","Exposure_sum0","Exposure_sum0"), measurement = c(0,0,0),
              quantile=c("0.1","0.5","0.9"),sd_error=c(0,0,0),CIl=c(0,0,0),CIu=c(0,0,0))
    
    
    CoeffPlotsum <- ggplot( data_sum , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      labs(x="Quantile", y="Birthweight difference in g") +
      ggtitle(paste((varExp)))+
      scale_color_manual("Exposure:",values =  cbp1 )+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "bottom")
    
    return( CoeffPlotsum)
  }
  
  
  else if( varExp == "unadjusted_year") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      droplevels
    
    formula<-as.formula( paste("weight ~ year"))
    qr1 <- rq(formula , data=datared , tau =c(0.1, 0.5, 0.9))
    
    # plot(summary(qr1))
    
    qr1_sum <- summary(qr1,se = "boot", bsmethod= "xy")
    
    qr1_std_error01 <- qr1_sum[[1]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.1)
    
    qr1_std_error05 <-qr1_sum[[2]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.5)
    
    qr1_std_error09 <-qr1_sum[[3]][3]$coefficients[,2]%>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.9)
    
    qr1_std_error <- rbind(qr1_std_error01, qr1_std_error05,qr1_std_error09)
    
    data_qr1 <- qr1$coefficients %>%
      as.data.frame()%>%
      mutate(Variables=row.names(.)) %>%
      gather(., condition, measurement, `tau= 0.1`:`tau= 0.9`,factor_key=TRUE)%>%
      mutate(quantile = as.numeric(substr(condition,6,8))) %>%
      select(-condition) %>%
      left_join(qr1_std_error) %>%
      mutate(CIl = measurement-1.96*sd_error,
             CIu = measurement+1.96*sd_error,
             quantile = as.factor(quantile))
    
    
    data_sum <- data_qr1 %>%
      filter(!Variables=="(Intercept)") %>%
      mutate(Variables=as.factor(Variables))%>%
      add_row(Variables = c("year1912-13","year1912-13","year1912-13"), measurement = c(0,0,0),
              quantile=c("0.1","0.5","0.9"),sd_error=c(0,0,0),CIl=c(0,0,0),CIu=c(0,0,0))
    
    
    CoeffPlotsum <- ggplot( data_sum , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      labs(x="Quantile", y="Birthweight difference in g") +
      ggtitle(paste((varExp)))+
      scale_color_manual("Year:",values =   mypalette3)+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "bottom")
  
    
    return( CoeffPlotsum)
  }
  
  
  else if( varExp == "unadjusted_year_linear") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      droplevels
    
    formula<-as.formula( paste("weight ~ year"))
    qr1 <- lm(formula , data=datared)
    
    # plot(summary(qr1))
    
    qr1_sum <- summary(qr1)
    
    qr1_results <- data.frame(qr1_sum$coefficients) %>%
      mutate( CIl = Estimate-1.96*Std..Error,
              CIu = Estimate+1.96*Std..Error,
              Year = row.names(.)) %>%
      select(Year, Estimate, CIl, CIu) %>%
      add_row(Year = "year1912-13", Estimate =0, CIl=0, CIu=0) %>%
      filter(!Year =="(Intercept)")

    
    CoeffPlotsum <- ggplot(  qr1_results, aes(x=Year,y=Estimate),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Year),position=pd,lwd=lwd_size)+
      labs(x="Year", y="Birthweight difference in g") +
      ggtitle("Birth weight")+
      scale_color_manual("Year:",values =   mypalette3)+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text.x=element_text(color="black",size=6),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "none")
    
    
    return( CoeffPlotsum)
  }
  
  
  
  else if( varExp == "adjusted_year_linear") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"),
      #        Exposure_sum = as.factor(Exposure_sum),
      #        birth_month = as.factor(birth_month))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum),
             year = as.character(year),
             year=replace(year, year=="1912", "1912-1913"),
             year=replace(year, year=="1913", "1912-1913"),
             birth_month = as.factor(birth_month)) %>%
      droplevels
    
    qr1 <- lm(weight ~ year +boy+parity+Gest_group+birth_month+matage+ married+Exposure_sum , data=datared)
    
summary(qr1)
}
    

else if( varExp == "adjusted_year_linear_Exp") {
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    filter(stillborn =="0") %>%
    # mutate(year = as.character(year),
    #        year=replace(year, year=="1912", "1912-1913"),
    #        year=replace(year, year=="1913", "1912-1913"),
    #        Exposure_sum = as.factor(Exposure_sum),
    #        birth_month = as.factor(birth_month))%>%
    mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
           Year_week = paste0(year,"_",birth_isoweek),
           birth_isoweek_W = paste0("W",birth_isoweek),
           birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
           birth_time = ISOweek2date( birthweek_year),
           Birth_year_week_num = as.numeric(birth_time)/1000,
           Exposure_sum = as.factor(Exposure_sum),
           year = as.character(year),
           year=replace(year, year=="1912", "1912-1913"),
           year=replace(year, year=="1913", "1912-1913")) %>%
    droplevels
  
  qr1 <- lm(weight ~ Exposure_sum , data=datared)
  
  summary(qr1)
  
  }
  
  else if( varExp == "unadjusted_gam_model") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000) %>%
      droplevels
    qr1 <- gam(weight ~ s(Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12),data=datared)
     # qr2 <- gam(weight ~ s( Birth_year_week_num, k =120) + s(birth_month,bs = "cc", k = 12),data=datared)
    plot_time <- plot(qr1, select=1)
    plot_month <- plot(qr1, select=2)
    
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 

  }
  
  else if( varExp == "adjusted_gam_model") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum)) %>%
      droplevels
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+matage+ married+Exposure_sum,data=datared)
   summary(qr1)
   # plot_time <- plot(qr1, select=1)
   # plot_month <- plot(qr1, select=2)
   #  
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 
    
  }
  
  else if( varExp == "adjusted_gam_model_Exp") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum)) %>%
      droplevels
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) +Exposure_sum,data=datared)
    summary(qr1)
    # plot_time <- plot(qr1, select=1)
    # plot_month <- plot(qr1, select=2)
    #  
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 
    
  }
  
  
  else if( varExp == "adjusted_gam_model_plot") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum)) %>%
      droplevels
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+matage+ married+Exposure_sum,data=datared)
    
    plot_time <- plot(qr1, select=1)
    plot_month <- plot(qr1, select=2)
    #  
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 
    
  }
  
  else if( varExp == "adjusted_gam_model_gest") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum)) %>%
      droplevels
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+Gest_group+matage+ married+Exposure_sum,data=datared)
    summary(qr1)
    # plot_time <- plot(qr1, select=1)
    # plot_month <- plot(qr1, select=2)
    #  
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 
    
  }
  
  else if( varExp == "adjusted_gam_model_plot_gest") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      
      # mutate(year = as.character(year),
      #        year=replace(year, year=="1912", "1912-1913"),
      #        year=replace(year, year=="1913", "1912-1913"))%>%
      mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
             Year_week = paste0(year,"_",birth_isoweek),
             birth_isoweek_W = paste0("W",birth_isoweek),
             birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
             birth_time = ISOweek2date( birthweek_year),
             Birth_year_week_num = as.numeric(birth_time)/1000,
             Exposure_sum = as.factor(Exposure_sum)) %>%
      droplevels
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+Gest_group+matage+ married+Exposure_sum,data=datared)
    
    plot_time <- plot(qr1, select=1)
    plot_month <- plot(qr1, select=2)
    #  
    # op <- par(mfrow=c(2,1))
    # plot(qr1,all.terms=TRUE)
    # par(op) 
    
  }
  else if( varExp == "unadjusted_Exposure_sum") {
    datared <- used.data %>%
      mutate(year_num = as.integer(as.character(year))) %>%
      filter(year_num >1913) %>%
      filter(stillborn =="0") %>%
      mutate(year = as.character(year),
             year=replace(year, year=="1912", "1912-1913"),
             year=replace(year, year=="1913", "1912-1913"),
             Exposure_sum = as.factor(Exposure_sum))%>%
      droplevels
    
    
    formula<-as.formula( paste("weight ~ Exposure_sum"))
    qr1 <- rq(formula , data=datared , tau =c(0.1, 0.5, 0.9))
    
    # plot(summary(qr1))
    
    qr1_sum <- summary(qr1,se = "boot", bsmethod= "xy")
    
    qr1_std_error01 <- qr1_sum[[1]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.1)
    
    qr1_std_error05 <-qr1_sum[[2]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.5)
    
    qr1_std_error09 <-qr1_sum[[3]][3]$coefficients[,2]%>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.9)
    
    qr1_std_error <- rbind(qr1_std_error01, qr1_std_error05,qr1_std_error09)
    
    data_qr1 <- qr1$coefficients %>%
      as.data.frame()%>%
      mutate(Variables=row.names(.)) %>%
      gather(., condition, measurement, `tau= 0.1`:`tau= 0.9`,factor_key=TRUE)%>%
      mutate(quantile = as.numeric(substr(condition,6,8))) %>%
      select(-condition) %>%
      left_join(qr1_std_error) %>%
      mutate(CIl = measurement-1.96*sd_error,
             CIu = measurement+1.96*sd_error,
             quantile = as.factor(quantile))
    
    
    data_sum <- data_qr1 %>%
      filter(Variables=="Exposure_sum1" | Variables=="Exposure_sum2" | Variables=="Exposure_sum3")%>%
      mutate(Variables=as.factor(Variables))%>%
      add_row(Variables = c("Exposure_sum0","Exposure_sum0","Exposure_sum0"), measurement = c(0,0,0),
              quantile=c("0.1","0.5","0.9"),sd_error=c(0,0,0),CIl=c(0,0,0),CIu=c(0,0,0))
    
    
    CoeffPlotsum <- ggplot( data_sum , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      labs(x="Quantile", y="Birthweight difference in g") +
      ggtitle(paste((varExp)))+
      scale_color_manual("Exposure:",values =  cbp1 )+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "bottom")
    
    return( CoeffPlotsum)
  }
  
  else {
    datared <- used.data %>%
        filter(year==1918  | year==1919) %>%
        filter(stillborn =="0") %>%
        droplevels
    
    formula<-as.formula( paste("weight ~ year +boy+parity+Gest_group+birth_season+city+matage+ married+",eval(substitute(varExp))))
    qr1 <- rq(formula , data=datared , tau =c(0.1, 0.5, 0.9))
    
    # plot(summary(qr1))
    
    qr1_sum <- summary(qr1,se = "boot", bsmethod= "xy")
    
    qr1_std_error01 <- qr1_sum[[1]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.1)
    
    qr1_std_error05 <-qr1_sum[[2]][3]$coefficients[,2] %>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.5)
    
    qr1_std_error09 <-qr1_sum[[3]][3]$coefficients[,2]%>%
      as.data.frame() %>%
      rename(sd_error = ".") %>%
      mutate(Variables = row.names(.),
             quantile = 0.9)
    
    qr1_std_error <- rbind(qr1_std_error01, qr1_std_error05,qr1_std_error09)
    
    data_qr1 <- qr1$coefficients %>%
      as.data.frame()%>%
      mutate(Variables=row.names(.)) %>%
      gather(., condition, measurement, `tau= 0.1`:`tau= 0.9`,factor_key=TRUE)%>%
      mutate(quantile = as.numeric(substr(condition,6,8))) %>%
      select(-condition) %>%
      left_join(qr1_std_error) %>%
      mutate(CIl = measurement-1.96*sd_error,
             CIu = measurement+1.96*sd_error,
             quantile = as.factor(quantile))
    
    data_exposure <- data_qr1 %>%
      filter(Variables==eval(substitute(varExp)))%>%
      mutate(Variables=as.factor(Variables))
    
    CoeffPlotexposure <- ggplot( data_exposure  , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      ggtitle(paste((varExp)))+
      labs(x="Quantile", y="Birthweight difference in g") +
      scale_color_manual("",values =  cbp1)+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "bottom")
    
    data_year <- data_qr1 %>%
      filter( Variables=="year1919")%>%
      mutate(Variables=as.factor(Variables))%>%
      add_row(Variables = c("1918","1918","1918"), measurement = c(0,0,0),
              quantile=c("0.1","0.5","0.9"),sd_error=c(0,0,0),CIl=c(0,0,0),CIu=c(0,0,0))
    
    CoeffPlotYear <- ggplot(data_year , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      labs(x="Quantile", y="Birthweight difference in g") +
      ggtitle(paste((varExp)))+
      scale_color_manual("Year:",values = cbp1 )+
      theme_bw()+
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=15),
            axis.title=element_text(size=15),
            legend.text=element_text(size=15),
            legend.title =element_blank(),
            plot.title = element_text(size=15),
            legend.position = "bottom")
    
    
    AllCoef <- cowplot::plot_grid(CoeffPlotexposure,NULL,CoeffPlotYear,
                                  rel_widths = c(1, 0.1, 1),
                                  ncol=3,nrow=1)
    
    return(AllCoef)
  }
}

function_quantile_regression_1914_1922 <- function(varExp) {
  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    filter(stillborn=="0") %>%
    mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
           Year_week = paste0(year,"_",birth_isoweek),
           birth_isoweek_W = paste0("W",birth_isoweek),
           birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
           birth_time = ISOweek2date( birthweek_year),
           Birth_year_week_num = as.numeric(birth_time),
           year = as.character(year),
           birth_month = as.factor(birth_month),
           occupation2 = dplyr::recode(occupation2,
                                       "5" ="7",
                                       "6" = "7")) %>%
    droplevels
  
  if( varExp == "unadjusted_year_linear") {

    formula<-as.formula( paste("weight ~ year"))
    qr1 <- lm(formula , data=datared)
    qr1_sum <- summary(qr1)
    
    qr1_results <- data.frame(qr1_sum$coefficients) %>%
      mutate( CIl = Estimate-1.96*Std..Error,
              CIu = Estimate+1.96*Std..Error,
              Year = row.names(.)) %>%
      select(Year, Estimate, CIl, CIu) %>%
      add_row(Year = "year1914", Estimate =0, CIl=0, CIu=0) %>%
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
  
  
  
  else if( varExp == "unadjusted_year_qr") {
    
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
      add_row(Variables = c("year1914","year1914","year1914"), measurement = c(0,0,0),
              quantile=c("0.1","0.5","0.9"),sd_error=c(0,0,0),CIl=c(0,0,0),CIu=c(0,0,0))
    
    
    CoeffPlotsum <- ggplot( data_sum , aes(x=quantile, y=measurement),position=pd) + 
      geom_hline(yintercept=0, colour="grey") + 
      geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Variables),position=pd,lwd=lwd_size)+
      labs(x="Quantile", y="Birthweight difference in g") +
      # ggtitle(paste((varExp)))+
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
  
  else if( varExp == "linear_Exp") {
    
    qr1 <- lm(weight ~ Exposure_sum , data=datared)
    
    summary(qr1)
    
  }
  
  
  else if( varExp == "linear_Int") {
    
    qr1 <- lm(weight ~ Flu_intensity_all , data=datared)
    
    summary(qr1)
    
  }
  
  else if( varExp == "unadjusted_gam_model") {
    
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
  
    qr1 <- gam(weight ~ s(Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12),data=datared)
  
    summary(qr1)


    
    
  }
  else if( varExp == "unadjusted_gam_model_plot") {

    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    format_x <- function(x){ 
      as.Date(x)
    }
    
    z <- qnorm(0.975)
    
    qr1 <- gam(weight ~ s(Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12),data=datared)

    plot.data <- {
      pdf(NULL)
      res <- plot(qr1)
      invisible(dev.off())
      res
    }
    
    data.plot.time <- data.frame(x=plot.data[[1]]$x) %>%
      mutate(fit =  plot.data[[1]]$fit,
             se  =  plot.data[[1]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      geom_vline(xintercept=as.numeric(as.Date("1918-06-03")), col="grey",lwd=0.5)+
      geom_vline(xintercept=as.numeric(as.Date("1919-06-02")),col="grey",lwd=0.5)+
      xlab("Birth date")+
      ylab("Effect on Birthweight")+
      scale_x_continuous(labels=format_x )+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
    
    data.plot.month <- data.frame(x=plot.data[[2]]$x) %>%
      mutate(fit =  plot.data[[2]]$fit,
             se  =  plot.data[[2]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      xlab("Birth month")+
      ylab("Effect on Birthweight")+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
  

    return(list(data.plot.time,data.plot.month))
    
    
  }
  
  else if( varExp == "adjusted_gam_model_Exp") {
    
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) +Exposure_sum,data=datared)
    summary(qr1)
    
  }
  
  else if( varExp == "adjusted_gam_model_Int") {
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) +Flu_intensity_all,data=datared)
    summary(qr1)

  }
  
# adjusted
  
  else if( varExp == "adjusted_year_linear_Exp") {
    
    
    qr1 <- lm(weight ~ year +boy+parity+Gest_group+birth_month+matage+ married+city +insurance+ Exposure_sum , data=datared)
    
    summary(qr1)
  }
  
  else if( varExp == "adjusted_year_linear_Int") {
    
    
    qr1 <- lm(weight ~ year +boy+parity+Gest_group+birth_month+matage+ married+city +insurance+ Flu_intensity_all, data=datared)
    
    summary(qr1)
  }
  

  
  else if( varExp == "adjusted_gam_model") {
    
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
   qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+matage+ married+city+ insurance+Exposure_sum,data=datared)
   summary(qr1)
   
    
  }
  

  else if( varExp == "adjusted_gam_model_plot") {
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    format_x <- function(x){ 
      as.Date(x)
    }
    
    z <- qnorm(0.975)
    
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+matage+ married+city +insurance+Exposure_sum,data=datared)

    plot.data <- {
      pdf(NULL)
      res <- plot(qr1)
      invisible(dev.off())
      res
    }
    
    data.plot.time <- data.frame(x=plot.data[[1]]$x) %>%
      mutate(fit =  plot.data[[1]]$fit,
             se  =  plot.data[[1]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      geom_vline(xintercept=as.numeric(as.Date("1918-06-03")), col="grey",lwd=0.5)+
      geom_vline(xintercept=as.numeric(as.Date("1919-06-02")),col="grey",lwd=0.5)+
      xlab("Birth date")+
      ylab("Effect on Birthweight")+
      scale_x_continuous(labels=format_x )+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
    
    data.plot.month <- data.frame(x=plot.data[[2]]$x) %>%
      mutate(fit =  plot.data[[2]]$fit,
             se  =  plot.data[[2]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      xlab("Birth month")+
      ylab("Effect on Birthweight")+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
    
    
    
    
    return(list(data.plot.time,data.plot.month))
    

    
    
  }
  
  else if( varExp == "adjusted_gam_model_gest") {
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+Gest_group+matage+ married+city+insurance+Exposure_sum,data=datared)
    summary(qr1)

    
  }
  
  else if( varExp == "adjusted_gam_model_plot_gest") {
    datared <-   datared %>%
      mutate(birth_month= as.integer(birth_month))
    
    format_x <- function(x){ 
      as.Date(x)
    }
    
    z <- qnorm(0.975)
    
    qr1 <- gam(weight ~ s( Birth_year_week_num, k=20) + s(birth_month,bs = "cc", k = 12) + boy+parity+Gest_group+matage+ married+insurance+Exposure_sum,data=datared)
    
    plot.data <- {
      pdf(NULL)
      res <- plot(qr1)
      invisible(dev.off())
      res
    }
    
    data.plot.time <- data.frame(x=plot.data[[1]]$x) %>%
      mutate(fit =  plot.data[[1]]$fit,
             se  =  plot.data[[1]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      geom_vline(xintercept=as.numeric(as.Date("1918-06-03")), col="grey",lwd=0.5)+
      geom_vline(xintercept=as.numeric(as.Date("1919-06-02")),col="grey",lwd=0.5)+
      xlab("Birth date")+
      ylab("Effect on Birthweight")+
      scale_x_continuous(labels=format_x )+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
    
    data.plot.month <- data.frame(x=plot.data[[2]]$x) %>%
      mutate(fit =  plot.data[[2]]$fit,
             se  =  plot.data[[2]]$se,
             CIl =  fit - z*se,
             CIu =  fit + z*se) %>%
      select(-se) %>%
      gather(., variable, value, fit:CIu ) %>%
      ggplot() +
      geom_line(aes(x=x,y=value, col=variable,linetype=variable),lwd=lwdline)+
      xlab("Birth month")+
      ylab("Effect on Birthweight")+
      scale_colour_manual("",
                          values = c("#555555","#555555","#555555"),
                          limits=c("fit","CIl","CIu"),
                          labels=c("fully adjusted","95% CI","95% CI"))+
      scale_linetype_manual(name= "",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      theme_bw()+
      theme(
        strip.text.x=element_text(size=strip_text),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=size_legend),
        legend.title=element_text(size=size_legend_title),
        legend.position="none")
    
    
    
    return(list(data.plot.time,data.plot.month))
    


    
  }
  
}

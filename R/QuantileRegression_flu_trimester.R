function_quantile_regression_trimester <- function(varExp, Trimester) {
  
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
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
 
  
 
  
  if( varExp == "linear_Int") {
    
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
    
    
    qr1 <- lm(weight ~ boy+parity+Gest_group+birth_month+matage+ married+city +insurance+ Flu_intensity_all, data=datared)
    
    summary(qr1)
  }
  
  
  else if( varExp == "adjusted_year_linear") {
    
    
    qr1 <- lm(weight ~ year + boy+parity+Gest_group+birth_month+matage+ married+city +insurance, data=datared)
    
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

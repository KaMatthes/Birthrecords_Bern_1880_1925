# Birthweight

function_regression_trimester <- function(trimester, varExp) {
  
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
           birth_month = as.factor(birth_month)) %>%
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
  
  if(varExp=="unadjusted") {
    
    formula<-as.formula(paste("weight ~",eval(substitute(trimester))))
    res_un <- data.frame(summary(lm(formula , data=datared))$coefficients) %>%
      mutate( CIl = round(Estimate-1.96*Std..Error,2),
              CIu = round(Estimate+1.96*Std..Error,2),
              Estimate = round(Estimate, 2),
              Variable = row.names(.),
              Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
      select(Variable, Result) %>%
      filter(!Variable =="(Intercept)")
    
    return(res_un)
    
  }
  
  else if(varExp=="adjusted") {
    
    formula<-as.formula(paste("weight ~ boy+parity+Gest_group+birth_month+matage+ married+city +insurance+",eval(substitute(trimester))))
    res_ad <- data.frame(summary(lm(formula , data=datared))$coefficients) %>%
      mutate( CIl = round(Estimate-1.96*Std..Error,2),
              CIu = round(Estimate+1.96*Std..Error,2),
              Estimate = round(Estimate, 2),
              Variable = row.names(.),
              Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
      select(Variable, Result) %>%
      filter(!Variable =="(Intercept)")
    
    return(res_ad)
    
  }
  
}



function_regression_trimester(trimester="Flu_intensity_first", varExp="unadjusted")
function_regression_trimester(trimester="Flu_intensity_first", varExp="adjusted")

function_regression_trimester(trimester="Flu_intensity_second", varExp="unadjusted")            
function_regression_trimester(trimester="Flu_intensity_second", varExp="adjusted")

function_regression_trimester(trimester="Flu_intensity_third", varExp="unadjusted")            
function_regression_trimester(trimester="Flu_intensity_third", varExp="adjusted")

# Low birth weight

function_regression_lbw_trimester <- function(trimester, varExp) {
  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    mutate(lbw = ifelse(weight < 2500, 1, 0),
           birth_month = as.factor(birth_month)) %>%
    filter(stillborn==0) %>%
    droplevels %>%
    mutate(year = as.character(year))
  
  
  # data
  if(varExp== "unadjusted") {
    
    formula<-as.formula( paste("lbw ~",eval(substitute(trimester))))
    res_un <- data.frame(summary(glm(formula , data=datared, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_un)
    
  }
  
  else if (varExp=="adjusted"){
    
    formula<-as.formula( paste("lbw ~ boy+parity+birth_month+matage+married+city+ insurance+",eval(substitute(trimester))))
    res_ad <- data.frame(summary(glm(formula , data=datared, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_ad)
    
  }
  
}

function_regression_lbw_trimester(trimester="Flu_intensity_first", varExp="unadjusted")
function_regression_lbw_trimester(trimester="Flu_intensity_first", varExp="adjusted")

function_regression_lbw_trimester(trimester="Flu_intensity_second", varExp="unadjusted")            
function_regression_lbw_trimester(trimester="Flu_intensity_second", varExp="adjusted")

function_regression_lbw_trimester(trimester="Flu_intensity_third", varExp="unadjusted")            
function_regression_lbw_trimester(trimester="Flu_intensity_third", varExp="adjusted")

# Stillborn

function_regression_stillborn_trimester <- function(trimester, varExp) {
  
  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    droplevels %>%
    mutate(year = as.character(year),
           Exposure_sum=as.factor(Exposure_sum)) %>%
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
  
  
  if (varExp=="unadjusted") {
    formula<-as.formula( paste("stillborn ~ " ,eval(substitute(trimester))))
    res_un <- data.frame(summary(glm(formula , data=datared, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_un)
  }
  
  
  else if (varExp=="adjusted"){
    
    formula<-as.formula(paste("stillborn~boy+parity+birth_month+city+matage+ married+ insurance+Gest_group+",eval(substitute(trimester))))
    res_ad <- data.frame(summary(glm(formula , data=datared, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_ad)
  }
  
}


function_regression_stillborn_trimester(trimester="Flu_intensity_first", varExp="unadjusted")
function_regression_stillborn_trimester(trimester="Flu_intensity_first", varExp="adjusted")

function_regression_stillborn_trimester(trimester="Flu_intensity_second", varExp="unadjusted")            
function_regression_stillborn_trimester(trimester="Flu_intensity_second", varExp="adjusted")

function_regression_stillborn_trimester(trimester="Flu_intensity_third", varExp="unadjusted")            
function_regression_stillborn_trimester(trimester="Flu_intensity_third", varExp="adjusted")


# Gestational age
function_regression_gestage_trimester <- function(trimester, varExp) {
  
  datared_still_n <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    droplevels %>%
    mutate(year = as.character(year),
           Gest_group = as.character(Gest_group),
           Gest_group=replace(Gest_group, Gest_group=="normal", "0"),
           Gest_group=replace(Gest_group, Gest_group=="early", "1"),
           Gest_group = as.factor(Gest_group),
           Exposure_sum=as.factor(Exposure_sum),
           birth_month = as.factor(birth_month))%>%
    filter(stillborn=="0") %>%
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
  
  if(varExp=="unadjusted") {
    
    formula<-as.formula(paste("Gest_group~",eval(substitute(trimester))))
    res_un <- data.frame(summary(glm(formula , data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_un)
    
  }
  
  else if (varExp=="adjusted"){
    
    formula<-as.formula(paste("Gest_group~boy+parity+birth_month+city+matage+ married+ insurance+",eval(substitute(trimester))))
    res_ad <- data.frame(summary(glm(formula , data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
      mutate(Est = exp(Estimate),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             Est = round(Est, 2),
             Fac = row.names(.),
             Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
      filter(!Fac =="(Intercept)") %>%
      select(Fac,  Result)
    
    return(res_ad)
  }
  
}

function_regression_gestage_trimester(trimester="Flu_intensity_first", varExp="unadjusted")
function_regression_gestage_trimester(trimester="Flu_intensity_first", varExp="adjusted")

function_regression_gestage_trimester(trimester="Flu_intensity_second", varExp="unadjusted")            
function_regression_gestage_trimester(trimester="Flu_intensity_second", varExp="adjusted")

function_regression_gestage_trimester(trimester="Flu_intensity_third", varExp="unadjusted")            
function_regression_gestage_trimester(trimester="Flu_intensity_third", varExp="adjusted")

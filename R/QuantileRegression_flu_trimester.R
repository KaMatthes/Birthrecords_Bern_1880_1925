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

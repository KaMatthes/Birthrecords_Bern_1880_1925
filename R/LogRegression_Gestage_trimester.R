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
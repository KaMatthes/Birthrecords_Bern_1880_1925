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

  
 
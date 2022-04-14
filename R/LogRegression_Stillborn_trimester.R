function_regression_stillborn_trimester <- function(varExp) {

  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    droplevels %>%
    mutate(year = as.character(year),
           Exposure_sum=as.factor(Exposure_sum)) %>%
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
  
  

formula<-as.formula( paste("stillborn ~ " ,eval(substitute(varExp))))
summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))

}

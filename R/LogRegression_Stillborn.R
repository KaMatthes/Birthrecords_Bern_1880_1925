function_regression_stillborn <- function(varExp) {

  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    droplevels %>%
    mutate(year = as.character(year),
           Exposure_sum=as.factor(Exposure_sum))
  
  
# data
if(varExp== "unadjusted") {

formula<-as.formula( paste("stillborn ~  year"))
summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
# plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "unadjusted_plot") {
   
    formula<-as.formula( paste("stillborn ~  year"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn, title = paste("stillborn"))
  }
  
  else if(varExp== "unadjusted_Exp") {

    formula<-as.formula( paste("stillborn ~  Exposure_sum"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "unadjusted_Exp_plot") {

    formula<-as.formula( paste("stillborn ~  Exposure_sum"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  
  else if(varExp== "unadjusted_Int") {
    
    formula<-as.formula( paste("stillborn ~ Flu_intensity_all"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "unadjusted_Int_plot") {
    
    formula<-as.formula( paste("stillborn ~  Flu_intensity_all"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  
  
  else if(varExp== "adjusted_year") {
    
    formula<-as.formula( paste("stillborn ~  year+boy+parity+birth_month+matage+married+city"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_year_plot") {

    formula<-as.formula( paste("stillborn ~ year+ boy+parity+birth_month+matage+married+city"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  

  else if(varExp== "adjusted_Exp") {
    
    formula<-as.formula( paste("stillborn ~  boy+parity+birth_month+matage+married+city+Exposure_sum"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_Exp_plot") {
    
    formula<-as.formula( paste("stillborn ~  boy+parity+birth_month+matage+married+city+Exposure_sum"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  
  else if(varExp== "adjusted_Int") {
    
    formula<-as.formula( paste("stillborn ~  boy+parity+birth_month+matage+married+city+Flu_intensity_all"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_Int_plot") {
    
    formula<-as.formula( paste("stillborn ~  boy+parity+birth_month+matage+married+city+Flu_intensity_all"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  

}

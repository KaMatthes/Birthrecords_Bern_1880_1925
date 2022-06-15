function_regression_lbw <- function(varExp) {

  
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

formula<-as.formula( paste("lbw ~  year"))
summary(mod.lbw <- glm(formula , data=datared, binomial(link = "logit")))




# plot_model(mod.lbw, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "unadjusted_plot") {
   
    formula<-as.formula( paste("lbw ~  year"))
    mod.lbw <- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.lbw, title = paste("Low birth weight < 2500 g"))
  }
  
  else if(varExp== "unadjusted_Int") {
    
    formula<-as.formula( paste("lbw ~ Flu_intensity_all"))
    summary(mod.lbw<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "unadjusted_Int_plot") {
    
    formula<-as.formula( paste("lbw ~  Flu_intensity_all"))
    mod.lbw<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.lbw)
  }
  
  
  else if(varExp== "adjusted") {
    
    formula<-as.formula( paste("lbw ~  year+boy+parity+birth_month+matage+married+city+ insurance"))
    summary(mod.lbw<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_plot") {

    formula<-as.formula( paste("lbw ~ year+ boy+parity+birth_month+matage+married+city+ insurance"))
    mod.lbw<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.lbw)
  }
  
  
  else if(varExp== "adjusted_Int") {
    
    formula<-as.formula( paste("lbw ~  boy+parity+birth_month+matage+married+city+ insurance+Flu_intensity_all"))
    summary(mod.lbw <- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_Int_plot") {
    
    formula<-as.formula( paste("lbw ~  boy+parity+birth_month+matage+married+city+ insurance+Flu_intensity_all"))
    mod.lbw<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.lbw)
  }

}

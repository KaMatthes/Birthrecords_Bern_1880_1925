function_regression_stillborn_1880 <- function(varExp) {

  
  datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year)),
           birth_month = as.factor(birth_month)) %>%
    filter(year_num<1902) %>%
    mutate(occupation2 = dplyr::recode(occupation2,
                                "5" ="7",
                                "6" = "7")) %>%
    droplevels 
  
  
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

  
  
  else if(varExp== "adjusted_year") {
    
    formula<-as.formula( paste("stillborn ~  year+boy+parity+Gest_group+birth_month+matheight2+malnutrition2+
              matbody2+occupation2+matage+agemenarche +city"))
    summary(mod.stillborn<- glm(formula , data=datared, binomial(link = "logit")))
    # plot_model(mod.stillborn, title = paste("stillborn",eval(substitute(varExp))))
  }
  
  else if(varExp== "adjusted_year_plot") {

    formula<-as.formula( paste("stillborn ~year+boy+parity+Gest_group+birth_month+matheight2+malnutrition2+
              matbody2+occupation2+matage+agemenarche +city"))
    mod.stillborn<- glm(formula , data=datared, binomial(link = "logit"))
    plot_model(mod.stillborn)
  }
  
}

function_regression_gestage_flu <- function(varExp, stillbornVar) {

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


# datared_still_y <- used.data %>%
#    mutate(year_num = as.integer(as.character(year))) %>%
#    filter(year_num >1913) %>%
#    droplevels %>%
#    mutate(year = as.character(year),
#           Gest_group = as.character(Gest_group),
#           Gest_group=replace(Gest_group, Gest_group=="normal", "0"),
#           Gest_group=replace(Gest_group, Gest_group=="early", "1"),
#           Gest_group = as.factor(Gest_group),
#           Exposure_sum=as.factor(Exposure_sum),
#           birth_month = as.factor(birth_month))%>%
#    droplevels

    
# data
if(varExp== "unadjusted" ) {
    
    
    formula<-as.formula( paste("Gest_group ~ year"))
    summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
    # plot_model(mod.Gest, title =paste("Gastational age"))
    # 
}

   else if(varExp== "unadjusted_plot") {
     
     formula<-as.formula( paste("Gest_group ~ year"))
     mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
     plot_model(mod.Gest)
     
   }
   # else if (varExp== "unadjusted_Exp")  {
   #   
   #   formula<-as.formula( paste("Gest_group ~ Exposure_sum"))
   #   summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
   #   # plot_model(mod.Gest, title =paste("Gastational age"))
   #   
   # }
   # 
   # else if (varExp== "unadjusted_Exp_plot")  {
   #   
   #   formula<-as.formula( paste("Gest_group ~ Exposure_sum"))
   #   mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
   #   plot_model(mod.Gest)
   #   
   # }

# else if (varExp== "unadjusted_Exp"  & stillbornVar=="yes")  {
# 
#    formula<-as.formula( paste("Gest_group ~ Flu_intensity_all"))
#    summary(mod.Gest <- glm(formula , data=datared_still_y, binomial(link = "logit")))
#    # plot_model(mod.Gest, title =paste("Gastational age"))
# 
# }

else if (varExp== "unadjusted_Int" )  {
   
      
   formula<-as.formula( paste("Gest_group ~ Flu_intensity_all"))
   summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
   # plot_model(mod.Gest, title =paste("Gastational age"))
   
}

else if (varExp== "unadjusted_Int_plot")  {
   
   formula<-as.formula( paste("Gest_group ~ Flu_intensity_all"))
   mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
   plot_model(mod.Gest)
   
}


   # else if (varExp== "adjusted" )  {
   # 
   #   
   #   formula<-as.formula( paste("Gest_group ~ year +boy+parity+birth_month+city+matage+ married+ insurance+Exposure_sum"))
   #   summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
   #   # plot_model(mod.Gest, title =paste("Gastational age"))
   #   
   # }
   # 
   # else if (varExp== "adjusted_plot")  {
   #    
   #   formula<-as.formula( paste("Gest_group ~ year +boy+parity+birth_month+city+matage+ married+ insurance+Exposure_sum"))
   #   mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
   #   plot_model(mod.Gest)
   #   
   # }
 
 # else if (varExp== "adjusted"  & stillbornVar=="yes")  {
 #    
 #    
 #    formula<-as.formula( paste("Gest_group ~ year +boy+parity+birth_month+city+matage+ married+Exposure_sum"))
 #    summary(mod.Gest <- glm(formula , data=datared_still_y, binomial(link = "logit")))
 #    # plot_model(mod.Gest, title =paste("Gastational age"))
 #    
 # }

else if (varExp== "adjusted_year" )  {
   
   
   formula<-as.formula( paste("Gest_group ~ year +boy+parity+birth_month+city+matage+ married+ insurance"))
   summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
   # plot_model(mod.Gest, title =paste("Gastational age"))
   
}

else if (varExp== "adjusted_year_plot")  {
   
   formula<-as.formula( paste("Gest_group ~ year +boy+parity+birth_month+city+matage+ married+ insurance"))
   mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
   plot_model(mod.Gest)
   
}

# else if (varExp== "adjusted_Exp")  {
#    
#    
#    formula<-as.formula( paste("Gest_group ~ boy+parity+birth_month+city+matage+ married + insurance+ Exposure_sum"))
#    summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
#    # plot_model(mod.Gest, title =paste("Gastational age"))
#    
# }

# else if (varExp== "adjusted_Exp_plot")  {
#    
#    formula<-as.formula( paste("Gest_group ~ boy+parity+birth_month+city+matage+ married  + insurance+ Exposure_sum"))
#    mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
#    plot_model(mod.Gest)
#    
# }

else if (varExp== "adjusted_Int")  {
   
   
   formula<-as.formula( paste("Gest_group ~ boy+parity+birth_month+city+matage+ married  + insurance+ Flu_intensity_all"))
   summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
   # plot_model(mod.Gest, title =paste("Gastational age"))
   
}

else if (varExp== "adjusted_Int_plot")  {
   
   formula<-as.formula( paste("Gest_group ~ boy+parity+birth_month+city+matage+ married  + insurance+Flu_intensity_all"))
   mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit"))
   plot_model(mod.Gest)
   
}



}

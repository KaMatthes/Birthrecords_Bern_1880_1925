function_regression_gestage_1880 <- function(varExp, stillbornVar) {

datared_still <- used.data %>%
       mutate(year_num = as.integer(as.character(year))) %>%
       filter(year_num <1902) %>%
       droplevels %>%
       mutate(year = as.character(year),
              Gest_group = as.character(Gest_group),
              Gest_group=replace(Gest_group, Gest_group=="normal", "0"),
              Gest_group=replace(Gest_group, Gest_group=="early", "1"),
              Gest_group = as.factor(Gest_group),
              birth_month = as.factor(birth_month),
              occupation2 = dplyr::recode(occupation2,
                                                 "5" ="7",
                                                 "6" = "7"),
              occupation2 = levels( occupation2 = )) %>%
       filter(stillborn=="0") %>%
   
       droplevels

    
# data
if(varExp== "unadjusted" ) {
    
    
    formula<-as.formula( paste("Gest_group ~ year"))
    summary(mod.Gest <- glm(formula , data=datared_still, binomial(link = "logit")))
    # plot_model(mod.Gest, title =paste("Gastational age"))
    # 
}

   else if(varExp== "unadjusted_plot") {
     
     formula<-as.formula( paste("Gest_group ~ year"))
     mod.Gest <- glm(formula , data=datared_still, binomial(link = "logit"))
     plot_model(mod.Gest)
     
   }
   

   else if (varExp== "adjusted" )  {
   
     
     formula<-as.formula( paste("Gest_group ~  year+boy+parity+birth_month+matheight2+malnutrition2+
              matbody2+occupation2+matage+agemenarche +city"))
     summary(mod.Gest <- glm(formula , data=datared_still, binomial(link = "logit")))
     # plot_model(mod.Gest, title =paste("Gastational age"))
     
   }

   else if (varExp== "adjusted_plot")  {
      
     formula<-as.formula( paste("Gest_group ~ year+boy+parity+birth_month+matheight2+malnutrition2+
              matbody2+occupation2+matage+agemenarche +city"))
     mod.Gest <- glm(formula , data=datared_still, binomial(link = "logit"))
     plot_model(mod.Gest)
     
   }
 
}
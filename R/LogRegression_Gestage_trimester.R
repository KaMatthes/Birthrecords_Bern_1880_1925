function_regression_gestage_trimester <- function(varExp) {

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


formula<-as.formula(paste("Gest_group~",eval(substitute(varExp))))

    summary(mod.Gest <- glm(formula , data=datared_still_n, binomial(link = "logit")))
    
}
            
            
function_quantile_regression_trimester <- function(varExp) {
  
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
           birth_month = as.factor(birth_month),
           occupation2 = dplyr::recode(occupation2,
                                       "5" ="7",
                                       "6" = "7")) %>%
    mutate(birthday2 = dmy(birthday2))%>%
    filter(birthday2 <= ymd("1920-01-31")) %>%
    droplevels
    
    formula<-as.formula(paste("weight ~",eval(substitute(varExp))))
    qr1 <- lm(formula, data=datared)
    summary(qr1)
}
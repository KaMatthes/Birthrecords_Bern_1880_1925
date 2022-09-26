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
    droplevels
  
# unadjusted

mod_year <- lm(weight ~ year , data=datared)
mod_year_sum <- summary(mod_year)
mod_year_t <- data.frame(mod_year_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")


mod_boy <- lm(weight ~ boy , data=datared)
mod_boy_sum <- summary(mod_boy)
mod_boy_t <- data.frame(mod_boy_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")


mod_parity <- lm(weight ~ parity , data=datared)
mod_parity_sum <- summary(mod_parity)
mod_parity_t <- data.frame(mod_parity_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

mod_gest <- lm(weight ~ Gest_group , data=datared)
mod_gest_sum <- summary(mod_gest)
mod_gest_t <- data.frame(mod_gest_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

mod_birth_month <- lm(weight ~ birth_month , data=datared)
mod_birth_month_sum <- summary(mod_birth_month)
mod_birth_month_sum_t <- data.frame(mod_birth_month_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")


mod_matage <- lm(weight ~ matage , data=datared)
mod_matage_sum <- summary(mod_matage)
mod_matage_t <- data.frame(mod_matage_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

mod_married <- lm(weight ~ married , data=datared)
mod_married_sum <- summary(mod_married)
mod_married_t <- data.frame(mod_married_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

mod_city <- lm(weight ~ city , data=datared)
mod_city_sum <- summary(mod_city )
mod_city_t <- data.frame(mod_city_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

mod_insurance <- lm(weight ~ insurance , data=datared)
mod_insurance_sum <- summary(mod_insurance)
mod_insurance_t <- data.frame(mod_insurance_sum$coefficients) %>%
      mutate( CIl = round(Estimate-1.96*Std..Error,2),
              CIu = round(Estimate+1.96*Std..Error,2),
              Estimate = round(Estimate, 2),
              Variable = row.names(.),
              Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
      select(Variable, Result) %>%
      filter(!Variable =="(Intercept)")
    
    
results_unadjusted_modern <- rbind(mod_year_t, mod_boy_t,  mod_parity_t, mod_gest_t,  mod_birth_month_t,
                                mod_matage_t, mod_married_t,  mod_city_t,  mod_insurance_t)
    
write.xlsx(results_unadjusted_modern,file=paste0("output/results_unadjusted_1914_1922.xlsx"),row.names=FALSE)

# adjusted

mod_adjusted <- lm(weight ~ year + boy+parity+Gest_group+birth_month+matage+ married+city +insurance, data=datared)
mod_adjusted_sum <- summary(mod_adjusted)
mod_adjusted_t <- data.frame(mod_adjusted_sum$coefficients) %>%
      mutate( CIl = round(Estimate-1.96*Std..Error,2),
              CIu = round(Estimate+1.96*Std..Error,2),
              Estimate = round(Estimate, 2),
              Variable = row.names(.),
              Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
      select(Variable, Result) %>%
      filter(!Variable =="(Intercept)")
  
write.xlsx(mod_adjusted_t,file=paste0("output/results_adjusted_1914_1922.xlsx"),row.names=FALSE)
 
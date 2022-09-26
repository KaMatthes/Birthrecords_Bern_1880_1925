
# data
datared <- used.data %>%
  mutate(year_num = as.integer(as.character(year))) %>%
  filter(year_num<1902) %>%
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
mod_birth_month_t <- data.frame(mod_birth_month_sum $coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")

mod_matheight <- lm(weight ~ matheight2 , data=datared)
mod_matheight_sum <- summary(mod_matheight)
mod_matheight_t <- data.frame(mod_matheight_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")
  
mod_malnutrition <- lm(weight ~ malnutrition2 , data=datared)
mod_malnutrition_sum <- summary(mod_malnutrition)
mod_malnutrition_t <- data.frame(mod_malnutrition_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")
  

mod_matbody <- lm(weight ~ matbody2, data=datared)
mod_matbody_sum <- summary(mod_matbody)
mod_matbody_t <- data.frame(mod_matbody_sum $coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")

mod_occupation <- lm(weight ~ occupation2 , data=datared)
mod_occupation_sum <- summary(mod_occupation)
mod_occupation_t <- data.frame(mod_occupation_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")


mod_matage <- lm(weight ~ matage, data=datared)
mod_matage_sum <- summary(mod_matage)
mod_matage_t <- data.frame(mod_matage_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")

mod_agemenarche <- lm(weight ~ agemenarche , data=datared)
mod_agemenarche_sum <- summary(mod_agemenarche)
mod_agemenarche_t <- data.frame(mod_agemenarche_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")

mod_city <- lm(weight ~ city , data=datared)
mod_city_sum <- summary(mod_city)
mod_city_t <- data.frame(mod_city_sum $coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")

results_unadjusted <- rbind(mod_year_t,mod_boy_t, mod_parity_t, mod_gest_t,  mod_birth_month_t,
                            mod_matheight_t,  mod_malnutrition_t, mod_matbody_t,mod_occupation_t,
                            mod_matage_t, mod_agemenarche_t,mod_city_t)

# 
write.xlsx(results_unadjusted,file=paste0("output/results_unadjusted_1880_1900.xlsx"),row.names=FALSE)

# adjusted

mod_adj_partity <- lm(weight ~ year+boy+parity+Gest_group+birth_month+matheight2+malnutrition2+
            matbody2+matage+agemenarche +city, data=datared)
mod_adj_partity_sum <- summary(mod_adj_partity)
mod_adj_partity_t <- data.frame(mod_adj_partity_sum$coefficients) %>%
  mutate( CIl = round(Estimate-1.96*Std..Error,2),
          CIu = round(Estimate+1.96*Std..Error,2),
          Estimate = round(Estimate, 2),
          Variable = row.names(.),
          Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
  select(Variable, Result) %>%
  filter(!Variable =="(Intercept)")

write.xlsx(mod_adj_partity_t ,file=paste0("output/results_adjusted_parity_1880_1900.xlsx"),row.names=FALSE)
  
  
mod_adj_occupation <- lm(weight ~ year+boy+occupation2+Gest_group+birth_month+matheight2+malnutrition2+
              matbody2+matage+agemenarche +city, data=datared)
mod_adj_occupation_sum <- summary(mod_adj_occupation)
mod_adj_occupation_t <- data.frame(mod_adj_occupation_sum$coefficients) %>%
    mutate( CIl = round(Estimate-1.96*Std..Error,2),
            CIu = round(Estimate+1.96*Std..Error,2),
            Estimate = round(Estimate, 2),
            Variable = row.names(.),
            Result = paste0(Estimate," (", CIl, " - ", CIu, ")")) %>%
    select(Variable, Result) %>%
    filter(!Variable =="(Intercept)")
  
write.xlsx(mod_adj_occupation_t,file=paste0("output/results_adjusted_occupation_1880_1900.xlsx"),row.names=FALSE)

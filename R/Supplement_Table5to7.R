datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    mutate(lbw = ifelse(weight < 2500, 1, 0),
           birth_month = as.factor(birth_month)) %>%
    filter(stillborn==0) %>%
    droplevels %>%
    mutate(year = as.character(year))
  

mod.lbw.year.un <- data.frame(summary(glm(lbw ~  year , data=datared, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)
  
mod.lbw.flu.un <- data.frame(summary(glm(lbw ~   Flu_intensity_all , data=datared, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

results_lbw_un <- rbind(mod.lbw.year.un,mod.lbw.flu.un)

write.xlsx(results_lbw_un ,file=paste0("output/results_lbw_un.xlsx"),row.names=FALSE)


mod.lbw.year.an <- data.frame(summary(glm(lbw ~  year+boy+parity+birth_month+matage+married+city+ insurance , data=datared, binomial(link = "logit")))$coefficients)[2:28,] %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

write.xlsx(mod.lbw.year.an,file=paste0("output/results_lbw_year_ad.xlsx"),row.names=FALSE)


mod.lbw.flu.an <- data.frame(summary(glm(lbw ~  Flu_intensity_all+  boy+parity+birth_month+matage+married+city+ insurance , data=datared, binomial(link = "logit")))$coefficients)[2:20,] %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

write.xlsx(mod.lbw.flu.an,file=paste0("output/results_lbw_flu_ad.xlsx"),row.names=FALSE)


datared_s <- used.data %>%
  mutate(year_num = as.integer(as.character(year)),
         birth_month = as.factor(birth_month)) %>%
  filter(year_num >1913) %>%
  droplevels %>%
  mutate(year = as.character(year))


mod.stillborn.year.un <- data.frame(summary(glm(stillborn ~  year, data=datared_s, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)


mod.stillborn.flu.un <- data.frame(summary(glm(stillborn ~ Flu_intensity_all , data=datared_s, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

results_stillborn_un <- rbind(mod.stillborn.year.un , mod.stillborn.flu.un)

write.xlsx(results_stillborn_un ,file=paste0("output/_stillborn_un.xlsx"),row.names=FALSE)
         


mod.stillborn.year.an <- data.frame(summary(glm(stillborn ~  year+boy+parity+birth_month+matage+married+city+ insurance +Gest_group, data=datared_s, binomial(link = "logit")))$coefficients)[2:29,] %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)



write.xlsx(mod.stillborn.year.an,file=paste0("output/mod.stillborn.year.an.xlsx"),row.names=FALSE)


mod.stillborn.flu.an <- data.frame(summary(glm(stillborn ~  Flu_intensity_all +boy+parity+birth_month+matage+married+city+ insurance+Gest_group, data=datared_s, binomial(link = "logit")))$coefficients)[2:29,] %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)


write.xlsx(mod.stillborn.flu.an ,file=paste0("output/mod.stillborn.flu.an.xlsx"),row.names=FALSE)


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
  droplevels


mod.gest.year.un <- data.frame(summary(glm(Gest_group ~ year, data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

mod.gest.flu.un <- data.frame(summary(glm(Gest_group ~ Flu_intensity_all , data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)


results_gest_un <- rbind(mod.gest.year.un,mod.gest.flu.un)
write.xlsx(results_gest_un ,file=paste0("output/results_gest_un.xlsx"),row.names=FALSE)

mod.gest.year.an <- data.frame(summary(glm(Gest_group ~  year+boy+parity+birth_month+matage+married+city+ insurance , data=datared_still_n, binomial(link = "logit")))$coefficients)[2:29,] %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)

write.xlsx(mod.gest.year.an  ,file=paste0("output/mod.gest.year.an.xlsx"),row.names=FALSE)

mod.gest.flu.an <- data.frame(summary(glm(Gest_group~  Flu_intensity_all +boy+parity+birth_month+matage+married+city+ insurance, data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Est = round(Est, 2),
         Fac = row.names(.),
         Result = paste0(Est," (", CIl, " - ", CIu, ")")) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac,  Result)


write.xlsx(mod.gest.flu.an ,file=paste0("output/mod.gest.flu.an.xlsx"),row.names=FALSE)
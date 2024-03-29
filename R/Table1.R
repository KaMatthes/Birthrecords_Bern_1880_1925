data_table <- read.csv(paste0("data_raw/",data.bern), header=TRUE, sep=";", fileEncoding="UTF-8-BOM") %>%
  filter(multiple==0) %>%
  filter(!(gest <30)) %>%
  filter(!(weight < 1000)) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage < 14)) %>% 
  select(year, insurance, matage, married,parity, gest, birthday2, boy, stillborn, multiple, weight, matheight2, matbody2,
         malnutrition2, occupation2, agemenarche, city)%>%
  mutate(
         birth_isoweek = isoweek(dmy(birthday2)),
         birth_month = month(dmy(birthday2)),
         birth_weekday = wday(dmy(birthday2),week_start = 1),
         birth_weekday2 = weekdays(dmy(birthday2)),
         birth_season = as.character(cut(birth_month, breaks=c(1,3,6,9,12),include.lowest = TRUE)),
         birth_season  = replace(birth_season, birth_season=="[1,3]", "Winter" ),
         birth_season  = replace(birth_season, birth_season=="(3,6]", "Spring" ),
         birth_season  = replace(birth_season, birth_season=="(6,9]", "Summer" ),
         birth_season  = replace(birth_season, birth_season=="(9,12]", "Fall" ),
         birth_season = as.factor(birth_season),
         birth_season = factor(birth_season, levels = c("Spring","Summer","Fall","Winter")),
         Gest_group = floor(gest),
         # Gest_group  = replace(Gest_group ,Gest_group>42, "late" ),
         Gest_group  = replace(Gest_group ,Gest_group>=38, "normal" ),
         Gest_group  = replace(Gest_group ,Gest_group<38, "early" ),
         Gest_group = as.factor(Gest_group),
         parity = ifelse(parity>3, 4, parity),
         parity = as.factor(parity),
         parity = ifelse(parity=="4",">=4", parity),
         parity = factor(parity, levels = c("1","2","3",">=4")),
         boy = as.factor(boy),
         stillborn = as.factor(stillborn),
         city = as.factor(city),
         insurance = as.factor(insurance),
         married = as.factor(married),
         matbody2 = as.factor(matbody2),
         occupation2 = as.factor(occupation2),
         matheight2  = as.factor(matheight2 ),
         malnutrition2 = as.factor(malnutrition2),
         Gest_group = factor(Gest_group, levels=c("normal","early")),
         parity = factor(parity, levels=c("1","2","3",">=4")),
         city = factor(city, levels=c("1","0")),
         married = factor(married, levels=c("1","0")),
         boy = factor(boy, levels=c("1","0")),
         birth_season = factor( birth_season, levels=c("Spring","Summer","Fall","Winter")),
         parity = factor(parity, levels = c("1","2","3",">=4")),
         matbody2 = factor(matbody2, levels=c("2","1","3")),
         matheight2 = factor(matheight2, levels=c("2","1","3")),
         malnutrition2 = factor(malnutrition2, levels=c("0","1")),
         occupation2 = factor(occupation2, levels=c("4","1","2","3","5","6","7")))


tab1 <- tableby(year ~ weight + boy + parity + matage+stillborn+ Gest_group+city+insurance+married
                + Gest_group+matheight2+malnutrition2+matbody2+
                  occupation2+agemenarche, data=data_plot,test=FALSE)
summary(tab1)

tab_des <- summary(tab1,text=TRUE)
tab_des <- as.data.frame(tab_des)
table2excel(tab_des, file = "output/Table1.xlsx")

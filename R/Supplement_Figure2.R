####### Density ##########

data_density <- read.csv(paste0("data_raw/",data.bern), header=TRUE, sep=";") %>%
  filter(multiple==0) %>%
  filter(!(gest <30)) %>%
  filter(!(weight < 1500)) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage <14)) %>%
  filter(!year=="1901") %>%
  filter(!year=="1902") %>%
  filter(!year=="1903") %>%
  filter(!year=="1911") %>%
  filter(!year=="1912") %>%
  filter(!year=="1913") %>%
  filter(!year=="1923") %>%
  select(year, insurance, matage, married,parity, gest, birthday2, boy, stillborn, multiple, weight, gestdummy2, matheight2, matbody2,
         malnutrition2, occupation2, agemenarche, coordinates, distance, city)%>%
  mutate(coordinates=ifelse(coordinates=="", NA, coordinates),
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


data_density_old <-  data_density  %>%
  filter(year < 1901) %>%
  mutate(year = as.factor(year))

density_plot_old <- ggplot() +
  geom_density(data = data_density_old, aes(x=weight, col=year), lwd=1) +
  geom_vline(xintercept=3000, col="grey")+
  xlim(c(1000,6000))+
  scale_color_manual("Year:",values =   mypalette4)+
  xlab("birth weight")+
  theme_bw()+
  theme(
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 15),
        plot.title = element_text(size=15),
        legend.position = "bottom")


data_density_new<-  data_density  %>%
  filter(year > 1901) %>%
  mutate(year = as.factor(year))

density_plot_new <- ggplot() +
  geom_density(data = data_density_new, aes(x=weight, col=year), lwd=1) +
  geom_vline(xintercept=3000, col="grey")+
  xlim(c(1000, 6000))+
  scale_color_manual("Year:",values =   mypalette3)+
  xlab("birth weight")+
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.position = "bottom")

Supplement2 <- cowplot::plot_grid(density_plot_old, density_plot_new,ncol=1,nrow=2, labels=c("A", "B"),
                                  label_size = 20)

cowplot::save_plot("output/Supplement2.pdf", Supplement1,base_height=15,base_width=15)



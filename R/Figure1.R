####### Birthweight ##########

# 1880 - 1900
data1880 <- databern %>%
  filter(multiple==0) %>%
  filter(!(weight < 1000)) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage <14)) %>%
  filter(!(gest <30)) %>%
  mutate(
    year = as.factor(year),
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
    occupation2 = factor(occupation2, levels=c("4","1","2","3","5","6","7")),
    Exposure_sum_dummy = as.factor(Exposure_sum_dummy),
    year_num = as.integer(as.character(year))) %>%
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

# 1880 - 1900 undadjusted   
mod1 <- lm(weight ~ year , data=data1880)
mod1_results <- as.data.frame(ggeffect(mod1, terms="year"))

mod1_plot <- ggplot(mod1_results, aes(x=x,y=predicted),position=pd) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high,col=x),position=pd,lwd=lwd_size)+
  ylim(c(3000,3300)) +
  labs(x="Year", y="mean birthweight in gram") +
  scale_color_manual("Year:",values =  mypalette7,
                     labels=c("1880","1885","1890","1895", "1900"))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.position = "none")


# 1880 - 1900 adjusted  
mod2 <- lm(weight ~ year+boy+parity+Gest_group+birth_month+matheight2+malnutrition2+
            matbody2+matage+agemenarche +city, data=data1880)

mod2_results <- as.data.frame(ggeffect(mod2, terms="year"))

mod2_plot<- ggplot(mod2_results, aes(x=x,y=predicted),position=pd) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high,col=x),position=pd,lwd=lwd_size)+
  ylim(c(3000,3300)) +
  labs(x="Year", y="adjusted mean birthweight in gram") +
  scale_color_manual("Year:",values =    mypalette7)+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.position = "none")

# 1914 - 1922
data1914 <- databern %>%
  filter(multiple==0) %>%
  filter(!(weight < 1000)) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage <14)) %>%
  filter(!(gest <30)) %>%
  mutate(
    year = as.factor(year),
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
    occupation2 = factor(occupation2, levels=c("4","1","2","3","5","6","7")),
    Exposure_sum_dummy = as.factor(Exposure_sum_dummy),
    year_num = as.integer(as.character(year))) %>%
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


# 1914-1922 unadjusted

mod3 <- lm(weight ~ year , data=data1914)

mod3_results <- as.data.frame(ggeffect(mod3, terms="year"))

mod3_plot <- ggplot(mod3_results, aes(x=x,y=predicted),position=pd) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high,col=x),position=pd,lwd=lwd_size)+
  ylim(c(3000,3300)) +
  labs(x="Year", y="mean birthweight in gram") +
  scale_color_manual("Year:",
                     values = mypalette7)+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.position = "none")
  
# 1914-1922 adjusted

mod4 <- lm(weight ~ year + boy+parity+Gest_group+birth_month+matage+ married+city +insurance, data=data1914)

mod4_results <- as.data.frame(ggeffect(mod4, terms="year"))

mod4_plot <- ggplot( mod4_results, aes(x=x,y=predicted),position=pd) + 
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high,col=x),position=pd,lwd=lwd_size)+
  ylim(c(3000,3300)) +
  labs(x="Year", y="adjusted mean birthweight in gram") +
  scale_color_manual("Year:",
                     values = mypalette7)+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.position = "none")



Figure1 <- cowplot::plot_grid(mod1_plot,mod2_plot,mod3_plot,mod4_plot, nrow=2, ncol=2, labels = c("A","B","C","D"),
                              label_size = 14)
cowplot::save_plot("output/Figure1.pdf", Figure1,base_height=15,base_width=15)



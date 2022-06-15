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
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_unadj")
  
mod.lbw.flu.un <- data.frame(summary(glm(lbw ~   Flu_intensity_all , data=datared, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_unadj")

mod.lbw.year.an <- data.frame(summary(glm(lbw ~  year+boy+parity+birth_month+matage+married+city+ insurance , data=datared, binomial(link = "logit")))$coefficients)[2:9,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_adj")

mod.lbw.flu.an <- data.frame(summary(glm(lbw ~  Flu_intensity_all+  boy+parity+birth_month+matage+married+city+ insurance , data=datared, binomial(link = "logit")))$coefficients)[2,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_adj")
 

data_year <- rbind(mod.lbw.year.un,mod.lbw.year.an) %>%
  arrange(Fac) %>%
  mutate(Var=factor(Var, levels=c("Year_unadj","Year_adj")))


Coeff_plot_year <- ggplot( data_year, aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Year", y="OR") +
  # ggtitle("Birth weight")+
  scale_color_manual(" ",
                     breaks=c("Year_unadj","Year_adj"),
                     labels=c("unadjusted","adjusted"),
                     values = c(mypalette7[2],mypalette7[1]))+
  scale_x_discrete( breaks=c("year1914","year1915","year1916","year1917","year1918","year1919","year1920","year1921", "year1922"),
                    labels=c("1914","1915","1916","1917", "1918", "1919","1920","1921","1922"))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.text=element_text(size=15),
        legend.position = "bottom") +
  coord_flip() 


data_flu <- rbind(mod.lbw.flu.un,mod.lbw.flu.an) 

Coeff_plot_flu <- ggplot( data_flu, aes(x=Fac,y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Flu intensity", y="OR") +
  # ggtitle("Birth weight")+
  scale_color_manual(" ",
                     breaks=c("Flu_unadj","Flu_adj"),
                     labels=c("unadjusted","adjusted"),
                     values = c(mypalette7[2],mypalette7[1]))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text.x=element_text(color="black",size= size_axis),
        axis.text.y=element_blank(),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=15),
        legend.text=element_text(size=15),
        legend.position = "bottom") +
  coord_flip() 


Coeff_plot <- cowplot::plot_grid(Coeff_plot_year, Coeff_plot_flu,ncol=2,nrow=1, labels=c("A", "B"),
                                  label_size = 20, align="hv")

cowplot::save_plot("output/Lbw_OR.pdf", Coeff_plot,base_height=10,base_width=20)


size_axis <- 15
size_axis_title <- 15
size_plot_title <-15
size_legend_text <- 20

datared <- used.data %>%
    mutate(year_num = as.integer(as.character(year))) %>%
    filter(year_num >1913) %>%
    droplevels %>%
    mutate(year = as.character(year),
           Exposure_sum=as.factor(Exposure_sum))
  
  
mod.stillborn.year.un <- data.frame(summary(glm(stillborn ~  year, data=datared, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_unadj")

mod.stillborn.flu.un <- data.frame(summary(glm(stillborn ~ Flu_intensity_all , data=datared, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_unadj")
  

mod.stillborn.year.an <- data.frame(summary(glm(stillborn ~  year+boy+parity+birth_month+matage+married+city+ insurance +Gest_group, data=datared, binomial(link = "logit")))$coefficients)[2:9,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_adj")


mod.stillborn.flu.an <- data.frame(summary(glm(stillborn ~  Flu_intensity_all +boy+parity+birth_month+matage+married+city+ insurance+Gest_group, data=datared, binomial(link = "logit")))$coefficients)[2,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_adj")


data_year <- rbind(mod.stillborn.year.un,mod.stillborn.year.an) %>%
  arrange(Fac) %>%
  mutate(Var=factor(Var, levels=c("Year_unadj","Year_adj")))


Coeff_plot_year <- ggplot( data_year, aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Year", y="OR") +
  ggtitle("Stillborn")+
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
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=15),
        legend.position = "none") +
  coord_flip() 


data_flu <- rbind(mod.stillborn.flu.un,mod.stillborn.flu.an) 

Coeff_plot_flu <- ggplot( data_flu, aes(x=Fac,y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Flu intensity", y="OR") +
  ggtitle("Stillborn")+
  scale_color_manual(" ",
                     breaks=c("Flu_unadj","Flu_adj"),
                     labels=c("unadjusted","adjusted"),
                     values = c(mypalette7[2],mypalette7[1]))+
  theme_bw()+
  theme(aspect.ratio=1,
        plot.margin=unit(c(1,1,1,0.8), "cm"),
        axis.text.x=element_text(color="black",size= size_axis),
        axis.text.y=element_blank(),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        axis.title.y.left = element_text(vjust=0.5),
        legend.text=element_text(size=15),
        legend.position = "none") +
  coord_flip() 

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
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_unadj")


mod.gest.flu.un <- data.frame(summary(glm(Gest_group ~ Flu_intensity_all , data=datared_still_n, binomial(link = "logit")))$coefficients) %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_unadj")


mod.gest.year.an <- data.frame(summary(glm(Gest_group ~  year+boy+parity+birth_month+matage+married+city+ insurance , data=datared_still_n, binomial(link = "logit")))$coefficients)[2:9,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  add_row(Fac = "year1914", Est =1, CIl=0, CIu=0) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Year_adj")


mod.gest.flu.an <- data.frame(summary(glm(Gest_group~  Flu_intensity_all +boy+parity+birth_month+matage+married+city+ insurance, data=datared_still_n, binomial(link = "logit")))$coefficients)[2,] %>%
  mutate(Est = exp(Estimate),
         CIl = exp(Estimate - 1.96*`Std..Error`),
         CIu = exp(Estimate + 1.96*`Std..Error`),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Flu_adj")


data_year_gest <- rbind(mod.gest.year.un,mod.gest.year.an) %>%
  arrange(Fac) %>%
  mutate(Var=factor(Var, levels=c("Year_unadj","Year_adj")))


Coeff_plot_year_gest <- ggplot( data_year_gest, aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Year", y="OR") +
  ggtitle("Gestational week")+
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
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=15),
        legend.position = "none") +
  coord_flip() 


data_flu_gest <- rbind(mod.gest.flu.un,mod.gest.flu.an) 

Coeff_plot_flu_gest <- ggplot( data_flu_gest, aes(x=Fac,y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Flu intensity", y="OR") +
  ggtitle("Gestational week")+
  scale_color_manual(" ",
                     breaks=c("Flu_unadj","Flu_adj"),
                     labels=c("unadjusted","adjusted"),
                     values = c(mypalette7[2],mypalette7[1]))+
  theme_bw()+
  theme(aspect.ratio=1,
        plot.margin=unit(c(1,1,1,0.8), "cm"),
        axis.text.x=element_text(color="black",size= size_axis),
        axis.text.y=element_blank(),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=15),
        axis.title.y.left = element_text(vjust=-0.5),
        legend.position = "none") +
  coord_flip() 


Plot_legend <- ggplot( data_flu_gest, aes(x=Fac,y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=1) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  labs(x="Flu intensity", y="OR") +
  ggtitle("Gestational week")+
  scale_color_manual(" ",
                     breaks=c("Flu_unadj","Flu_adj"),
                     labels=c("unadjusted","adjusted"),
                     values = c(mypalette7[2],mypalette7[1]))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text.x=element_text(color="black",size= size_axis),
        axis.text.y=element_blank(),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=size_legend_text),
        legend.position = "bottom") +
  coord_flip() 


Coeff_plot <- cowplot::plot_grid(Coeff_plot_year, Coeff_plot_flu,
                                 Coeff_plot_year_gest, Coeff_plot_flu_gest,
                                 ncol=2,nrow=2, labels=c("A", "B", "C", "D"),
                                 label_size = 20, align="hv")


Plotlegend <-get_legend(Plot_legend)


AllPlots <- cowplot::plot_grid(Coeff_plot,Plotlegend, nrow=2, rel_heights = c(1, .1))
                                

cowplot::save_plot("output/Stillborn_Gest_OR.pdf", AllPlots,base_height=15,base_width=15)

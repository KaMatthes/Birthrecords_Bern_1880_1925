data_flu <-  read.csv(paste0("data_raw/",data.bern), header=TRUE, sep=";",fileEncoding="UTF-8-BOM") %>%
  mutate(year_num = as.integer(as.character(year))) %>%
  filter(year_num >1913) %>%
  filter(!(gest <30)) %>%
  filter(multiple==0) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage <14)) %>%
  filter(!(weight <1000)) %>%
  mutate( gest = ifelse(gest >50, 50, gest),
         birthday2 = dmy(birthday2),
         Grippe = ifelse(is.na(Grippe), 0, Grippe),
         Grippe= as.integer(Grippe),
         Grippe = as.character(Grippe),
         Grippe = replace(Grippe, Grippe=="0", "no flu"),
         Grippe = replace(Grippe, Grippe=="1", "flu"),
         lbw = ifelse(weight < 2500, 1, 0))%>%
  filter(birthday2 >= ymd("1918-07-09") & birthday2 <= ymd("1919-02-02")) %>%
  dplyr::select(Grippe,lbw,weight, stillborn, gest) %>%
  droplevels

plot_flu_weight <- ggplot(data=data_flu ,aes(x=factor(Grippe),y=weight)) +
  geom_violin(aes(x=factor(Grippe),y=weight))+
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  geom_quasirandom(aes(x=factor(Grippe),y=weight,col=as.factor(stillborn), shape=as.factor(stillborn)),width = 0.4,size=3) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  annotate("text", x=0.60, y=1900, label= "stillborn =  8.11 %", size=5) + 
  annotate("text", x=0.66, y=1700, label= "lbw = 13.51 %", size=5) + 
  annotate("text", x=1.65, y=1900, label= "stillborn =  6.13 %", size=5) + 
  annotate("text", x=1.70, y=1700, label=  "lbw = 9.87 %", size=5) + 
  scale_color_manual("",
                     breaks=c("0","1"),
                     labels=c("alive", "stillborn"),
                     values=c(cbp1[1],"red"))+
  scale_shape_manual("",
                     breaks=c("0","1"),
                     labels=c("alive", "stillborn"),
                     values=c(16,17))+
  xlab("")+
  ylab("Birthweight in gram")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=15),
        legend.position = c(.1, .9),
        legend.key.size = unit(1.5, "cm"))


plot_flu_gest <- ggplot(data=data_flu ,aes(x=factor(Grippe),y=gest)) +
  geom_violin(aes(x=factor(Grippe),y=gest))+
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  geom_quasirandom(aes(x=factor(Grippe),y=gest,col=as.factor(stillborn), shape=as.factor(stillborn)),width = 0.4,size=3) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  annotate("text", x=0.66, y=35, label= "stillborn =  8.11 %", size=5) + 
  annotate("text", x=0.66, y=33, label= "preterm = 29.73 %", size=5) + 
  annotate("text", x=1.70,y=35, label= "stillborn =  6.13 %", size=5) + 
  annotate("text", x=1.70, y=33, label=  "preterm = 14.85 %", size=5) + 
  scale_color_manual("",
                     breaks=c("0","1"),
                     labels=c("alive", "stillborn"),
                     values=c(cbp1[1],"red"))+
  scale_shape_manual("",
                     breaks=c("0","1"),
                     labels=c("alive", "stillborn"),
                     values=c(16,17))+
  xlab("")+
  ylab("Gestational weeks")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=15),
        legend.position = c(.1, .9),
        legend.key.size = unit(1.5, "cm"))

Figure3 <- cowplot::plot_grid(plot_flu_weight, plot_flu_gest,ncol=2,nrow=1, labels=c("A", "B"),
                                  label_size = 20, align="v")


cowplot::save_plot("output/Figure3.pdf", Figure3,base_height=10,base_width=20)

  
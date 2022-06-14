function_plot_flu <- function(){
datared <-  databern %>%
    filter(multiple==0) %>%
    filter(!(matage > 50))  %>%
    filter(!(matage <14)) %>%
  filter(!(weight <1000)) %>%
  mutate(birthday2 = dmy(birthday2),
         Grippe = ifelse(is.na(Grippe), 0, Grippe),
         Grippe= as.integer(Grippe),
         Grippe = as.character(Grippe),
         Grippe = replace(Grippe, Grippe=="0", "no flu"),
         Grippe = replace(Grippe, Grippe=="1", "flu"))%>%
  filter(birthday2 >= ymd("1918-07-09") & birthday2 <= ymd("1919-02-02")) %>%
  mutate(year_num = as.integer(as.character(year))) %>%
  filter(year_num >1913) %>%
  mutate(birth_isoweek = sprintf("%02d",birth_isoweek),
         Year_week = paste0(year,"_",birth_isoweek),
         birth_isoweek_W = paste0("W",birth_isoweek),
         birthweek_year = paste0(year,"-", birth_isoweek_W,"-1"),
         birth_time = ISOweek2date( birthweek_year),
         Birth_year_week_num = as.numeric(birth_time),
         year = as.character(year),
         #birth_month = as.factor(birth_month),
       ) %>%
  dplyr::select(Grippe,boy,parity,gest,matage,married,city,weight, stillborn) %>%
  droplevels

plot_grippe_weight <- ggplot(data=datared,aes(x=factor(Grippe),y=weight)) +
  geom_violin(aes(x=factor(Grippe),y=weight))+
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  geom_quasirandom(aes(x=factor(Grippe),y=weight,col=as.factor(stillborn), shape=as.factor(stillborn)),width = 0.4,size=3) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  annotate("text", x=0.65, y=1900, label= "stillborn = 8.11 %", size=5) + 
  annotate("text", x=1.70, y=1900, label= "stillborn = 6.13 %", size=5) + 
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


plot_grippe_gest <- ggplot(data=datared,aes(x=factor(Grippe),y=gest)) +
  geom_violin(aes(x=factor(Grippe),y=gest))+
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  geom_quasirandom(aes(x=factor(Grippe),y=gest,col=as.factor(stillborn), shape=as.factor(stillborn)),width = 0.4,size=3) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  # annotate("text", x=0.65, y=1900, label= "stillborn = 6.13 %", size=5) + 
  # annotate("text", x=1.70, y=1900, label= "stillborn = 8.12 %", size=5) + 
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

Boxplot_grippe <- cowplot::plot_grid(plot_grippe_weight, plot_grippe_gest,ncol=2,nrow=1, labels=c("A", "B"),
                                  label_size = 20, align="v")


# cowplot::save_plot("output/Boxplot_grippe.pdf", Boxplot_grippe,base_height=10,base_width=20)

return(Boxplot_grippe)

}
  
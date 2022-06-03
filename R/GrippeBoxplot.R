function_plot_flu <- function(){
datared <-  databern %>%
    filter(multiple==0) %>%
    filter(!(matage > 50))  %>%
    filter(!(matage <14)) %>%
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
         Grippe = ifelse(is.na(Grippe), 0, Grippe),
         Grippe= as.integer(Grippe)) %>%
  mutate(birthday2 = dmy(birthday2),
         Grippe = as.character(Grippe),
         Grippe = replace(Grippe, Grippe=="0", "no flu"),
         Grippe = replace(Grippe, Grippe=="1", "flu"))%>%
  filter(birthday2 >= ymd("1918-07-09") & birthday2 <= ymd("1919-02-02")) %>%
  dplyr::select(Grippe,boy,parity,Gest_group,matage,married,city,weight) %>%
  droplevels

plot_grippe <- ggplot(data=datared,aes(x=factor(Grippe),y=weight)) +
  geom_violin()+
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") +
 
  geom_quasirandom(alpha = 0.2, width = 0.4,aes(x=factor(Grippe),y=weight))
  xlab("")+
  theme_bw()+
  theme(aspect.ratio=1,
        strip.text.x=element_text(size=strip_text),
        axis.text.x=element_text(color="black",size=10),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title =element_blank(),
        plot.title = element_text(size=15),
        legend.position = "none")

return(plot_grippe)

}
  
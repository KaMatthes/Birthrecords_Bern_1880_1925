function_flu_plot <- function() {
  
data_flu <- read.csv(paste0("../data/datagrippe.csv"), header=TRUE, sep=";") %>%
  arrange(year, KW) %>%
  filter(Number_weeks==12) %>%
  filter(!Canton==0) %>%
  mutate(ID = 1:dim(.)[1],
         ID = as.factor(ID))%>%
  filter(!week=="1918_w25")

data_flu16 <- read.csv(paste0("../data/datagrippe.csv"), header=TRUE, sep=";") %>%
  arrange(year, KW) %>%
  filter(Number_weeks==16) %>%
  filter(!Canton==0) %>%
  mutate(ID = 1:dim(.)[1],
         ID = as.factor(ID))%>%
  filter(!week=="1918_w25")

data_flu4 <- read.csv(paste0("../data/datagrippe.csv"), header=TRUE, sep=";") %>%
  arrange(year, KW) %>%
  filter(Number_weeks==4) %>%
  filter(!Canton==0) %>%
  mutate(ID = 1:dim(.)[1],
         ID = as.factor(ID))%>%
  filter(!week=="1918_w25")

  
flu_plot <- ggplot() +
  geom_bar(data = data_flu , aes(x=ID, y=Canton),stat="identity") +
  ylab("Cases")+
  xlab("")+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Flu Cases")+
  theme_bw()+
  theme(
        strip.text.x=element_text(size=10),
        axis.text=element_text(color="black",size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        plot.title = element_text(size=15,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        axis.title.x = element_text(vjust=10))
      
flu_rollsum_plot <- ggplot() +
  geom_bar(data = data_flu , aes(x=ID, y=Cases.rollsum),stat="identity") +
  ylab("Roll sum cases")+
  xlab("")+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Roll sum 12 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=10),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))


flu_intensity_plot <- ggplot() +
  geom_bar(data = data_flu , aes(x=ID, y= Cases.range),stat="identity") +
  ylab("Intensity")+
  xlab("")+
  ylim(c(0,1))+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Intensity 12 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=10),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))


flu_rollsum16_plot <- ggplot() +
  geom_bar(data = data_flu16 , aes(x=ID, y=Cases.rollsum),stat="identity") +
  ylab("Roll sum cases")+
  xlab("")+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Roll sum 16 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=10),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))

flu_intensity16_plot <- ggplot() +
  geom_bar(data = data_flu16 , aes(x=ID, y=Cases.range),stat="identity") +
  ylab("Intensity")+
  xlab("")+
  ylim(c(0,1))+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Intensity 16 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=10),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))


flu_rollsum4_plot <- ggplot() +
  geom_bar(data = data_flu4 , aes(x=ID, y=Cases.rollsum),stat="identity") +
  ylab("Roll sum cases")+
  xlab("")+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Roll sum 4 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=10),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))

flu_intensity4_plot <- ggplot() +
  geom_bar(data = data_flu , aes(x=ID, y=Cases.range),stat="identity") +
  ylab("Intensity")+
  ylim(c(0,1))+
  xlab("")+
  scale_x_discrete(labels=data_flu$week) +
  labs(title="Intensity 4 weeks")+
  theme_bw()+
  theme(
    strip.text.x=element_text(size=10),
    axis.text=element_text(color="black",size=8),
    axis.title=element_text(size=10),
    legend.text=element_text(size=10),
    legend.title =element_text(size=10),
    plot.title = element_text(size=15,hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.title.x = element_text(vjust=10))
      
      All_flu <- cowplot::plot_grid(flu_plot, flu_rollsum_plot,flu_intensity_plot,
                                    flu_rollsum16_plot,flu_intensity16_plot,
                                    flu_rollsum4_plot,flu_intensity4_plot,
                                    ncol=1,nrow=7,
                                    labels = c('A', 'B', 'C', 'D', 'E','F', 'G'))
      
      return( All_flu)
}
      
     
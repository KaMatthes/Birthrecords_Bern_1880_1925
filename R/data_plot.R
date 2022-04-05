function_plot <- function() {
  
  
boxplot_weight <- ggplot(data=used.data)+
    geom_violin(aes(x=year,y=weight,group=year, fill=year))+
    geom_boxplot(aes(x=year,y=weight,group=year, fill=year),width=0.2)+
    ylab("Birthweight in gram")+
    ggtitle("Birthweihts")+
    scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                    cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
    xlab("")+
    theme_bw()+
    theme(aspect.ratio=1,
          strip.text.x=element_text(size=strip_text),
          axis.text=element_text(color="black",size=size_axis),
          axis.title=element_text(size=size_axis_title),
          legend.text=element_text(size=size_legend),
          legend.title = element_blank(),
          plot.title = element_text(size=size_legend,hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          axis.title.x = element_text(vjust=10))

  weighttab <- table(used.data$Weight_cat_quan,used.data$year)
  weighttabpro <- prop.table(weighttab,2)
  weighttabpro <-melt(weighttabpro) 
  colnames(weighttabpro) <- c("weight","year","prop")
  weighttabpro <- na.omit(weighttabpro)
  weighttabpro <-  weighttabpro %>%
    mutate(weight = as.factor(weight),
           year = as.factor(year))
  
  weightplot <- ggplot(data=weighttabpro)+
    geom_bar( aes(x =  year , y = prop,fill =  weight),stat="identity")+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual("weight",values = cbp1)+
    ylab("Weight cat")+
    xlab("")+
    guides(fill=guide_legend(title="Weight: "))+
    theme_bw()+
    theme(aspect.ratio=1,
          strip.text.x=element_text(size=strip_text),
          axis.text=element_text(color="black",size=size_axis),
          axis.title=element_text(size=size_axis_title),
          legend.text=element_text(size=size_legend),
          legend.title =element_text(size=size_legend),
          plot.title = element_text(size=plot_title,hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          axis.title.x = element_text(vjust=10))
  
  
  # weightscaletab <- table(used.data$Weight_cat_quan_scale,used.data$year)
  # weightscaletabpro <- prop.table(weightscaletab,2)
  # weightscaletabpro <-melt(weightscaletabpro) 
  # colnames(weightscaletabpro) <- c("weightscale","year","prop")
  # weightscaletabpro <- na.omit(weightscaletabpro)
  # weightscaletabpro <-  weightscaletabpro %>%
  #   mutate(weightscale = as.factor(weightscale),
  #          year = as.factor(year))
  # 
  # weightscaleplot <- ggplot(data=weightscaletabpro)+
  #   geom_bar( aes(x =  year , y = prop,fill =  weightscale),stat="identity")+
  #   scale_y_continuous(labels = scales::percent)+
  #   scale_fill_manual("weightscale",values = cbp1)+
  #   ylab("weightscale cat")+
  #   xlab("")+
  #   guides(fill=guide_legend(title="Weight "))+
  #   theme_bw()+
  #   theme(aspect.ratio=1,
  #         strip.text.x=element_text(size=strip_text),
  #         axis.text=element_text(color="black",size=size_axis),
  #         axis.title=element_text(size=size_axis_title),
  #         legend.text=element_text(size=size_legend),
  #         legend.title =element_text(size=size_legend),
  #         plot.title = element_text(size=plot_title,hjust = 0.5),
  #         axis.text.x = element_text(angle = 45, hjust = 1),
  #         legend.position = "top",
  #         axis.title.x = element_text(vjust=10))
  
  boytab <- table(used.data$boy,used.data$year)
  boytabpro <- prop.table(boytab,2)
  boytabpro <-melt(boytabpro) 
  colnames(boytabpro) <- c("boy","year","prop")
  boytabpro <- na.omit(boytabpro)
  boytabpro <-  boytabpro %>%
  mutate(boy = as.factor(boy),
  year = as.factor(year))
      
  Boyplot <- ggplot(data=boytabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  boy),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("Sex",values = cbp1)+
        ylab("Sex")+
        xlab("")+
        guides(fill=guide_legend(title="Sex: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
  ParaKattab <- table(used.data$parity,used.data$year)
  ParaKatpro <- prop.table(ParaKattab,2)
  ParaKatpro <-melt(ParaKatpro)
  colnames(ParaKatpro) <- c("ParaKat","year","prop")
  ParaKatpro <- na.omit(ParaKatpro)
  ParaKatpro <-   ParaKatpro %>%
    mutate(boy = as.factor(ParaKat),
           year = as.factor(year))
      
      ParaKatplot <- ggplot(data=ParaKatpro)+
        geom_bar(aes(x =  year , y = prop,fill = factor(ParaKat, levels=c(">=4","3","2","1")))
                 ,stat="identity")+
        scale_y_continuous(labels=percent)+
        scale_fill_manual("Parity",values = cbp1)+
        ylab("Parity")+
        xlab("")+
        guides(fill=guide_legend(title="Parity: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      boxplot_Alter <- ggplot(data=used.data)+
        geom_violin(aes(x=year,y=matage,group=year, fill=year))+
        geom_boxplot(aes(x=year,y=matage,group=year, fill=year),width=0.2)+
        ylab("Age of mother in years")+
        xlab("")+
        scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                        cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
        ggtitle("Age of mother")+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title = element_blank(),
              plot.title = element_text(size=size_legend,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "",
              axis.title.x = element_text(vjust=10))
      
      
      boxplot_GestationalAge <- ggplot(data=used.data)+
        geom_violin(aes(x=year,y=gest,group=year, fill=year))+
        geom_boxplot(aes(x=year,y=gest,group=year, fill=year),width=0.2)+
        ylab("Gestational age in weeks")+
        ggtitle("Gestational age")+
        scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                        cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
        xlab("")+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title = element_blank(),
              plot.title = element_text(size=size_legend,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              axis.title.x = element_text(vjust=10))
      
      
      Gestkattab <- table(used.data$Gest_group,used.data$year)
      Gestkattabpro <- prop.table(Gestkattab,2)
      Gestkattabpro <-melt(Gestkattabpro) 
      colnames(Gestkattabpro) <- c("Gest_group","year","prop")
      Gestkattabpro <- na.omit(Gestkattabpro)
      Gestkattabpro <-  Gestkattabpro %>%
        mutate(Gest_group = as.factor(Gest_group),
               year = as.factor(year))
      
      Gestkatplot <- ggplot(data=Gestkattabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  Gest_group),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("Gestational age groups",values = cbp1)+
        ylab("Gestational age")+
        xlab("")+
        guides(fill=guide_legend(title="Gestational age: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      boxplot_Birthweek <- ggplot(data=used.data)+
        geom_violin(aes(x=year,y=birth_isoweek,group=year, fill=year))+
        geom_boxplot(aes(x=year,y=birth_isoweek,group=year, fill=year),width=0.2)+
        ylab("Birthweek")+
        ggtitle("Birthweek")+
        scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                        cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
        xlab("")+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title = element_blank(),
              plot.title = element_text(size=size_legend,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              axis.title.x = element_text(vjust=10))
      
      
      boxplot_Birthmonth <- ggplot(data=used.data)+
        geom_violin(aes(x=year,y=birth_month,group=year, fill=year))+
        geom_boxplot(aes(x=year,y=birth_month,group=year, fill=year),width=0.2)+
        ylab("Birthmonth")+
        ylim(1,12)+
        ggtitle("Birthmonth")+
        scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                        cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
        xlab("")+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title = element_blank(),
              plot.title = element_text(size=size_legend,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              axis.title.x = element_text(vjust=10))
      
     Birthmonth_cattab <- table(as.factor(used.data$birth_month),used.data$year)
     Birthmonth_catpro <- prop.table(Birthmonth_cattab,2)
     Birthmonth_catpro <-melt(Birthmonth_catpro)
      colnames( Birthmonth_catpro) <- c("Birthmonth_cat","year","prop")
     Birthmonth_catpro <- na.omit( Birthmonth_catpro)
     Birthmonth_catpro <-  Birthmonth_catpro %>%
        mutate(boy = as.factor(Birthmonth_cat),
               year = as.factor(year))
      
     Birthmonth_catplot <- ggplot(data= Birthmonth_catpro)+
       geom_bar(aes(x =  year , y = prop,fill = factor(Birthmonth_cat, levels=c("12","11","10","9","8","7","6","5","4","3","2","1")))
                ,stat="identity")+
       scale_y_continuous(labels=percent)+
       scale_fill_manual("Birthmonth_cat",values = mypalette2)+
       ylab("Birthmonth")+
       xlab("")+
       guides(fill=guide_legend(title="Birthmonth: "))+
       theme_bw()+
       theme(aspect.ratio=1,
             strip.text.x=element_text(size=strip_text),
             axis.text=element_text(color="black",size=size_axis),
             axis.title=element_text(size=size_axis_title),
             legend.text=element_text(size=size_legend),
             legend.title =element_text(size=size_legend),
             plot.title = element_text(size=plot_title,hjust = 0.5),
             axis.text.x = element_text(angle = 45, hjust = 1),
             legend.position = "top",
             axis.title.x = element_text(vjust=10))
     
     
     Birthseason_cattab <- table(as.factor(used.data$birth_season),used.data$year)
     Birthseason_catpro <- prop.table(Birthseason_cattab,2)
     Birthseason_catpro <-melt(Birthseason_catpro)
     colnames( Birthseason_catpro) <- c("Birthseason_cat","year","prop")
     Birthseason_catpro <- na.omit( Birthseason_catpro)
     Birthseason_catpro <-  Birthseason_catpro %>%
       mutate(boy = as.factor(Birthseason_cat),
              year = as.factor(year))
     
     Birthseason_catplot <- ggplot(data= Birthseason_catpro)+
       geom_bar(aes(x =  year , y = prop,fill = Birthseason_cat)
                ,stat="identity")+
       scale_y_continuous(labels=percent)+
       scale_fill_manual("Birthseason_cat",values = cbp1)+
       ylab("Birthseason")+
       xlab("")+
       guides(fill=guide_legend(title="Birthseason: "))+
       theme_bw()+
       theme(aspect.ratio=1,
             strip.text.x=element_text(size=strip_text),
             axis.text=element_text(color="black",size=size_axis),
             axis.title=element_text(size=size_axis_title),
             legend.text=element_text(size=size_legend),
             legend.title =element_text(size=size_legend),
             plot.title = element_text(size=plot_title,hjust = 0.5),
             axis.text.x = element_text(angle = 45, hjust = 1),
             legend.position = "top",
             axis.title.x = element_text(vjust=10))
     
     
      
     Stillborntab <- table(used.data$stillborn,used.data$year)
     Stillborntabpro <- prop.table(Stillborntab,2)
     Stillborntabpro <-melt(Stillborntabpro) 
      colnames(Stillborntabpro) <- c("Stillborn","year","prop")
     Stillborntabpro <- na.omit(Stillborntabpro)
     Stillborntabpro <- Stillborntabpro %>%
        mutate(Stillborn = as.factor(Stillborn),
               year = as.factor(year))
      
     Stillbornplot <- ggplot(data=Stillborntabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  Stillborn),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("Stillborn",values = cbp1)+
        ylab("Stillborn")+
        xlab("")+
        guides(fill=guide_legend(title="Stillborn: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      citytab <- table(used.data$city,used.data$year)
      citypro <- prop.table(citytab,2)
      citypro <-melt(citypro)
      colnames(citypro) <- c("city","year","prop")
      citypro <- na.omit(citypro)
      citypro <-  citypro %>%
        mutate(city = as.factor(city),
               year = as.factor(year))
      
      cityplot <- ggplot(data=citypro)+
        geom_bar(aes(x =  year , y = prop,fill = city)
                 ,stat="identity")+
        scale_y_continuous(labels=percent)+
        scale_fill_manual("city",values = cbp1)+
        ylab("city")+
        xlab("")+
        guides(fill=guide_legend(title="city: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      insurancetab <- table(used.data$insurance,used.data$year)
      insurancepro <- prop.table(insurancetab,2)
      insurancepro <-melt(insurancepro)
      colnames(insurancepro) <- c("insurance","year","prop")
      insurancepro <- na.omit(insurancepro)
      insurancepro <-  insurancepro %>%
        mutate(insurance = as.factor(insurance),
               year = as.factor(year))
      
      insuranceplot <- ggplot(data=insurancepro)+
        geom_bar(aes(x =  year , y = prop,fill = insurance)
                 ,stat="identity")+
        scale_y_continuous(labels=percent)+
        scale_fill_manual("insurance",values = cbp1)+
        ylab("insurance")+
        xlab("")+
        guides(fill=guide_legend(title="insurance: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      marriedtab <- table(used.data$married,used.data$year)
      marriedpro <- prop.table(marriedtab,2)
      marriedpro <-melt(marriedpro)
      colnames(marriedpro) <- c("married","year","prop")
      marriedpro <- na.omit(marriedpro)
      marriedpro <-  marriedpro %>%
        mutate(married = as.factor(married),
               year = as.factor(year))
      
      marriedplot <- ggplot(data=marriedpro)+
        geom_bar(aes(x =  year , y = prop,fill =married)
                 ,stat="identity")+
        scale_y_continuous(labels=percent)+
        scale_fill_manual("married",values = cbp1)+
        ylab("married")+
        xlab("")+
        guides(fill=guide_legend(title="married: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      matbodytab <- table(used.data$matbody2,used.data$year)
      matbodytabpro <- prop.table(matbodytab,2)
      matbodytabpro <-melt(matbodytabpro) 
      colnames(matbodytabpro) <- c("matbody","year","prop")
      matbodytabpro <- na.omit(matbodytabpro)
      matbodytabpro <- matbodytabpro %>%
        mutate(matbody = as.factor(matbody),
               year = as.factor(year))
      
      matbodyplot <- ggplot(data=matbodytabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  matbody),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("maternal body",
                          breaks=c("1","2","3"),
                          labels=c("grazil","mittel","kräftig"),
                          values = cbp1)+
        ylab("matbody")+
        xlab("")+
        guides(fill=guide_legend(title="maternal body: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      matheighttab <- table(used.data$matheight2,used.data$year)
      matheighttabpro <- prop.table(matheighttab,2)
      matheighttabpro <-melt(matheighttabpro) 
      colnames(matheighttabpro) <- c("matheight","year","prop")
      matheighttabpro <- na.omit(matheighttabpro)
      matheighttabpro <- matheighttabpro %>%
        mutate(matheight = as.factor(matheight),
               year = as.factor(year))
      
      matheightplot <- ggplot(data=matheighttabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  matheight),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("maternal height",
                          breaks=c("1","2","3"),
                          labels=c("klein","mittel","gross"),
                          values = cbp1)+
        ylab("matheight")+
        xlab("")+
        guides(fill=guide_legend(title="maternal height: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      
      malnutritiontab <- table(used.data$malnutrition2,used.data$year)
      malnutritiontabpro <- prop.table(malnutritiontab,2)
      malnutritiontabpro <-melt(malnutritiontabpro) 
      colnames(malnutritiontabpro) <- c("malnutrition","year","prop")
      malnutritiontabpro <- na.omit(malnutritiontabpro)
      malnutritiontabpro <- malnutritiontabpro %>%
        mutate(malnutrition = as.factor(malnutrition),
               year = as.factor(year))
      
      malnutritionplot <- ggplot(data=malnutritiontabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  malnutrition),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("malnutrition",
                          values = cbp1)+
        ylab("malnutrition")+
        xlab("")+
        guides(fill=guide_legend(title="malnutrition: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      occupationtab <- table(used.data$occupation2,used.data$year)
      occupationtabpro <- prop.table(occupationtab,2)
      occupationtabpro <-melt(occupationtabpro) 
      colnames(occupationtabpro) <- c("occupation","year","prop")
      occupationtabpro <- na.omit(occupationtabpro)
      occupationtabpro <- occupationtabpro %>%
        mutate(occupation = as.factor(occupation),
               year = as.factor(year))
      
      occupationplot <- ggplot(data=occupationtabpro)+
        geom_bar( aes(x =  year , y = prop,fill =  occupation),stat="identity")+
        scale_y_continuous(labels = scales::percent)+
        scale_fill_manual("occupation",
                          breaks=c("1","2","3","4","5","6","7"),
                          labels=c("Landarbeiterin","Magd","Fabrikarbeiterin", "Hausfrau","Schneiderin","andere","Köchin"),
                          values = cbp1)+
        ylab("occupation")+
        xlab("")+
        guides(fill=guide_legend(title="occupation: "))+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title =element_text(size=size_legend),
              plot.title = element_text(size=plot_title,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top",
              axis.title.x = element_text(vjust=10))
      
      
      boxplot_agemenarche <- ggplot(data=used.data)+
        geom_violin(aes(x=year,y=agemenarche,group=year, fill=year))+
        geom_boxplot(aes(x=year,y=agemenarche,group=year, fill=year),width=0.2)+
        ylab("Agemenarche")+
        xlim(c("1880","1885","1890","1895", "1900"))+
        ggtitle("Age menarche")+
        scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
        xlab("")+
        theme_bw()+
        theme(aspect.ratio=1,
              strip.text.x=element_text(size=strip_text),
              axis.text=element_text(color="black",size=size_axis),
              axis.title=element_text(size=size_axis_title),
              legend.text=element_text(size=size_legend),
              legend.title = element_blank(),
              plot.title = element_text(size=size_legend,hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              axis.title.x = element_text(vjust=10))
      
      
      
      
      # gestdummytab <- table(used.data$gestdummy,used.data$year)
      # gestdummypro <- prop.table(gestdummytab,2)
      # gestdummypro <-melt(gestdummypro)
      # colnames(gestdummypro) <- c("gestdummy","year","prop")
      # gestdummypro <- na.omit(gestdummypro)
      # gestdummypro <-  gestdummypro %>%
      #   mutate(gestdummy = as.factor(gestdummy),
      #          year = as.factor(year))
      # 
      # gestdummyplot <- ggplot(data=gestdummypro)+
      #   geom_bar(aes(x =  year , y = prop,fill =gestdummy)
      #            ,stat="identity")+
      #   scale_y_continuous(labels=percent)+
      #   scale_fill_manual("gestdummy",values = cbp1)+
      #   ylab("gestdummy")+
      #   xlab("")+
      #   guides(fill=guide_legend(title="gestdummy: "))+
      #   theme_bw()+
      #   theme(aspect.ratio=1,
      #         strip.text.x=element_text(size=strip_text),
      #         axis.text=element_text(color="black",size=size_axis),
      #         axis.title=element_text(size=size_axis_title),
      #         legend.text=element_text(size=size_legend),
      #         legend.title =element_text(size=size_legend),
      #         plot.title = element_text(size=plot_title,hjust = 0.5),
      #         axis.text.x = element_text(angle = 45, hjust = 1),
      #         legend.position = "top",
      #         axis.title.x = element_text(vjust=10))
      
      
      AllCoef <- cowplot::plot_grid(boxplot_weight, weightplot,Boyplot,
                                    ParaKatplot,boxplot_Alter , boxplot_GestationalAge, 
                                    Gestkatplot,  boxplot_Birthweek,boxplot_Birthmonth,
                                    Birthmonth_catplot, Stillbornplot,cityplot, 
                                    insuranceplot,marriedplot,matbodyplot,
                                    matheightplot,malnutritionplot, occupationplot,
                                     boxplot_agemenarche ,
                                    ncol=3,nrow=7)
      
      return(AllCoef)
}
      
     
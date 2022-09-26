####### Deskriptive plot ##########

data_plot <- read.csv(paste0("data_raw/",data.bern), header=TRUE, sep=";") %>%
  filter(!year=="1901") %>%
  filter(!year=="1902") %>%
  filter(!year=="1903") %>%
  filter(!year=="1911") %>%
  filter(!year=="1912") %>%
  filter(!year=="1913") %>%
  filter(!year=="1923") %>%
  filter(multiple==0) %>%
  filter(!(gest <30)) %>%
  filter(!(weight < 1000)) %>%
  filter(!(matage > 50))  %>%
  filter(!(matage <14)) %>%
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
         occupation2 = dplyr::recode(occupation2,
                                     "5" ="7",
                                     "6" = "7"),
         occupation2 = factor(occupation2, levels=c("4","1","2","3","7")))


boxplot_weight <- ggplot(data=data_plot)+
  geom_violin(aes(x=year,y=weight,group=factor(year), fill=factor(year)))+
  geom_boxplot(aes(x=year,y=weight,group=factor(year), fill=factor(year)),width=0.2)+
  ylab("Birthweight in gram")+
  ggtitle("Birthweights")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                  cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  xlab("")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=size_legend),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(vjust=10))

boytab <- table(data_plot$boy,data_plot$year)
boytabpro <- prop.table(boytab,2)
boytabpro <-melt(boytabpro) 
colnames(boytabpro) <- c("boy","year","prop")
boytabpro <- na.omit(boytabpro)
boytabpro <-  boytabpro %>%
  mutate(boy = as.factor(boy))

Boyplot <- ggplot(data=boytabpro)+
  geom_bar( aes(x =  year , y = prop,fill =  boy),stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Sex",
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("female", "male"))+
  ylab("")+
  xlab("")+
  ggtitle("Sex")+
  guides(fill=guide_legend(title="Sex: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))

ParaKattab <- table(data_plot$parity,data_plot$year)
ParaKatpro <- prop.table(ParaKattab,2)
ParaKatpro <-melt(ParaKatpro)
colnames(ParaKatpro) <- c("ParaKat","year","prop")
ParaKatpro <- na.omit(ParaKatpro)
ParaKatpro <-   ParaKatpro %>%
  mutate(boy = as.factor(ParaKat))

ParaKatplot <- ggplot(data=ParaKatpro)+
  geom_bar(aes(x =  year , y = prop,fill = factor(ParaKat, levels=c(">=4","3","2","1")))
           ,stat="identity")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual("Parity",values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Parity")+
  guides(fill=guide_legend(title="Parity: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))


boxplot_Alter <- ggplot(data=data_plot)+
  geom_violin(aes(x=year,y=matage,group=factor(year), fill=factor(year)))+
  geom_boxplot(aes(x=year,y=matage,group=factor(year), fill=factor(year)),width=0.2)+
  ylab("Age of mother in years")+
  xlab("")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                  cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  ggtitle("Age of mother")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=size_legend),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(vjust=10))


boxplot_GestationalAge <- ggplot(data=data_plot)+
  geom_violin(aes(x=year,y=gest,group=factor(year), fill=factor(year)))+
  geom_boxplot(aes(x=year,y=gest,group=factor(year), fill=factor(year)),width=0.2)+
  ylab("Gestational age in weeks")+
  ggtitle("Gestational age")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                  cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  xlab("")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=size_legend),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(vjust=10))

Gestkattab <- table(data_plot$Gest_group,data_plot$year)
Gestkattabpro <- prop.table(Gestkattab,2)
Gestkattabpro <-melt(Gestkattabpro) 
colnames(Gestkattabpro) <- c("Gest_group","year","prop")
Gestkattabpro <- na.omit(Gestkattabpro)
Gestkattabpro <-  Gestkattabpro %>%
  mutate(Gest_group = as.factor(Gest_group))

Gestkatplot <- ggplot(data=Gestkattabpro)+
  geom_bar( aes(x =  year , y = prop,fill =  Gest_group),stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Gestational age groups",
                    values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Gestational age")+
  guides(fill=guide_legend(title="Gestational age: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))


Birthweekdaytab <- table(data_plot$birth_weekday,data_plot$year)
Birthweekdaytabpro <- prop.table(Birthweekdaytab,2)
Birthweekdaytabpro <-melt(Birthweekdaytabpro) 
colnames(Birthweekdaytabpro) <- c("birth_weekday","year","prop")
Birthweekdaytabpro <- na.omit(Birthweekdaytabpro)
Birthweekdaytabpro <- Birthweekdaytabpro %>%
  mutate(birth_weekday= as.factor(birth_weekday))


barplot_Birthweekday <- ggplot(data=Birthweekdaytabpro)+
  geom_bar( aes(x =  year , y = prop,fill =  birth_weekday),stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Weekday",
                    breaks=c("1","2","3","4","5","6","7"),
                    labels=c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday","Sunday"),
                    values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Weekday")+
  guides(fill=guide_legend(title="Weekday: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
        # axis.title.x = element_text(vjust=10))


Birthweekday <- plyr::count(data_plot, 'birth_weekday') %>%
  mutate(prop=freq/sum(freq))


barplot_Birthweekday_total <- ggplot(data=Birthweekday)+
  geom_bar( aes(x =  birth_weekday , y = prop, fill=factor(birth_weekday)),stat="identity")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                  cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  xlab("Weekday")+
  ylab("")+
  ggtitle("Weekday")+
  guides(fill=guide_legend(title="Weekday: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
        # axis.title.x = element_text(vjust=10))

Birthmonth_cattab <- table(as.factor(data_plot$birth_month),data_plot$year)
Birthmonth_catpro <- prop.table(Birthmonth_cattab,2)
Birthmonth_catpro <-melt(Birthmonth_catpro)
colnames( Birthmonth_catpro) <- c("Birthmonth_cat","year","prop")
Birthmonth_catpro <- na.omit( Birthmonth_catpro)
Birthmonth_catpro <-  Birthmonth_catpro %>%
  mutate(boy = as.factor(Birthmonth_cat))

Birthmonth_catplot <- ggplot(data= Birthmonth_catpro)+
  geom_bar(aes(x =  year , y = prop,fill = factor(Birthmonth_cat, levels=c("12","11","10","9","8","7","6","5","4","3","2","1")))
           ,stat="identity")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual("Birthmonth_cat",values = mypalette2)+
  ylab("")+
  ggtitle("Birthmonth")+
  xlab("")+
  guides(fill=guide_legend(title="Birthmonth: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
        # axis.title.x = element_text(vjust=10))

Birthmonth <- plyr::count(data_plot, 'birth_month') %>%
  mutate(prop=freq/sum(freq))


Birthmonth_catplot_total <- ggplot(data=     Birthmonth)+
  geom_bar(aes(x =  factor(birth_month) , y = prop, fill=factor(birth_month)),stat="identity")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],
                                  cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  xlab("Birthmonth")+
  ylab("")+
  ggtitle("Birthmonth")+
  guides(fill=guide_legend(title="Birthmonth: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
        # axis.title.x = element_text(vjust=10))

Stillborntab <- table(data_plot$stillborn,data_plot$year)
Stillborntabpro <- prop.table(Stillborntab,2)
Stillborntabpro <-melt(Stillborntabpro) 
colnames(Stillborntabpro) <- c("Stillborn","year","prop")
Stillborntabpro <- na.omit(Stillborntabpro)
Stillborntabpro <- Stillborntabpro %>%
  mutate(Stillborn = as.factor(Stillborn))

Stillbornplot <- ggplot(data=Stillborntabpro)+
  geom_bar( aes(x =  year , y = prop,fill =  Stillborn),stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual("Stillborn",
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("no", "yes"))+
  ylab("")+
  xlab("")+
  ggtitle("Stillborn")+
  guides(fill=guide_legend(title="Stillborn: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))


citytab <- table(data_plot$city,data_plot$year)
citypro <- prop.table(citytab,2)
citypro <-melt(citypro)
colnames(citypro) <- c("city","year","prop")
citypro <- na.omit(citypro)
citypro <-  citypro %>%
  mutate(city = as.factor(city))

cityplot <- ggplot(data=citypro)+
  geom_bar(aes(x =  year , y = prop,fill = city)
           ,stat="identity")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual("city",
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("no", "yes"))+
  ylab("")+
  xlab("")+
  ggtitle("City")+
  guides(fill=guide_legend(title="city: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))

insurancetab <- table(data_plot$insurance,data_plot$year)
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
  scale_fill_manual("insurance",
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("no", "yes"))+
  ylab("")+
  xlab("")+
  ggtitle("Insurance")+
  guides(fill=guide_legend(title="insurance: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))

marriedtab <- table(data_plot$married,data_plot$year)
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
  scale_fill_manual("married",
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("no", "yes"))+
  ylab("")+
  xlab("")+
  ggtitle("Married")+
  guides(fill=guide_legend(title="married: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))

matbodytab <- table(data_plot$matbody2,data_plot$year)
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
                    labels=c("grazil","medium","strong"),
                    values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Maternal body")+
  guides(fill=guide_legend(title="maternal body: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))



matheighttab <- table(data_plot$matheight2,data_plot$year)
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
                    labels=c("small","medium","tall"),
                    values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Maternal height")+
  guides(fill=guide_legend(title="maternal height: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))



malnutritiontab <- table(data_plot$malnutrition2,data_plot$year)
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
                    values = cbp1,
                    breaks=c("0","1"),
                    labels=c("no", "yes"))+
  ylab("")+
  xlab("")+
  ggtitle("Malnutrition")+
  guides(fill=guide_legend(title="malnutrition: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))


occupationtab <- table(data_plot$occupation2,data_plot$year)
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
                    breaks=c("1","2","3","4","7"),
                    labels=c("Farm worker","Maid","Worker", "Housewife","other"),
                    values = cbp1)+
  ylab("")+
  xlab("")+
  ggtitle("Occupation")+
  guides(fill=guide_legend(title="occupation: "))+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=40),
        axis.title=element_text(size=40),
        legend.text=element_text(size=30),
        legend.title = element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        axis.title.x = element_text(vjust=10))



boxplot_agemenarche <- ggplot(data=data_plot)+
  geom_violin(aes(x=year,y=agemenarche,group=factor(year), fill=factor(year)))+
  geom_boxplot(aes(x=year,y=agemenarche,group=factor(year), fill=factor(year)),width=0.2)+
  ylab("Agemenarche")+
  ggtitle("Age menarche")+
  scale_fill_manual("",values = c(cbp1[1],cbp1[2],cbp1[1],cbp1[2],cbp1[1]))+
  xlab("")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=50),
        axis.title=element_text(size=50),
        legend.text=element_text(size=size_legend),
        legend.title = element_blank(),
        plot.title = element_text(size=50,hjust = 0.5),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(vjust=10))

 

 Supplement3 <- cowplot::plot_grid(boxplot_weight,boxplot_Alter, 
                                   boxplot_GestationalAge,boxplot_agemenarche,
                                   ncol=2,nrow=2, align="hv", labels=c("A","B","C", "D"),
                                   label_size = 50)
 cowplot::save_plot("output/Supplement3.pdf", Supplement3,base_height=50,base_width=55,limitsize = FALSE)
 
 Supplement4 <- cowplot::plot_grid(Boyplot,ParaKatplot, 
                                   Gestkatplot,Stillbornplot,
                                   cityplot,insuranceplot,
                                   marriedplot,
                                   ncol=2,nrow=4, align="hv", labels=c("A","B","C","D", "E", "F", "G"),
                                   label_size = 30)
 cowplot::save_plot("output/Supplement4.pdf", Supplement4 ,base_height=50,base_width=30,limitsize = FALSE)
 
 Supplement5 <- cowplot::plot_grid(  Birthmonth_catplot_total, barplot_Birthweekday_total, 
                                     Birthmonth_catplot, barplot_Birthweekday,
                                   ncol=2,nrow=2, align="hv", labels=c("A","B","C","D"),
                                   label_size = 50)
 cowplot::save_plot("output/Supplement5.pdf",  Supplement5,base_height=50,base_width=55,limitsize = FALSE)
 
 Supplement6 <- cowplot::plot_grid(    matbodyplot, matheightplot,
                                       malnutritionplot, occupationplot, 
                                     ncol=2,nrow=2, align="hv", labels=c("A","B","C","D"),
                                     label_size = 50)
 
 cowplot::save_plot("output/Supplement6.pdf",  Supplement6,base_height=50,base_width=55,limitsize = FALSE)
 
data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE)  %>%
  mutate(Mecanisme_normal=ifelse(Mecanisme_normal==0,1,0),
         Mecanisme_normal= as.factor(Mecanisme_normal),
         Position_normal=ifelse(Position_normal==0,1,0),
         Position_normal= as.factor(Position_normal),
         Episiotomy = as.factor(Episiotomy),
         sex= as.factor(sex),
         sex=recode(sex, 
                    "0" = "male",
                    "1" ="female" ),
         Position_normal = recode(Position_normal,"0"="normal",
                                  "1" = "non-normal"))

data_com$birthweight100 <- data_com$birthweight/100
data_com$height10 <- data_com$height/10

data_laus <- data_com %>%
  filter(City=="Lausanne") 


data_basel <- data_com %>%
  filter(City=="Basel") 

### Episiotomy, 1= ja, 0 = nein

table(data_laus$Position_normal)
table(data_laus$Episiotomy)
table(data_laus$Position_normal,data_laus$Episiotomy)

# Position normal und Episiotomy nein: 966/1062 = 90.96 %
# Position normal und Episiotomy ja: 9.04 %

# Position nicht normal und Episiotomy nein: 29/83 = 97.6 %
# Postion nicht normal und Episiotomy ja:  2.4 %

# Ergebniss  Postion stimmt, weniger Eposiotomy bei nicht normaler Postion. OR kleiner 1, aber nicht signifikant.


Mod_laus_ep_uni <- data.frame(summary(glm(Episiotomy ~  Position_normal, 
                                      data = data_laus, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Lausanne",
         outcome="Episiotomy")

Mod_laus_ep <- data.frame(summary(glm(Episiotomy ~  sex + parity + Position_normal + age_mother + head_circ + 
                                        birthweight100 + height10 + GA_weeks +
                                        Bassin_Epines +Bassin_Cretes +Bassin_ConjExt, 
                                        data = data_laus, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
           mutate(City="Lausanne",
                  outcome="Episiotomy")

Mod_laus_ep


#Basel Modell

table(data_basel$Position_normal)
table(data_basel$Episiotomy)
table(data_basel$Position_normal,data_basel$Episiotomy)

# Position normal und Episiotomy nein: 709/1290 = 54.96 %
# Position normal und Episiotomy ja: 45.04 %

# Position nicht normal und Episiotomy nein: 29/83 = 54.7 %
# Postion nicht normal und Episiotomy ja:  45.29 %


# Ergebniss Postion normal stimmt, keine Unterschiede zwischen Postion normal und nicht normal, multivariate ein weniger
# grösser OR, aber immer noch nicht signifakt.

Mod_basel_ep_uni <- data.frame(summary(glm(Episiotomy ~  Position_normal, 
                                       data = data_basel, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",    
         outcome="Episiotomy")

Mod_basel_ep <- data.frame(summary(glm(Episiotomy ~  sex + parity + Position_normal + age_mother + head_circ + 
                                        birthweight100 + height10 + GA_weeks +
                                        Bassin_Epines +Bassin_Cretes +Bassin_ConjExt, 
                                      data = data_basel, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",    
         outcome="Episiotomy")

Mod_basel_ep


### Mecanisme_normal, 1 = nicht normal, 0 = normal
# co-factoren ergänzen oder ändern, nur Beispielhaft welche genommen 

# Lausanne, 

table(data_laus$Position_normal)
table(data_laus$Mecanisme_normal)
table(data_laus$Position_normal,data_laus$Mecanisme_normal)

# Position normal und Mechanis normal: 915/1062 = 86.16 %
# Position normal und Mechanis nicht normal:  13.84 %

# Position nicht normal und Mechanis normal: 29/83 = 34.9 %
# Postion nicht normal und Mechanis nicht normal:  65.01 %

# Ergbeniss Position normal stimmt -> Unterschiede sind sehr gross, siehe Prozente, daher grosser OR, Univariate noch
# kleiner, nach Adjustement grösser, dabei wird der OR besonders grösser bei hinzufügen der Parität ins Model.


Mod_laus_me_uni <- data.frame(summary(glm(Mecanisme_normal ~   Position_normal, 
                                      data = data_laus, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Lausanne",
         outcome="Mecanisme")

Mod_laus_me <- data.frame(summary(glm(Mecanisme_normal ~  sex + parity + Position_normal + 
                                        age_mother + head_circ + birthweight100 + height10 + 
                                        GA_weeks  + Bassin_Cretes, 
                                        data = data_laus, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Lausanne",
         outcome="Mecanisme")


# Basel

table(data_basel$Position_normal)
table(data_basel$Mecanisme_normal)
table(data_basel$Position_normal,data_basel$Mecanisme_normal)

# Position normal und Mechanis normal: 700/1290=  54.26 %
# Position normal und Mechanis nicht normal:  45.74 %

# Position nicht normal und Mechanis normal: 52/117 = 44.4 %
# Postion nicht normal und Mechanis nicht normal:  55.6%

# Ergbeniss Position normal stimmt, hier sind die Unterschiede nicht sehr gross, darum kleiner OR. Univariate 
# Analyse noch signifikant, aber noch Adjustment nicht mehr.

Mod_basel_me_un <- data.frame(summary(glm(Mecanisme_normal ~  Position_normal,
                                       data = data_basel, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",
         outcome="Mecanisme")



Mod_basel_me <- data.frame(summary(glm(Mecanisme_normal ~  sex + parity + Position_normal + 
                                         age_mother + head_circ + birthweight100 + height10 + 
                                         GA_weeks  + Bassin_Cretes, 
                                       data = data_basel, family="binomial"))$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",
         outcome="Mecanisme")


# Plot

data_plot <- rbind(Mod_laus_ep,Mod_basel_ep,
                   Mod_laus_me,Mod_basel_me) %>%
  mutate(Fac=recode(Fac,
                    "sex" = "Sex of the child",
                    "Bassin_ConjExt" = "Bassin ConjExt",
                    "Bassin_Cretes" = "Bassin Cretes",
                    "Bassin_Epines" = "Bassin Epines",
                    "birthweight100" = "Birthweight in 100gr",
                    "height10" = "Maternal height in 10cm",
                    "GA_weeks" = "Gestational age (w)",
                    "head_circ" = "Head circumference"))


OR_plot <- ggplot( data_plot , aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=City),lwd=lwd_size,position=pd,fatten=fatten_size)+
  facet_grid(~outcome)+
  # ylim(c(0,2.5))+
  labs(x="",y="OR") +
  # guides(color = guide_legend(override.aes = list(size = 1.5)))+
  ggtitle("Each city separately")+
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     # labels=c("Age >=40 (Ref)","Age <40"),
                     values = c(mypalette[3],mypalette[2]))+
  theme_bw()+
  theme(aspect.ratio=1,
        strip.text = element_text(color="black",size= strip_text),
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=size_legend_text),
        legend.title= element_blank(),
        legend.position = "bottom") +
  coord_flip() 

cowplot::save_plot("output/OR_plot2.pdf", OR_plot,base_height=8,base_width=20)  

###  Next steps: 
# Coeff Plot anpassen auch für kategorische Variablen
# Für welche Variablen soll das Modell kontrolliert werden? 
# Basel Daten updaten und integrieren
# Dauer Austreibungsphase als Outcome. Outcomes sind: Episiotomie, Intervention generell, Dauer Austreibung (z-transformation?), Stillbirth?
# Konstruieren Ratio Grösse zu Becken Mutter; Ratio Kopf Kind zu Conj Ext Mutter; Ratio Grösse Mutter vs. Grösse Kind?
# Interventionen nur modelieren für normale Kindspositionen?
# Ratio Height vs. Bassin_Cretes; Ratio head_circ vs Bassin_ConjExt; Height vs. Birthweight
# Frage zusätzlich zu Vergleichmodelle, für Basel der Sozialstatus, in Lausanne noch Gesundheitszustand der Frau als Einzelmodelle
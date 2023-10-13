data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE)  %>%
  mutate(Mecanisme_normal=ifelse(Mecanisme_normal==0,1,0),
         Mecanisme_normal= as.factor(Mecanisme_normal),
         Position_normal=ifelse(Position_normal==0,1,0),
         Position_normal= as.factor(Position_normal),
         Episiotomy = as.factor(Episiotomy),
         sex= as.factor(sex),
         # sex=recode(sex, 
         #            "0" = "male",
         #            "1" ="female" ),
         Position_normal = recode(Position_normal,"0"="normal",
                                  "1" = "non-normal"),
         birthweight100 = birthweight/100,
         height10 = height/10,
         Bassin_Cretes10= Bassin_Cretes/10,
         height_cretes = height10/Bassin_Cretes10,
         sex = factor( sex, levels = c("male", "female"))) %>%
  group_by(City) %>%
  mutate(Duree_2me_periode_z = (Duree_2me_periode-mean(Duree_2me_periode,na.rm = TRUE))/sd(Duree_2me_periode,na.rm = TRUE)) %>%
  ungroup()

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
                                        birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
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
                                         birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
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

Mod_laus_me <- data.frame(summary(glm(Mecanisme_normal ~  sex + parity + Position_normal + age_mother + head_circ + 
                                        birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
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



Mod_basel_me <- data.frame(summary(glm(Mecanisme_normal ~  sex + parity + Position_normal + age_mother + head_circ + 
                                         birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
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


# Dauer der Ausbreitung

# Lausanne


Mod_laus_du_uni <- data.frame(summary(lm(Duree_2me_periode_z ~   Position_normal, 
                                          data = data_laus))$coefficients) %>%
  mutate(Est = round(Estimate,2),
         CIl = round(Estimate - 1.96*`Std..Error`,2),
         CIu = round(Estimate + 1.96*`Std..Error`,2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Lausanne",
         outcome="Duration")

Mod_laus_du <- data.frame(summary(lm(Duree_2me_periode_z ~  sex + parity + Position_normal + age_mother + head_circ + 
                                        birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
                                      data = data_laus))$coefficients) %>%
  mutate(Est = round(Estimate,2),
         CIl = round(Estimate - 1.96*`Std..Error`,2),
         CIu = round(Estimate + 1.96*`Std..Error`,2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Lausanne",
         outcome="Duration")


# Basel
Mod_basel_du_un <- data.frame(summary(lm(Duree_2me_periode_z ~   Position_normal, 
                                        data = data_basel))$coefficients) %>%
  mutate(Est = round(Estimate,2),
         CIl = round(Estimate - 1.96*`Std..Error`,2),
         CIu = round(Estimate + 1.96*`Std..Error`,2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",
         outcome="Duration")



Mod_basel_du <- data.frame(summary(lm(Duree_2me_periode_z ~  sex + parity + Position_normal + age_mother + head_circ + 
                                        birthweight100 + height10 + GA_weeks + height_cretes + Bassin_Epines+ Bassin_ConjExt,
                                      data = data_basel))$coefficients) %>%
  mutate(Est = round(Estimate,2),
         CIl = round(Estimate - 1.96*`Std..Error`,2),
         CIu = round(Estimate + 1.96*`Std..Error`,2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  #filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(City="Basel",
         outcome="Duration")


# Plot

data_plot <- rbind(Mod_laus_ep,Mod_basel_ep,
                   Mod_laus_me,Mod_basel_me,
                   Mod_laus_du,Mod_basel_du) %>%
  mutate(Fac=recode(Fac,
                    "Position_normalnon-normal" = "Position non-normal [Ref:normal]",
                    "sexfemale" = "Sex female [Ref: male]",
                    "parity" ="Parity",
                    "age_mother" ="Age of the mother",
                    "GA_weeks" = "Gestational age (w)",
                    "Bassin_ConjExt" = "Bassin ConjExt",
                    "Bassin_Epines" = "Bassin Epines",
                    "height_cretes" = "Ratio height/Bassin Cretes",
                    # "Bassin_Cretes" = "Bassin Cretes",
                    "birthweight100" = "Birthweight in 100gr",
                    "height10" = "Maternal height in 10cm",
                    "head_circ" = "Head circumference in cm"),
         outcome = factor(outcome, levels = c("Episiotomy", "Mecanisme", "Duration")))



OR_plot <- ggplot( data_plot[!data_plot$Fac=="Position non-normal [Ref:normal]" & !data_plot$outcome=="Duration"  ,] , aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=City),lwd=1.5,position=pd,fatten=6)+
  facet_wrap(~outcome, ncol=2)+
  # ylim(c(0,2.5))+
  labs(x="",y="OR") +
  # guides(color = guide_legend(override.aes = list(size = 1.5)))+
  ggtitle("")+
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
  coord_flip(ylim=c(0, 2)) 

# cowplot::save_plot("output/OR_plot.pdf", OR_plot,base_height=8,base_width=15)  

OR_plot_position <- ggplot( data_plot[data_plot$Fac=="Position non-normal [Ref:normal]"& !data_plot$outcome=="Duration"  ,] , aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=City),lwd=1.5,position=pd,fatten=6)+
  facet_grid(~outcome)+
  # ylim(c(0,2.5))+
  labs(x="",y="OR") +
  # guides(color = guide_legend(override.aes = list(size = 1.5)))+
  ggtitle("")+
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
  coord_flip(ylim=c(0, 30)) 

# cowplot::save_plot("output/OR_plot_position.pdf", OR_plot_position ,base_height=8,base_width=15)  

OR_both <- cowplot::plot_grid( OR_plot,OR_plot_position,ncol=1,align="h")

cowplot::save_plot("output/OR_plot.pdf",OR_both ,base_height=15,base_width=15)  

OR_plot_duration <- ggplot( data_plot[data_plot$outcome=="Duration"  ,] , aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=City),lwd=1.5,position=pd,fatten=6)+
  facet_wrap(~outcome, ncol=2)+
  # ylim(c(0,2.5))+
  labs(x="",y="OR") +
  # guides(color = guide_legend(override.aes = list(size = 1.5)))+
  ggtitle("")+
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
  coord_flip(ylim=c(-1, 1)) 


cowplot::save_plot("output/OR_plot_duration.pdf", OR_plot_duration ,base_height=8,base_width=15)  

###  Next steps: 
# Coeff Plot anpassen auch für kategorische Variablen
# Für welche Variablen soll das Modell kontrolliert werden? 
# Basel Daten updaten und integrieren
# Dauer Austreibungsphase als Outcome. Outcomes sind: Episiotomie, Intervention generell, Dauer Austreibung (z-transformation?), Stillbirth?
# Konstruieren Ratio Grösse zu Becken Mutter; Ratio Kopf Kind zu Conj Ext Mutter; Ratio Grösse Mutter vs. Grösse Kind?
# Interventionen nur modelieren für normale Kindspositionen?
# Ratio Height vs. Bassin_Cretes (cor 0.39); Ratio head_circ vs Bassin_ConjExt (cor 0.09); Height vs. Birthweight (cor 0.15)
# Frage zusätzlich zu Vergleichmodelle, für Basel der Sozialstatus, in Lausanne noch Gesundheitszustand der Frau als Einzelmodelle
source("R/functions/function_or.R")
source("R/functions/function_lm.R")


data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE)  %>%
  mutate(
         Position_normal=ifelse(Position_normal==0,1,0),
         Position_normal= as.factor(Position_normal),
         Episiotomy = as.factor(Episiotomy),
         sex= as.factor(sex),
         stillbirth= as.factor(stillbirth),
         # sex=recode(sex, 
         #            "0" = "male",
         #            "1" ="female" ),
         Position_normal = recode(Position_normal,"0"="normal",
                                  "1" = "non-normal"),
         birthweight100 = birthweight/100,
         height10 = height/10,
         Bassin_Cretes10= Bassin_Cretes/10,
         head_terc = cut(head_circ, breaks=c(quantile( head_circ, c(0:3/3), na.rm = TRUE)),
                         labels=c("1","2","3"), include.lowest=TRUE),
         Bassin_ConjExt_terc = cut(Bassin_ConjExt, breaks=c(quantile( Bassin_ConjExt, c(0:3/3), na.rm = TRUE)),
                         labels=c("1","2","3"), include.lowest=TRUE),
         head_ConjExt = case_when(
          Bassin_ConjExt_terc==1 & head_terc==3 ~ "small-large",
          Bassin_ConjExt_terc==3 & head_terc==1 ~"large-small",
          Bassin_ConjExt_terc==1 & head_terc==1 ~"normal",
          Bassin_ConjExt_terc==2 & head_terc==2 ~"normal",
          Bassin_ConjExt_terc==3 & head_terc==3 ~"normal",
          Bassin_ConjExt_terc==1 & head_terc==2 ~"normal",
          Bassin_ConjExt_terc==2 & head_terc==1 ~"normal",
          Bassin_ConjExt_terc==2 & head_terc==3 ~"normal",
          Bassin_ConjExt_terc==3 & head_terc==2 ~"normal"),
          head_ConjExt  = factor( head_ConjExt , levels = c("normal","large-small","small-large")),
          sex = factor( sex, levels = c("male", "female")),
         SEP_comb_3a = as.factor(SEP_comb_3a),
         SEP_comb_3a = recode(SEP_comb_3a,
                              "0" = "medium",
                              "9" = "medium",
                              "2" = "medium",
                              "1" = "low",
                              "3" = "high"),
         SEP_comb_3a = factor(SEP_comb_3a, levels=c("low", "medium", "high")),
         Etat.general = ifelse(Etat.general == 1 & rickets ==1, 2, Etat.general),
         Etat.general = as.factor(Etat.general),
         Etat.general = recode(Etat.general,
                              "1" = "healthy",
                              "2" = "medium",
                              "3" = "unhealthy"),
         Etat.general = factor(Etat.general, levels=c("healthy", "medium", "unhealthy")),
         Maternal.body=case_when(Obesite==1 ~ "obese",
                                 Maigre==1 ~ "thin",
                                 Obesite==0 &  Maigre==0 ~ "normal"),
         Maternal.body  = factor( Maternal.body , levels = c("normal","thin","obese")),
         rickets = recode(rickets,
                               "1" = "yes",
                               "0" = "no")) %>%
  group_by(City) %>%
  mutate(Duree_2me_periode_z = (Duree_2me_periode-mean(Duree_2me_periode,na.rm = TRUE))/sd(Duree_2me_periode,na.rm = TRUE)) %>%
  ungroup()

data_laus <- data_com %>%
  filter(City=="Lausanne") 


data_basel <- data_com %>%
  filter(City=="Basel") 

### Episiotomy ###

explanatory = c( "SEP_comb_3a","rickets")
dependent = "Episiotomy"

Mod_laus_ep <- glm(Episiotomy ~  rickets,
                   data = data_laus, family="binomial")
Mod_basel_ep <- glm(Episiotomy ~  SEP_comb_3a ,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2 (dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
             title_text_size = 15,
             dependent_label="SEP and Rickets - Episiotomy")

# cowplot::save_plot("output/plot_body_uni_epi.pdf", plot_epi,base_height=4,base_width=14)
ggsave("output/Episiotomy/OR_epi_sr.png",  plot_epi,h=5,w=14)


### Forceps/CS ###

explanatory = c("SEP_comb_3a","rickets")
dependent = "Mecanisme_normal"

Mod_laus_me <- glm(Mecanisme_normal  ~ rickets,
                   data = data_laus, family="binomial")


Mod_basel_me <- glm(Mecanisme_normal  ~   SEP_comb_3a ,
                    data = data_basel, family="binomial")

plot_forc <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_me,glmfit2 = Mod_basel_me,
            title_text_size = 15,
            dependent_label="Rickets - Forceps/CS")

ggsave("output/ForcepsCS/OR_forc_sr.png",  plot_forc,h=5,w=14)


### Expulsion time ### 

explanatory = c( "SEP_comb_3a","rickets")
dependent = "Duree_2me_periode_z"

Mod_laus_or <- glm(Duree_2me_periode_z  ~  rickets,
                   data = data_laus)


Mod_basel_or <- glm(Duree_2me_periode_z  ~   SEP_comb_3a ,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2_lm(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="Rickets  - Expulsion phase in z-values")

ggsave("output/Duration/OR_dur_sr.png",  plot_forc,h=5,w=14)

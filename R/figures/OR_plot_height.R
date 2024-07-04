source("R/functions/function_or.R")
source("R/functions/function_lm.R")

# load data
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
         dura_terc = cut(Duree_2me_periode, breaks=c(quantile(Duree_2me_periode, c(0:3/3), na.rm = TRUE)),
                         labels=c("1st tercile","2nd tercile","3rd tercile"), include.lowest=TRUE),
         sex = factor( sex, levels = c("male", "female"))) %>%
  group_by(City) %>%
  mutate(Duree_2me_periode_z = (Duree_2me_periode-mean(Duree_2me_periode,na.rm = TRUE))/sd(Duree_2me_periode,na.rm = TRUE)) %>%
  ungroup()

data_laus <- data_com %>%
  filter(City=="Lausanne") 

data_basel <- data_com %>%
  filter(City=="Basel") 

### Episiotomy ###

explanatory = c( "sex","parity","Position_normal","height10","birthweight100","head_ConjExt", "dura_terc")
dependent = "Episiotomy"

Mod_laus_ep <- glm(Episiotomy ~  sex + parity + Position_normal + height10+birthweight100+  head_ConjExt +dura_terc,
                   data = data_laus, family="binomial")

Mod_basel_ep <- glm(Episiotomy ~  sex + parity + Position_normal + height10+birthweight100+   head_ConjExt +dura_terc,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
            title_text_size = 15,
            dependent_label="Episiotomy")

# cowplot::save_plot("output/plot_height_epi_nga_p.pdf", plot_epi,base_height=5,base_width=14)
ggsave("output/Episiotomy/OR_epi_h.png",  plot_epi,h=5,w=14)

table_data_laus_ep <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_ep)
write.xlsx(table_data_laus_ep ,paste0("output/Episiotomy/results_l_epi_h.xlsx"), rowNames=FALSE, overwrite = TRUE)

table_data_basel_ep <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_ep)
write.xlsx(table_data_basel_ep ,paste0("output/Episiotomy/results_b_epi_h.xlsx"), rowNames=FALSE, overwrite = TRUE)

### Forceps/CS ###
#Mecanisme_normal, 1 = nicht normal, 0 = normal Mecanisme_normal 


explanatory = c( "sex","parity","Position_normal", "head_circ", "Bassin_ConjExt")
dependent = "Mecanisme_normal"

Mod_laus_me <- glm(Mecanisme_normal  ~ sex + parity + Position_normal +  head_circ + Bassin_ConjExt,
                   data = data_laus, family="binomial")

Mod_basel_me <- glm(Mecanisme_normal  ~   sex + parity + Position_normal +   head_circ + Bassin_ConjExt,
                    data = data_basel, family="binomial")

plot_forc <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_me,glmfit2 = Mod_basel_me,
            title_text_size = 15,
            dependent_label="Forceps/CS")

# cowplot::save_plot("output/plot_mec_nga_p.pdf", plot_mec,base_height=5,base_width=14)
ggsave("output/ForcepsCS/OR_forc.png",  plot_forc,h=5,w=14)

table_data_laus_me <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_me)

write.xlsx(table_data_laus_me ,paste0("output/ForcepsCS/results_l_forc.xlsx"), rowNames=FALSE, overwrite = TRUE)

table_data_basel_me <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_me)

write.xlsx(table_data_basel_me ,paste0("output/ForcepsCS/results_b_forc.xlsx"), rowNames=FALSE, overwrite = TRUE)

### Expulsion time ### 

explanatory = c( "sex","parity","Position_normal","height10","birthweight100","head_ConjExt")
dependent = "Duree_2me_periode_z"

Mod_laus_or <- glm(Duree_2me_periode_z  ~  sex + parity + Position_normal+ height10+birthweight100+   head_ConjExt,
                   data = data_laus)

Mod_basel_or <- glm(Duree_2me_periode_z  ~   sex + parity + Position_normal + height10+birthweight100+   head_ConjExt,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2_lm(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="Expulsion phase in z-values")

# cowplot::save_plot("output/Duration/plot_height_du_nga_p.pdf", plot_du,base_height=5,base_width=14)
ggsave("output/Duration/OR_dur_h.png", plot_du,h=5,w=14)

table_data_laus_du <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_me)
write.xlsx(table_data_laus_du ,paste0("output/Duration/results_l_dur.xlsx"), rowNames=FALSE, overwrite = TRUE)


table_data_basel_du <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_me)
write.xlsx(table_data_basel_du,paste0("output/Duration/results_b_dur.xlsx"), rowNames=FALSE, overwrite = TRUE)
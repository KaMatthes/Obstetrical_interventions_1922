source("R/functions/function_or.R")
source("R/functions/function_lm.R")

data_com <- read.xlsx("data/data_birth.xlsx",detectDates = TRUE)  %>%
  mutate(
         position_normal=ifelse(position_normal==0,1,0),
         position_normal= as.factor(position_normal),
         episiotomy = as.factor(episiotomy),
         sex= as.factor(sex),
         stillbirth= as.factor(stillbirth),
         # sex=recode(sex, 
         #            "0" = "male",
         #            "1" ="female" ),
         position_normal = recode(position_normal,"0"="normal",
                                  "1" = "non-normal"),
         birthweight100 = birthweight/100,
         height10 = height/10,
         bassin_cretes10= bassin_cretes/10,
         head_terc = cut(head_circ, breaks=c(quantile( head_circ, c(0:3/3), na.rm = TRUE)),
                         labels=c("1","2","3"), include.lowest=TRUE),
         bassin_conjExt_terc = cut(bassin_conjExt, breaks=c(quantile( bassin_conjExt, c(0:3/3), na.rm = TRUE)),
                         labels=c("1","2","3"), include.lowest=TRUE),
         head_ConjExt = case_when(
          bassin_conjExt_terc==1 & head_terc==3 ~ "small-large",
          bassin_conjExt_terc==3 & head_terc==1 ~"large-small",
          bassin_conjExt_terc==1 & head_terc==1 ~"normal",
          bassin_conjExt_terc==2 & head_terc==2 ~"normal",
          bassin_conjExt_terc==3 & head_terc==3 ~"normal",
          bassin_conjExt_terc==1 & head_terc==2 ~"normal",
          bassin_conjExt_terc==2 & head_terc==1 ~"normal",
          bassin_conjExt_terc==2 & head_terc==3 ~"normal",
          bassin_conjExt_terc==3 & head_terc==2 ~"normal"),
          head_ConjExt  = factor( head_ConjExt , levels = c("normal","large-small","small-large")),
         dura_terc = cut(explusion, breaks=c(quantile(explusion, c(0:3/3), na.rm = TRUE)),
                         labels=c("1st tercile","2nd tercile","3rd tercile"), include.lowest=TRUE),
         sex = factor( sex, levels = c("male", "female"))) %>%
  group_by(city) %>%
  mutate(explusion_z = (explusion-mean(explusion,na.rm = TRUE))/sd(explusion,na.rm = TRUE)) %>%
  ungroup()


data_laus <- data_com %>%
  filter(city=="Lausanne") 


data_basel <- data_com %>%
  filter(city=="Basel") 



### Episiotomy ###
explanatory = c("head_ConjExt", "dura_terc")

dependent = "episiotomy"

Mod_laus_ep <- glm(episiotomy ~  head_ConjExt +dura_terc,
                   data = data_laus, family="binomial")

Mod_basel_ep <- glm(episiotomy ~   head_ConjExt +dura_terc,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2 (dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="episiotomy")

cowplot::save_plot("output/plot_height_uni_epi.pdf", plot_epi,base_height=5,base_width=14)



table_data_laus_ep <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_ep) 


write.xlsx(table_data_laus_ep ,paste0("output/table_data_laus_height_uni_ep.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_ep <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_ep) 


write.xlsx(table_data_basel_ep ,paste0("output/table_data_basel_height_uni_ep.xlsx"), rowNames=FALSE, overwrite = TRUE)

### Mecanisme_normal, 1 = nicht normal, 0 = normal Mecanisme_normal 


explanatory = c("head_circ")

dependent = "mecanisme_normal"

Mod_laus_me <- glm(mecanisme_normal  ~  head_circ,
                   data = data_laus, family="binomial")


Mod_basel_me <- glm(mecanisme_normal  ~   head_circ ,
                    data = data_basel, family="binomial")

plot_mec <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_me,glmfit2 = Mod_basel_me,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="Forceps/CS")

cowplot::save_plot("output/plot_uni_mec.pdf", plot_mec,base_height=5,base_width=14)




table_data_laus_me <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_me) 


write.xlsx(table_data_laus_me ,paste0("output/table_data_laus_uni_me.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_me <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_me) 


write.xlsx(table_data_basel_me ,paste0("output/table_data_basel_uni_me.xlsx"), rowNames=FALSE, overwrite = TRUE)

# Dauer der Ausbreitung explusion_z


explanatory = c( "head_ConjExt")


dependent = "explusion_z"

Mod_laus_or <- glm(explusion_z  ~   head_ConjExt,
                   data = data_laus)


Mod_basel_or <- glm(explusion_z  ~     head_ConjExt,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="Expulsion phase in z-values")

cowplot::save_plot("output/plot_height_uni_du.pdf", plot_du,base_height=5,base_width=14)



table_data_laus_du <- data_laus %>%
  finalfit(dependent, explanatory,glmfit = Mod_laus_me) 


write.xlsx(table_data_laus_du ,paste0("output/table_data_laus_height_uni_du.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_du <- data_basel %>%
  finalfit(dependent, explanatory,glmfit = Mod_basel_me) 


write.xlsx(table_data_basel_du ,paste0("output/table_data_basel_height_uni_du.xlsx"), rowNames=FALSE, overwrite = TRUE)


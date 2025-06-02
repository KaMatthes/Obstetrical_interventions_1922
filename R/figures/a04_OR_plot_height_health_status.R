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
          sex = factor( sex, levels = c("male", "female")),
         sep = as.factor(sep),
         sep = recode(sep,
                              "0" = "medium",
                              "9" = "medium",
                              "2" = "medium",
                              "1" = "low",
                              "3" = "high"),
         sep = factor(sep, levels=c("low", "medium", "high")),
         health_status = ifelse(health_status == 1 & rickets ==1, 2, health_status),
         health_status = as.factor(health_status),
         health_status = recode(health_status,
                              "1" = "healthy",
                              "2" = "medium",
                              "3" = "unhealthy"),
         health_status = factor(health_status, levels=c("healthy", "medium", "unhealthy")),
         dura_terc = cut(expulsion, breaks=c(quantile(expulsion, c(0:3/3), na.rm = TRUE)),
                         labels=c("1st tercile","2nd tercile","3rd tercile"), include.lowest=TRUE)) %>%
  group_by(city) %>%
  mutate(expulsion_z = (expulsion-mean(expulsion,na.rm = TRUE))/sd(expulsion,na.rm = TRUE)) %>%
  ungroup()


data_laus <- data_com %>%
  filter(city=="Lausanne") 


data_basel <- data_com %>%
  filter(city=="Basel") 

# data_com_reduced <-  data_com %>%
#   select('bassin_cretes', 'height','birthweight','head_ConjExt', "head_circ", "bassin_conjExt", "parity", "etatgeneral","sep") %>%
#   mutate(head_ConjExt= as.numeric(head_ConjExt),
#          etatgeneral= as.numeric(etatgeneral),
#          sep= as.numeric(sep))
# 
# 
# cor_mat <- round(cor(data_com_reduced, use="pairwise.complete.obs"), 2)

# Episiotomy

#table

explanatory = c( "sep","health_status","sex","parity","position_normal","height10","birthweight100",
                 "GA_weeks","head_ConjExt","dura_terc")

dependent = "episiotomy"

Mod_laus_ep <- glm(episiotomy ~  health_status+sex + parity + position_normal + height10+birthweight100+ 
                     GA_weeks +  head_ConjExt + dura_terc,
                   data = data_laus, family="binomial")

Mod_basel_ep <- glm(episiotomy ~  sep +sex + parity + position_normal  + height10+birthweight100+ 
                    GA_weeks +   head_ConjExt + dura_terc,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="episiotomy")

cowplot::save_plot("output/plot_etatgeneral_epi.pdf", plot_epi,base_height=7,base_width=14)

table_data_laus_ep <- data_laus %>%
  finalfit(dependent, explanatory[-1],glmfit = Mod_laus_ep) 


write.xlsx(table_data_laus_ep ,paste0("output/table_data_laus_etatgeneral_ep.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_ep <- data_basel %>%
  finalfit(dependent, explanatory[-2],glmfit = Mod_basel_ep) 


write.xlsx(table_data_basel_ep ,paste0("output/table_data_basel_etatgeneral_ep.xlsx"), rowNames=FALSE, overwrite = TRUE)

### Mecanisme_normal, 1 = nicht normal, 0 = normal Mecanisme_normal 


explanatory = c("sep","health_status", "sex","parity","position_normal",
                 "GA_weeks", "head_circ", "bassin_conjExt")

dependent = "mecanisme_normal"

Mod_laus_me <- glm(mecanisme_normal  ~ health_status + sex + parity + position_normal  + 
                     GA_weeks +  head_circ + bassin_conjExt,
                   data = data_laus, family="binomial")


Mod_basel_me <- glm(mecanisme_normal  ~   sep + sex + parity + position_normal + 
                      GA_weeks +  head_circ + bassin_conjExt,
                    data = data_basel, family="binomial")

plot_mec <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_me,glmfit2 = Mod_basel_me,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="Forceps/CS")

cowplot::save_plot("output/plot_mec_etatgeneral.pdf", plot_mec,base_height=7,base_width=14)




table_data_laus_me <- data_laus %>%
  finalfit(dependent, explanatory[-1],glmfit = Mod_laus_me) 


write.xlsx(table_data_laus_me ,paste0("output/table_data_laus_etatgeneral_me.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_me <- data_basel %>%
  finalfit(dependent, explanatory[-2],glmfit = Mod_basel_me) 


write.xlsx(table_data_basel_me ,paste0("output/table_data_basel_etatgeneral_me.xlsx"), rowNames=FALSE, overwrite = TRUE)

# Dauer der Ausbreitung expulsion_z


explanatory = c( "sep","health_status","sex","parity","position_normal","height10","birthweight100",
                 "GA_weeks","head_ConjExt")


dependent = "expulsion_z"

Mod_laus_or <- glm(expulsion_z  ~  health_status +sex + parity + position_normal  + height10+birthweight100+ 
                     GA_weeks +   head_ConjExt,
                   data = data_laus)


Mod_basel_or <- glm(expulsion_z  ~   sep + sex + parity + position_normal  + height10+birthweight100+ 
                      GA_weeks +   head_ConjExt,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="Expulsion phase in z-values")

cowplot::save_plot("output/plot_etatgeneral_du.pdf", plot_du,base_height=7,base_width=14)



table_data_laus_du <- data_laus %>%
  finalfit(dependent, explanatory[-1],glmfit = Mod_laus_me) 


write.xlsx(table_data_laus_du ,paste0("output/table_data_laus_etatgeneral_du.xlsx"), rowNames=FALSE, overwrite = TRUE)



table_data_basel_du <- data_basel %>%
  finalfit(dependent, explanatory[-2],glmfit = Mod_basel_me) 


write.xlsx(table_data_basel_du ,paste0("output/table_data_basel_etatgeneral_du.xlsx"), rowNames=FALSE, overwrite = TRUE)



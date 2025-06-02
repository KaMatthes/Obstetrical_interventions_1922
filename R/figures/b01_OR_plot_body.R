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
         Maternal.body=case_when(obesity==1 ~ "obese",
                                 thin==1 ~ "thin",
                                 obesity==0 &  thin==0 ~ "normal"),
         Maternal.body  = factor( Maternal.body , levels = c("normal","thin","obese"))) %>%
  group_by(city) %>%
  mutate(explusion_z = (explusion-mean(explusion,na.rm = TRUE))/sd(explusion,na.rm = TRUE)) %>%
  ungroup()

data_laus <- data_com %>%
  filter(city=="Lausanne") 


data_basel <- data_com %>%
  filter(city=="Basel") 

### Episiotomy ###

explanatory = c( "sep","Maternal.body")

dependent = "episiotomy"

Mod_laus_ep <- glm(episiotomy ~  Maternal.body,
                   data = data_laus, family="binomial")

Mod_basel_ep <- glm(episiotomy ~  sep ,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2 (dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="SEP and Maternal body - Episiotomy")

# cowplot::save_plot("output/plot_body_uni_epi.pdf", plot_epi,base_height=4,base_width=14)
ggsave("output/Episiotomy/OR_epi_sb.png",  plot_epi,h=5,w=14)


### Forceps/CS ###

explanatory = c("sep","Maternal.body")

dependent = "mecanisme_normal"

Mod_laus_me <- glm(mecanisme_normal  ~ Maternal.body,
                   data = data_laus, family="binomial")

Mod_basel_me <- glm(mecanisme_normal  ~   sep ,
                    data = data_basel, family="binomial")

plot_forc <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_me,glmfit2 = Mod_basel_me,
            title_text_size = 15,
            # breaks = c(0.0, 0.2,0.4,0.6, 0.8, 1.0, 1.2, 1.4,1.6, 1.8,2.0,2.2,2.4,2.6,2.8,3.0),
            dependent_label="SEP and Maternal body - Forceps/CS")
# 
# cowplot::save_plot("output/plot_body_uni_mec.pdf", plot_mec,base_height=7,base_width=14)
ggsave("output/ForcepsCS/OR_forc_sb.png",  plot_forc,h=5,w=14)


### Expulsion time ### 

explanatory = c( "sep","Maternal.body")


dependent = "explusion_z"

Mod_laus_or <- glm(explusion_z  ~  Maternal.body ,
                   data = data_laus)

Mod_basel_or <- glm(explusion_z  ~   sep ,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2_lm(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="SEP and Maternal body - Expulsion phase in z-values")

# cowplot::save_plot("output/plot_body_uni_du.pdf", plot_du,base_height=7,base_width=14)
ggsave("output/Duration/OR_dur_sb.png", plot_du,h=5,w=14)


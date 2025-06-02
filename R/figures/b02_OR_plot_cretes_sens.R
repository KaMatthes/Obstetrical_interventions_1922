source("R/functions/function_or.R")
source("R/functions/function_lm.R")

# load data
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
    dura_terc = cut(expulsion, breaks=c(quantile(expulsion, c(0:3/3), na.rm = TRUE)),
                    labels=c("1st tercile","2nd tercile","3rd tercile"), include.lowest=TRUE),
    sex = factor( sex, levels = c("male", "female"))) %>%
  group_by(city) %>%
  mutate(expulsion_z = (expulsion-mean(expulsion,na.rm = TRUE))/sd(expulsion,na.rm = TRUE)) %>%
  ungroup()

data_laus <- data_com %>%
  filter(city=="Lausanne") 

data_basel <- data_com %>%
  filter(city=="Basel") 

### episiotomy ###

explanatory = c( "sex","parity","position_normal","bassin_cretes","birthweight100","head_ConjExt", "dura_terc")
dependent = "episiotomy"

Mod_laus_ep <- glm(episiotomy ~  sex + parity + position_normal + bassin_cretes+birthweight100+  head_ConjExt +dura_terc,
                   data = data_laus, family="binomial")

Mod_basel_ep <- glm(episiotomy ~  sex + parity + position_normal + bassin_cretes+birthweight100+   head_ConjExt +dura_terc,
                    data = data_basel, family="binomial")

plot_epi <- data_com %>%
  or_plot_2(dependent,explanatory, glmfit = Mod_laus_ep,glmfit2 = Mod_basel_ep,
            title_text_size = 15,
            dependent_label="Bassin Cretes - Episiotomy")

# cowplot::save_plot("output/plot_height_epi_nga_p.pdf", plot_epi,base_height=5,base_width=14)
ggsave("output/Episiotomy/OR_epi_c.png",  plot_epi,h=5,w=14)


### Expulsion time ### 

dependent = "expulsion_z"

explanatory = c( "sex","parity","position_normal","bassin_cretes","birthweight100","head_ConjExt")

Mod_laus_or <- glm(expulsion_z  ~  sex + parity + position_normal +  bassin_cretes+birthweight100 +   head_ConjExt,
                   data = data_laus)


Mod_basel_or <- glm(expulsion_z  ~   sex + parity + position_normal +  bassin_cretes+birthweight100+  head_ConjExt,
                    data = data_basel)

plot_du <- data_com %>%
  or_plot_2_lm(dependent,explanatory, glmfit = Mod_laus_or,glmfit2 = Mod_basel_or,
            title_text_size = 15,
            dependent_label="Bassin Cretes - Expulsion phase in z-values")

# cowplot::save_plot("output/plot_cretes_du.pdf", plot_du,base_height=5,base_width=14)

ggsave("output/Duration/OR_dur_c.png", plot_du,h=5,w=14)

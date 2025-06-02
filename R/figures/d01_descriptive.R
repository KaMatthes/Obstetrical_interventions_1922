data_com <- read.xlsx("data/data_birth.xlsx",detectDates = TRUE)  %>%
  mutate(
    position_normal= as.factor(position_normal),
    position_normal = recode(position_normal,"0"="normal",
                             "1" = "non-normal"),
    episiotomy = as.factor(episiotomy),
    sex= as.factor(sex),
    stillbirth= as.factor(stillbirth),
    # sex=recode(sex, 
    #            "0" = "male",
    #            "1" ="female" ),

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

dt <- data_com %>%
  select(stillbirth,episiotomy,mecanisme_normal,expulsion_z,city) %>%
  mutate(mecanisme_normal = as.factor(mecanisme_normal))
  

dt_l <- dt %>%
  filter(city %in% "Lausanne")

ctable(x = dt_l$stillbirth, 
       y = dt_l$mecanisme_normal,
       prop = "r")



dt_l <- dt %>%
  filter(city %in% "Basel")

ctable(x = dt_l$stillbirth, 
       y = dt_l$episiotomy,
       prop = "r")

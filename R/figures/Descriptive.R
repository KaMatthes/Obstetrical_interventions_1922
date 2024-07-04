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

dt <- data_com %>%
  select(stillbirth,Episiotomy,Mecanisme_normal,Duree_2me_periode_z,City) %>%
  mutate(Mecanisme_normal = as.factor(Mecanisme_normal))
  

dt_l <- dt %>%
  filter(City %in% "Lausanne")

ctable(x = dt_l$stillbirth, 
       y = dt_l$Mecanisme_normal,
       prop = "r")



dt_l <- dt %>%
  filter(City %in% "Basel")

ctable(x = dt_l$stillbirth, 
       y = dt_l$Episiotomy,
       prop = "r")

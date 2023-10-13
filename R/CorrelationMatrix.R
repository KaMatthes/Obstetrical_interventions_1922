data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE) 

data_l <- data_com %>%
  filter(City=="Lausanne")

data_b <- data_com %>%
  filter(City=="Basel")


cor(data_l$height,data_l$Bassin_Cretes, use = "complete")
cor(data_b$height,data_b$Bassin_Cretes, use = "complete")


data_com_reduced <-  data_com%>%
  select('Bassin_Cretes', 'Bassin_Epines', 'height', 'Bassin_ConjExt', 'head_circ','birthweight')


cor_mat <- round(cor(data_com_reduced, use="pairwise.complete.obs"), 2)

library(corrplot)

corrplot(cor_mat, method="circle", diag=FALSE,tl.col="black")




#Von Rosa unten: 

laus_Rosa_df <- as.data.frame(laus_Rosa)
laus_Rosa_df_limited_var <-  laus_Rosa_df%>%
  select('age_mother', 'civil_status', 'Religion', 'Lausanne', 'birthyear', 'birthmonth', 'parity', 'menarche',  'height', 'waist_circ', 'sex', 'stillbirth', 'babylength', 
         'birthweight', 'placentaweight', 'head_circ', 'postbirth_death', 'feeding',
         'Obesite', 'Maigre', 'Goitre', 'Infection', 'Etat_general_cat', 'hisco', 'hisco_class_12', 'hisco_class_3', 'LBW', 'GA_weeks', 'PTB2', 'BW_r_PW', 'rickets', 'age_baby_cat')


laus_Rosa_df_limited_var$civil_status <- as.numeric(factor(laus_Rosa_df_limited_var$civil_status, exclude = NA))
laus_Rosa_df_limited_var$Religion <- as.numeric(factor(laus_Rosa_df_limited_var$Religion, exclude = NA))
laus_Rosa_df_limited_var$Lausanne <- as.numeric(factor(laus_Rosa_df_limited_var$Lausanne, exclude = NA))
laus_Rosa_df_limited_var$sex <- as.numeric(factor(laus_Rosa_df_limited_var$sex, exclude = NA))
laus_Rosa_df_limited_var$stillbirth <- as.numeric(factor(laus_Rosa_df_limited_var$stillbirth, exclude = NA))
laus_Rosa_df_limited_var$postbirth_death <- as.numeric(factor(laus_Rosa_df_limited_var$postbirth_death, exclude = NA))
laus_Rosa_df_limited_var$feeding <- as.numeric(factor(laus_Rosa_df_limited_var$feeding, exclude = NA))
laus_Rosa_df_limited_var$Obesite <- as.numeric(factor(laus_Rosa_df_limited_var$Obesite, exclude = NA))
laus_Rosa_df_limited_var$Maigre <- as.numeric(factor(laus_Rosa_df_limited_var$Maigre, exclude = NA))
laus_Rosa_df_limited_var$Goitre <- as.numeric(factor(laus_Rosa_df_limited_var$Goitre, exclude = NA))
laus_Rosa_df_limited_var$Infection <- as.numeric(factor(laus_Rosa_df_limited_var$Infection, exclude = NA))
laus_Rosa_df_limited_var$Etat_general_cat <- as.numeric(factor(laus_Rosa_df_limited_var$Etat_general_cat, exclude = NA))
laus_Rosa_df_limited_var$hisco <- as.numeric(factor(laus_Rosa_df_limited_var$hisco, exclude = NA))
laus_Rosa_df_limited_var$hisco_class_12 <- as.numeric(factor(laus_Rosa_df_limited_var$hisco_class_12, exclude = NA))
laus_Rosa_df_limited_var$hisco_class_3 <- as.numeric(factor(laus_Rosa_df_limited_var$hisco_class_3, exclude = NA))
laus_Rosa_df_limited_var$LBW <- as.numeric(factor(laus_Rosa_df_limited_var$LBW, exclude = NA))
laus_Rosa_df_limited_var$PTB2 <- as.numeric(factor(laus_Rosa_df_limited_var$PTB2, exclude = NA))
laus_Rosa_df_limited_var$rickets <- as.numeric(factor(laus_Rosa_df_limited_var$rickets, exclude = NA))
laus_Rosa_df_limited_var$age_baby_cat <- as.numeric(factor(laus_Rosa_df_limited_var$age_baby_cat, exclude = NA))


# Calculate the correlation matrix
cor_mat <- round(cor(laus_Rosa_df_limited_var, use="pairwise.complete.obs"), 2)
# print(cor_mat)

corrplot(cor_mat, method="circle", diag=FALSE,tl.col="black")
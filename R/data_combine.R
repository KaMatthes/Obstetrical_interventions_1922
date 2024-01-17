data_laus <- read.xlsx("data/laus_cleaned_2023-01-23.xlsx",detectDates = TRUE) %>%
  mutate(City="Lausanne") %>%
  select(-agemother_cat,-menarche_cat,-parity_cat,-LBW, -GA_days, -GA_weeks_cat,-age_baby_cat) %>%
  mutate( Bassin_Epines = as.numeric( Bassin_Epines),
          Bassin_Cretes = as.numeric(Bassin_Cretes),
          Bassin_Trochanters = as.numeric(Bassin_Trochanters),
          Bassin_ConjExt = as.numeric(Bassin_ConjExt),
          Duree_1re_periode = as.numeric(Duree_1re_periode),
          Duree_2me_periode = as.numeric(Duree_2me_periode),
          Duree_3me_periode = as.numeric(Duree_3me_periode), 
          Position_normal = ifelse(Position=="OIGA" | Position=="OIDA", 1, 0),
          # Mecanisme = ifelse(Mecanisme=="NULL", "normal", Mecanisme),
          Mecanisme_normal = ifelse(Mecanisme =="forceps" | Mecanisme =="cesarienne"  , 1, 0),
          Mecanisme_n = 1) %>%
  pivot_wider(names_from=Mecanisme,  values_from = Mecanisme_n, values_fill = 0)  %>%
  rename(Normal = normal,
         Episiotomy = episiotomie,
         Ceasarean = cesarienne,
         Extraction = extraction,
         Forceps= forceps) %>%
  mutate(Normal = as.factor(Normal),
         Episiotomy  = as.factor(Episiotomy ),
         Ceasarean = as.factor(Ceasarean),
         Forceps = as.factor(Forceps),
         Extraction = as.factor( Extraction),
         sex=recode(sex, "0"="male",
                    "1" ="female"))
  

data_basel <- read.xlsx("data/Daten_Basel_new.xlsx",detectDates = TRUE) %>%
  filter(Jahr >= 1921 & Jahr <= 1924) %>%
  filter(TWIN =="no") %>%
  mutate(City= "Basel",
         DauerEroeffnungH = DauerEroeffnungH *60,
         Position_normal = ifelse(Presentation=="occ-post" | Presentation=="occ-post2", 1, 0),
         Mecanisme_normal = ifelse(Ceasarean ==1 | Forceps ==1, 1, 0) ,
         # Normal = Mecanisme_normal,
         # Ceasarean = recode(Ceasarean, "yes" = "1","no" = ""0") ,    
         # Forceps = recode(Forceps, "yes" = "1","no" = "0"),
         # Episiotomy = recode(Episiotomy, "yes" = "1","no" = "0"),
         # Normal = as.factor(Normal),
         Episiotomy = ifelse(Episiotomy==1 & Forceps==1, 0, Episiotomy),
         Episiotomy  = as.factor(Episiotomy ),
         Ceasarean = as.factor(Ceasarean),
         Forceps = as.factor(Forceps)) %>%
  rename(birthweight = Birthweight,
         placentaweight = Plazentaweight,
         Bassin_ConjExt = Conjugata_Ext,
         Bassin_Cretes = DistCristar,
         Bassin_Epines = DistSpinae,
         age_mother =  Age, 
         GA_weeks = Gest_weeks,
         parity = Para,
         Profession = Stand_Beruf,
         menarche = Menarche,
         height = Stature,
         birthdate = TagGeburtK,
         sex =  SEX,
         babylength = Birthlenght,
         stillbirth = Live_Birth,
         civil_status = State_of_life,
         head_circ = Head_circumference,
         Duree_1re_periode = DauerEroeffnungH,
         Duree_2me_periode = DauerAustreibungMIN) %>%
  select(-AgeCategory,-GestCategory, -Childcount_proGebtermin, -Nachname_1,  -Nachname_2,
                -MutterFamname_Alternativschreibw, -Vorname, -Geburtsdatum_Mutter, - Adresse, -Hausnummer,
                - DummyGirl,-DummyStillbirth, -Name_Vater, -Vorname_Vater, - AdresseV,-parKat, -Presentation2,
                -AgeCategory2, -year2)


data_com <- data_laus %>%
  bind_rows(data_basel) %>%
  mutate(Bassin_Epines = ifelse(Bassin_Epines==0, NA, Bassin_Epines),
         Bassin_Cretes = ifelse(Bassin_Cretes==0, NA, Bassin_Cretes),
         Bassin_Trochanters = ifelse(Bassin_Trochanters==0, NA, Bassin_Trochanters),
         Bassin_ConjExt = ifelse(Bassin_ConjExt==0, NA, Bassin_ConjExt),
         stillbirth = recode(stillbirth,
                             "no" = "1",
                             "yes" = "0"),
         GA_weeks = ifelse(is.na(GA_weeks), age_baby_final, GA_weeks),
         GA_weeks = ifelse(GA_weeks=="a terme", 40, GA_weeks),
         GA_weeks = as.numeric(GA_weeks),
         GA_weeks_cat=cut(as.numeric(GA_weeks), breaks=c(10, 33, 37, 41, 52), include.lowest = TRUE,  right = FALSE))
         # Bassin_ConjExt = ifelse(Bassin_ConjExt>50, NA, Bassin_ConjExt))
         # 

write.xlsx(data_com, "data/data_com.xlsx")
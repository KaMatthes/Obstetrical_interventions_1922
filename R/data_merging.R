data_nora <- read.xlsx("data/20221219_Nora_Daten.xlsx") %>%
  select(Keyid, `Exclude.Period`, Bassin_Epines,Bassin_Cretes,Bassin_Trochanters,Bassin_ConjExt,Mecanisme,Position,Duree_1re_periode,
         Duree_2me_periode,Duree_3me_periode, Comment)

data_org <- read.csv("data/20220123.csv", sep=";") %>%
  select( -Bassin_Epines, -Bassin_Cretes, -Bassin_Trochanters,-Bassin_ConjExt,-Mecanisme,-Position,-Duree_1re_periode,
                 -Duree_2me_periode,-Duree_3me_periode, -Comment)

data_nora <- data_nora %>%
  left_join(data_org) %>%
  mutate(DateAccouchement = ymd(as.Date(DateAccouchement, "%d.%m.%Y")),
         Date  = ymd(as.Date(Date , "%d.%m.%Y")),
         DernieresRegles  = ymd(as.Date(DernieresRegles , "%d.%m.%Y")))

write.xlsx(data_nora, "data/20230123_Nora_Daten.xlsx")


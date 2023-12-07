source("R/function_new.R")
source("R/function_new_lm.R")

data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE)  %>%
  mutate(Mecanisme_normal=ifelse(Mecanisme_normal==0,1,0),
         Mecanisme_normal= as.factor(Mecanisme_normal),
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
         height_weight = height/birthweight,
         height_weight_quan = cut(height_weight, breaks=c(quantile(height_weight, probs = c(seq(0,1, by=0.25)), na.rm = TRUE)), 
                                  labels=c("1Q","2Q","3Q","4Q"), include.lowest=TRUE),
         height_weight_quan  = factor( height_weight_quan , levels = c("1Q","2Q","3Q","4Q")),
         height_cretes = height10/Bassin_Cretes10,
         height_cretes_quan = cut(height_cretes, breaks=c(quantile( height_cretes, probs = c(seq(0,1, by=0.25)), na.rm = TRUE)), 
                                  labels=c("1Q","2Q","3Q","4Q"), include.lowest=TRUE),
         height_cretes_quan  = factor(height_cretes_quan , levels = c("1Q","2Q","3Q","4Q")),
         head_Bassin_ConjExt = head_circ/Bassin_ConjExt,
         head_Bassin_ConjExt_quan = cut(head_Bassin_ConjExt, breaks=c(quantile( head_Bassin_ConjExt, probs = c(seq(0,1, by=0.25)), na.rm = TRUE)), 
                                        labels=c("1Q","2Q","3Q","4Q"), include.lowest=TRUE),
         head_Bassin_ConjExt_quan  = factor(head_Bassin_ConjExt_quan, levels = c("1Q","2Q","3Q","4Q")),
         sex = factor( sex, levels = c("male", "female"))) %>%
  group_by(City) %>%
  mutate(Duree_2me_periode_z = (Duree_2me_periode-mean(Duree_2me_periode,na.rm = TRUE))/sd(Duree_2me_periode,na.rm = TRUE)) %>%
  ungroup()

data_laus <- data_com %>%
  filter(City=="Lausanne") 


data_basel <- data_com %>%
  filter(City=="Basel") 


Scatter1 <- ggplot(data=data_com, aes(x=Bassin_ConjExt, y=head_circ, shape=head_Bassin_ConjExt_quan, color=head_Bassin_ConjExt_quan))+
  geom_point()+
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))+
  facet_grid(cols = vars(City))+
  theme(legend.position="bottom")
Scatter1


Scatter2 <- ggplot(data=data_com, aes(x=Bassin_Cretes, y=height, shape=height_cretes_quan, color=height_cretes_quan))+
  geom_point()+
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))+
  facet_grid(cols = vars(City))+
  theme(legend.position="bottom")
Scatter2


Scatter3 <- ggplot(data=data_com, aes(x=birthweight, y=height, shape=height_weight_quan, color=height_weight_quan))+
  geom_point()+
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))+
  facet_grid(cols = vars(City))+
  theme(legend.position="bottom")
Scatter3

plot_together <- cowplot::plot_grid(Scatter1,Scatter2,Scatter3,
                                    ncol=1, nrow=3,rel_heights = c(1,.7), align="hv")

cowplot::save_plot("output/Ratios.pdf", plot_together ,base_height=14,base_width=10)  





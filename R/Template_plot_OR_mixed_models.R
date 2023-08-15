data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE)  %>%
  mutate(Mecanisme_normal=ifelse(Mecanisme_normal==0,1,0),
         Mecanisme_normal= as.factor(Mecanisme_normal),
         Position_normal=ifelse(Position_normal==0,1,0),
         Position_normal= as.factor(Position_normal),
         Episiotomy = as.factor(Episiotomy))


data_laus <- data_com %>%
  filter(City=="Lausanne") 


data_basel <- data_com %>%
  filter(City=="Basel") 

### Episiotomy, 1= ja, 0 = nein
# co-factoren ergänzen oder ändern, nur Beispielhaft welche genommen 



Mod_ep <- data.frame(summary(glmmPQL(Episiotomy ~  head_circ + birthweight +Bassin_Epines +Bassin_Cretes +Bassin_ConjExt, ~1|City, 
                     data = data_com, family="binomial"(link="logit")))$tTable) %>%
  mutate(Est = round(exp(Value),2),
         CIl = round(exp(Value - 1.96*Std.Error),2),
         CIu = round(exp(Value + 1.96*Std.Error),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(outcome="Episiotomy")

### Mecanisme_normal, 1 = nicht normal, 0 = normal
# co-factoren ergänzen oder ändern, nur Beispielhaft welche genommen 



Mod_me <- data.frame(summary(glmmPQL(Mecanisme_normal ~  head_circ + birthweight +Bassin_Epines +Bassin_Cretes +Bassin_ConjExt, ~1|City, 
                                     data = data_com, family="binomial"(link="logit")))$tTable) %>%
  mutate(Est = round(exp(Value),2),
         CIl = round(exp(Value - 1.96*Std.Error),2),
         CIu = round(exp(Value + 1.96*Std.Error),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(outcome="Mecanisme")



### Position_normal, 1 = nicht normal, 0 = normal
# co-factoren ergänzen oder ändern, nur Beispielhaft welche genommen 



Mod_po <- data.frame(summary(glmmPQL(Position_normal ~  head_circ + birthweight +Bassin_Epines +Bassin_Cretes +Bassin_ConjExt, ~1|City, 
                                     data = data_com, family="binomial"(link="logit")))$tTable) %>%
  mutate(Est = round(exp(Value),2),
         CIl = round(exp(Value - 1.96*Std.Error),2),
         CIu = round(exp(Value + 1.96*Std.Error),2),
         Fac = row.names(.)) %>%
  filter(!Fac =="(Intercept)") %>%
  filter(!Fac =="birthweight") %>%
  select(Fac, Est, CIl,CIu) %>%
  mutate(outcome="Position")



# Plot

data_plot <- rbind(Mod_ep,
                   Mod_me,
                   Mod_po) %>%
  mutate(Fac=recode(Fac,
                    "Bassin_ConjExt" = "Bassin ConjExt",
                    "Bassin_Cretes" = "Bassin Cretes",
                    "Bassin_Epines" = "Bassin Epines",
                    "head_circ" = "Head circumference"))


OR_plot <- ggplot( data_plot , aes(x=forcats::fct_rev(Fac),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu),lwd=lwd_size,position=pd,fatten=fatten_size)+
  facet_grid(~outcome)+
  # ylim(c(0,2.5))+
  labs(x="",y="OR") +
  # guides(color = guide_legend(override.aes = list(size = 1.5)))+
  ggtitle("Mixed Effect Models (city included as random effect ")+
  # scale_color_manual(" ",
  #                    breaks=c("Lausanne","Basel"),
  #                    # labels=c("Age >=40 (Ref)","Age <40"),
  #                    values = c(mypalette[3],mypalette[2]))+
  theme_bw()+
  theme(aspect.ratio=1,
        strip.text = element_text(color="black",size= strip_text),
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=size_legend_text),
        legend.title= element_blank(),
        legend.position = "bottom") +
  coord_flip() 

cowplot::save_plot("output/OR_mixed_effects_plot.pdf", OR_plot,base_height=8,base_width=20)  



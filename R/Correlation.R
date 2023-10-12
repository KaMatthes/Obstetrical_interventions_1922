data_com <- read.xlsx("data/data_com.xlsx",detectDates = TRUE) 

data_l <- data_com %>%
  filter(City=="Lausanne")

data_b <- data_com %>%
  filter(City=="Basel")

plot_height_Epines <- ggplot(data=data_com) +
  geom_point(aes(x=height, y=Bassin_Epines, col=City)) +
  geom_smooth(aes(x=height, y=Bassin_Epines, col=City),method = lm) +
  
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))
plot_height_Epines

cor(data_l$height,data_l$Bassin_Epines, use = "complete")
cor(data_b$height,data_b$Bassin_Epines, use = "complete")
summary(lm(Bassin_Epines ~ height + City, data=data_com))

model1 <- lmer(Bassin_Epines ~ height + (1|City),data=data_com,REML=TRUE)
summary(model1)
ranova(model1)

# model2 <- lme(Bassin_Epines ~ height, random=~1|City,data=data_com,method="REML",na.action=na.exclude)
# Anova(model2)


plot_height_Cretes <- ggplot(data=data_com) +
  geom_point(aes(x=height, y=Bassin_Cretes, col=City)) +
  geom_smooth(aes(x=height, y=Bassin_Cretes, col=City),method = lm) +
  
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))
plot_height_Cretes

cor(data_l$height,data_l$Bassin_Cretes, use = "complete")
cor(data_b$height,data_b$Bassin_Cretes, use = "complete")
summary(lm(Bassin_Cretes ~ height + City, data=data_com))


model2 <- lmer(Bassin_Cretes ~ height + (1|City),data=data_com,REML=TRUE)
summary(model2)
ranova(model2)


plot_height_ConjExt <- ggplot(data=subset(data_com, Bassin_ConjExt<150))+
  geom_point(aes(x=height, y=Bassin_ConjExt, col=City)) +
  geom_smooth(aes(x=height, y=Bassin_ConjExt, col=City),method = lm) +
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))
plot_height_ConjExt

cor(data_l$height,data_l$Bassin_ConjExt, use = "complete")
cor(data_b$height,data_b$Bassin_ConjExt, use = "complete")
summary(lm(Bassin_ConjExt ~ height + City, data=data_com))

plot_height_ConjExt <- ggplot(data=data_com) +
    geom_point(aes(x=Bassin_ConjExt, y=head_circ, col=City)) +
  geom_smooth(aes(x=Bassin_ConjExt, y=head_circ, col=City),method = lm) +
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))
plot_height_ConjExt


plot_height_ConjExt <- ggplot(data=data_com) +
  geom_histogram(aes(x=height,col=City, fill=City), binwidth = 1, alpha=0.5)


plot_height_ConjExt <- ggplot(data=data_com) +
  geom_boxplot(aes(y=height,col=City))




+
  geom_smooth(aes(x=height, y=head_circ, col=City),method = lm) +
  
  theme_bw()+
  theme(
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=15),
    legend.text = element_text(size=15),
    legend.title = element_text(size=15),
    legend.position = c(0.2,0.9))

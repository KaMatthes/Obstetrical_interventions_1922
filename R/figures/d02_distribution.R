data_com <- read.xlsx("data/data_birth.xlsx",detectDates = TRUE) 

density_plot_age <- ggplot() +
  geom_density(data = data_com, aes(x=age_mother, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Age of mother (year)")+
  ggtitle("Age of the mother")+
  theme_bw()+
  theme(aspect.ratio=1,
    axis.text=element_text(color="black",size= 15),
    axis.title=element_text(size= 18),
    plot.title = element_text(size=18),
    legend.text=element_text(size=18),
    legend.position = c(0.15,0.9))


density_plot_height <- ggplot() +
  geom_density(data = data_com, aes(x=height, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Height of mother (cm)")+
  ggtitle("Height of mother")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))



density_plot_head <- ggplot() +
  geom_density(data = data_com, aes(x=head_circ, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Head circumference (cm)")+
  ggtitle("Head circumference")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))


density_plot_ConjExt <- ggplot() +
  geom_density(data = data_com, aes(x=Bassin_ConjExt, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Conjugata Externa (cm)")+
  ggtitle("Conjugata Externa")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))


density_plot_Cretes <- ggplot() +
  geom_density(data = data_com, aes(x=Bassin_Cretes, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Bassin Cretes (cm)")+
  ggtitle("Bassin Cretes")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))


density_plot_birthweight <- ggplot() +
  geom_density(data = data_com, aes(x=birthweight, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Birthweight in (gr)")+
  ggtitle("Birthweight")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))


density_plot_GA <- ggplot() +
  geom_density(data = data_com, aes(x=GA_weeks, col=City), lwd=1.5) +
  scale_color_manual(" ",
                     breaks=c("Lausanne","Basel"),
                     values = c(mypalette[3],mypalette[2]))+
  xlab("Gestational age (w)")+
  ggtitle("Gestational age")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size= 15),
        axis.title=element_text(size= 18),
        plot.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.position = c(0.2,0.9))


Supplement_density <- cowplot::plot_grid(density_plot_age,density_plot_height, 
                                 density_plot_head,density_plot_ConjExt,
                                 density_plot_Cretes, density_plot_birthweight,
                                 density_plot_GA,
                                  ncol=2,nrow=4, align="hv")

cowplot::save_plot("output/Supplement_density.pdf", Supplement_density,base_height=30,base_width=15,limitsize = FALSE)


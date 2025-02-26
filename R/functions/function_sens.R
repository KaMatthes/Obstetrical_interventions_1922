
function_sens <- function(pm) {
dt <- read.xlsx("data/data_com.xlsx",detectDates = TRUE) %>%
  # filter(City==city) %>%
  mutate(Normal=recode(Normal, "1"="0",
                       "0"="1"),
         Normal= as.factor(Normal)) %>%
  filter(!is.na(Normal)) %>%
  select(Normal, eval(substitute(pm))) %>%
  na.omit()

formula <- as.formula(paste("Normal ~ ", eval(substitute(pm))))
fit <- glm(formula, family=binomial, data=dt)

predicted_values <- predict(fit,type="response")

dt2 <- auc_roc(predicted_values, fit$y,returnDT=TRUE) %>%
  data_frame()

ggplot(data=dt2,aes(x=CumulativeFPR, y=CumulativeTPR ))+ 
  geom_line(size=1) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  ylab("TPR (sensitivity)") +
  xlab("FPR (1-specificity)") +
  ggtitle(paste0(eval(substitute(pm)))) +
  theme_bw()+
  theme(
    aspect.ratio=1,
    strip.text = element_text(color="black",size= strip_text),
    axis.text=element_text(color="black",size= 15),
    axis.text.x=element_text(color="black",size= 15),
    axis.title=element_text(size= 15),
    plot.title = element_text(size=size_plot_title),
    legend.text=element_text(size=15),
    legend.title=element_text(size=15),
    legend.position = "bottom")

}

epi_b <- function_sens("Bassin_Epines")
# epi_l <- function_sens("Lausanne", "Bassin_Epines")

cre_b <- function_sens("Bassin_Cretes")
# cre_l <- function_sens("Lausanne", "Bassin_Cretes")


ext_b <- function_sens("Bassin_ConjExt")
# ext_l <- function_sens("Lausanne", "Bassin_ConjExt")

plot_auc <- cowplot::plot_grid(epi_b,
                   cre_b, 
                   ext_b,
                   nrow=1, ncol=3)

ggsave("output/auc.png",  plot_auc,h=12,w=20)

library(MASS)
# library(lme4) 
library(tidyverse)
library(openxlsx)
library(lubridate)
library(ggsci)
library(conflicted)
# library(nlme)
# library(lmerTest)
library(ggplot2)
library(cowplot)
library(finalfit)
library(scales)
library(DescTools)
library(mltools)
library(cowplot)

conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("rbind.fill", "plyr")
conflict_prefer("group_by", "dplyr")


### Research questions from the paper:
# What was the relationship between maternal skeletal dimensions (pelvis, height) and infant body dimensions (head circumference, birth weight) among expectant mothers in two Swiss cities in the early 1920s? 
# What was the role of maternal malnutrition such as rickets or iodine deficiency? 
# How common were obstetric interventions such as caesarean section, episiotomy, forceps delivery and extraction? 
# Was the imbalance between the mother's and child's body size a relevant risk factor for such interventions, as well as for the duration of labour?


mypalette <-pal_jco()(10)

strip_text <- 25
lwd_size <- 2
pch_type <- 19
lwdline <- 1
size_legend <- 15
size_legend_title<- 25
pd <-position_dodge(width=0.5)
fatten_size <- 4
plot_title <- 25

size_axis <- 22
size_axis_title <- 22
size_plot_title <-15
size_legend_text <- 20




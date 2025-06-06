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
library(summarytools )

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




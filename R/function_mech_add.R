or_plot_mec_add <- function (.data, dependent, explanatory, random_effect = NULL, 
          factorlist = NULL, glmfit = NULL, glmfit2=NULL,confint_type = NULL, remove_ref = FALSE, 
          breaks = NULL, column_space = c(-0.1, 0, 0.05,0.1), dependent_label = NULL, 
          prefix = "", suffix = ": OR (95% CI)", 
          table_text_size = 4, title_text_size = 20, plot_opts = NULL, 
          table_opts = NULL, ...) 
{
  requireNamespace("ggplot2")
  if (!is.null(factorlist)) {
    if (is.null(factorlist$Total)) 
      stop("summary_factorlist function must include total_col=TRUE")
    if (is.null(factorlist$fit_id)) 
      stop("summary_factorlist function must include fit_id=TRUE")
  }
  if (is.null(factorlist)) {
    factorlist = summary_factorlist(.data, dependent, explanatory, 
                                    total_col = TRUE, fit_id = TRUE)
  }
  if (remove_ref) {
    factorlist = factorlist %>% dplyr::mutate(label = ifelse(label == 
                                                               "", NA, label)) %>% tidyr::fill(label) %>% 
      dplyr::group_by(label) %>% dplyr::filter(dplyr::row_number() != 
                                                 1 | dplyr::n() > 2 | levels %in% c("Mean (SD)", 
                                                                                    "Median (IQR)")) %>% rm_duplicate_labels()
  }
  if (is.null(breaks)) {
    breaks = scales::pretty_breaks()
  }
  if (is.null(confint_type) && is.null(random_effect)) {
    confint_type = "profile"
  }
  else if (is.null(confint_type) && (!is.null(random_effect) | 
                                     inherits(glmfit, "glmerMod"))) {
    confint_type = "default"
  }
  if (is.null(glmfit) && is.null(random_effect)) {
    glmfit = glmmulti(.data, dependent, explanatory)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type, ...)
    glmfit_df_c2 = fit2df(glmfit2, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type, ...)
  }
  else if (is.null(glmfit) && !is.null(random_effect)) {
    glmfit = glmmixed(.data, dependent, explanatory, random_effect)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)", 
                         confint_type = confint_type, ...)
  }
  if (!is.null(glmfit) && is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type, estimate_name = "OR", 
                         exp = TRUE, ...)
    glmfit_df_c2 = fit2df(glmfit2, condense = TRUE, estimate_suffix = " (multivariable)", 
                         confint_type = confint_type, estimate_name = "OR", 
                         exp = TRUE, ...)
  }
  else if (!is.null(glmfit) && !is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)", 
                         confint_type = confint_type, estimate_name = "OR", 
                         exp = TRUE, ...)
  }
  glmfit_df = fit2df(glmfit, condense = FALSE, confint_type = confint_type, 
                     estimate_name = "OR", exp = TRUE, ...)
  glmfit_df2 = fit2df(glmfit2, condense = FALSE, confint_type = confint_type, 
                     estimate_name = "OR", exp = TRUE, ...)
  df.out = finalfit_merge(factorlist, glmfit_df_c)
  df.out = finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
  df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>% 
    as.numeric()
  df.out$Total[which(df.out$levels %in% c("Mean (SD)", 
                                          "Median (IQR)"))] = dim(.data)[1]
  df.out$levels[which(df.out$levels %in% c("Mean (SD)", 
                                           "Median (IQR)"))] = "-"
  
  
  df.out2 = finalfit_merge(factorlist, glmfit_df_c2)
  df.out2 = finalfit_merge(df.out2, glmfit_df2, ref_symbol = "1.0")
  df.out2$Total = stringr::str_remove(df.out2$Total, " \\(.*\\)") %>% 
    as.numeric()
  df.out2$Total[which(df.out2$levels %in% c("Mean (SD)", 
                                          "Median (IQR)"))] = dim(.data)[1]
  df.out2$levels[which(df.out2$levels %in% c("Mean (SD)", 
                                           "Median (IQR)"))] = "-"
  
  if (any(is.na(df.out$label))) {
    remove_rows = which(is.na(df.out$label))
    df.out = df.out[-remove_rows, ]
  }
  else {
    df.out
  }
  
  
  if (any(is.na(df.out2$label))) {
    remove_rows = which(is.na(df.out2$label))
    df.out2 = df.out[-remove_rows, ]
  }
  else {
    df.out2
  }
  
  df.out$levels = as.character(df.out$levels)
  df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])
  df.out$City = "Lausanne"
  df.out$OR_plot=paste0(substr(df.out$`OR (multivariable)`,1,15),")")
  df.out <- df.out %>%
    mutate(OR_plot=recode(OR_plot, "-)" = "-"),
           label=recode(label,"Position_normal" = "Child position",
                        "sex" ="Sex",
                        "SEP_comb_3a" ="Socioeconomic position",
                        "Etat.general" = "Health status",
                        "Maternal.body" = "Maternal body",
                        "parity" ="Parity",
                        "age_mother" ="Maternal age (years)",
                        "GA_weeks" = "Gestational age (w)",
                        "Bassin_ConjExt" = "Conjugata Externa in cm",
                        # "Bassin_Epines" = "Bassin Epines",
                        # "height_cretes" = "Ratio height/Bassin Cretes",
                        # "height_cretes_quan" = "Ratio height/Bassin Cretes Q",
                        # "head_Bassin_ConjExt_quan" = "head cir/Bassin ConjExt Q",
                        # "height_weight_quan" = "height/birthweight Q",
                        "Bassin_Cretes" = "Bassin Cretes in cm",
                        "birthweight100" = "Birthweight in 100gr",
                        "height10" = "Maternal height in 10cm",
                        "head_ConjExt" = "Conjug. Ext. vs Head circum.",
                        "head_circ" = "Head circumference in cm"),
    levels=ifelse((label=="Parity" | label=="Maternal age (years)"
                   | label=="Gestational age (w)"  | label=="Conjugata Externa in cm" 
                   | label=="Head circumference in cm"),"-",levels)) %>%
           # fit_id= factor(fit_id, levels = c("sexmale","sexfmale","age_mother","parity",
           #                                 "birthweight100", "GA_weeks","Position_normalnormal","Position_normalnonnormal",
           #                                 "Bassin_Epines","height_cretes_quan1Q","height_cretes_quan2Q","height_cretes_quan3Q",
           #                                 "height_cretes_quan4Q", "height_cretes_quan5Q", "head_Bassin_ConjExt_quan1Q",
           #                                 "head_Bassin_ConjExt_quan2Q","head_Bassin_ConjExt_quan3Q","head_Bassin_ConjExt_quan4Q",
           #                                 "head_Bassin_ConjExt_quan5Q"))) %>%
    select(fit_id, label, levels, Total,City, OR_plot, OR, L95, U95)     %>%
    add_row(fit_id=NA, label="Explanatory variable", levels ="Factor", Total=NA, City="Lausanne", OR_plot="Lausanne", OR=NA,L95=NA, U95=NA )
  

  df.out2$levels = as.character(df.out2$levels)
  df.out2$fit_id = factor(df.out2$fit_id, levels = df.out2$fit_id[order(-df.out2$index)])
  df.out2$City = "Basel"
  df.out2$OR_plot=paste0(substr(df.out2$`OR (multivariable)`,1,15),")")
  df.out2 <- df.out2 %>%
    mutate(OR_plot=recode(OR_plot, "-)" = "-"),
           label=recode(label,"Position_normal" = "Child position",
                        "sex" ="Sex",
                        "SEP_comb_3a" ="Socioeconomic position",
                        "Etat.general" = "Health status",
                        "Maternal.body" = "Maternal body",
                        "parity" ="Parity",
                        "age_mother" ="Maternal age (years)",
                        "GA_weeks" = "Gestational age (w)",
                        "Bassin_ConjExt" = "Conjugata Externa in cm",
                        # "Bassin_Epines" = "Bassin Epines",
                        # "height_cretes" = "Ratio height/Bassin Cretes",
                        # "height_cretes_quan" = "Ratio height/Bassin Cretes Q",
                        # "head_Bassin_ConjExt_quan" = "head cir/Bassin ConjExt Q",
                        # "height_weight_quan" = "height/birthweight Q",
                        "Bassin_Cretes" = "Bassin Cretes in cm",
                        "birthweight100" = "Birthweight in 100gr",
                        "height10" = "Maternal height in 10cm",
                        "head_ConjExt" = "Conjug. Ext. vs Head circum.",
                        "head_circ" = "Head circumference in cm"),
                        levels=ifelse((label=="Parity" | label=="Maternal age (years)"
                                       | label=="Gestational age (w)"  | label=="Conjugata Externa in cm" 
                                       | label=="Head circumference in cm"),"-",levels)) %>%
    # fit_id= factor(fit_id, levels = c("sexmale","sexfmale","age_mother","parity",
    #                                 "birthweight100", "GA_weeks","Position_normalnormal","Position_normalnonnormal",
    #                                 "Bassin_Epines","height_cretes_quan1Q","height_cretes_quan2Q","height_cretes_quan3Q",
    #                                 "height_cretes_quan4Q", "height_cretes_quan5Q", "head_Bassin_ConjExt_quan1Q",
    #                                 "head_Bassin_ConjExt_quan2Q","head_Bassin_ConjExt_quan3Q","head_Bassin_ConjExt_quan4Q",
    #                                 "head_Bassin_ConjExt_quan5Q"))) %>%
   select(fit_id, label, levels, Total,City, OR_plot, OR, L95, U95)     %>%
    add_row(fit_id=NA, label="Explanatory variable", levels ="Factor", Total=NA, City="Basel", OR_plot="Basel", OR=NA,L95=NA, U95=NA )
  
  
  
  df.out.b <- rbind(df.out, df.out2) %>%
    rename(Number=Total) 
  # %>%
  #   mutate( fit_id= factor(fit_id, levels = c("sexmale","sexfmale","age_mother","parity",
  #                                             "birthweight100", "GA_weeks","Position_normalnormal","Position_normalnonnormal",
  #                                             "Bassin_Epines","height_cretes_quan1Q","height_cretes_quan2Q","height_cretes_quan3Q",
  #                                             "height_cretes_quan4Q", "height_cretes_quan5Q", "head_Bassin_ConjExt_quan1Q",
  #                                             "head_Bassin_ConjExt_quan2Q","head_Bassin_ConjExt_quan3Q","head_Bassin_ConjExt_quan4Q",
  #                                             "head_Bassin_ConjExt_quan5Q")))

#   return(df.out.b)
# }
 
  g1 = ggplot(df.out.b, aes(x = as.numeric(OR), xmin = as.numeric(L95), 
                          xmax = as.numeric(U95), y = fit_id, col=City, fill=City)) +
    geom_errorbarh(height = 0.2,position=pd, lwd=1) + 
    geom_vline(xintercept = 1, linetype = "longdash", 
               colour = "black") + 
    geom_point(aes(size = Number), shape = 22,position=pd) + 
    scale_x_continuous(trans = "log10") + 
    # scale_x_continuous( breaks = breaks) + 
    xlab("Odds ratio and 95% CI (log scale) ") + 
    scale_color_manual(" ",
                       breaks=c("Lausanne","Basel"),
                       values = c(mypalette[3],mypalette[2]))+
    scale_fill_manual(" ",
                       breaks=c("Lausanne","Basel"),
                       values = c(mypalette[3],mypalette[2]),
                       guide = FALSE)+
    scale_size_continuous(" ",
                      guide = FALSE)+

    theme_classic(11) + 
    theme(axis.title.x = element_text(size=12), 
          axis.text.x = element_text(size=12), 
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.line.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = c(0.8,0.9),
          legend.text =element_text(size=15))
  
  
  t1 = ggplot(df.out, aes(x = as.numeric(OR), y = fit_id)) + 
    annotate("text", x = column_space[1], y = df.out$fit_id, 
             label = df.out[, 2], hjust = 0, size = c(rep(4,15), 5)) + 
    annotate("text", x = column_space[2], y = df.out$fit_id,
             label = df.out[, 3], hjust = 1, size = c(rep(4,15), 5)) +
    annotate("text", x = column_space[3], y = df.out$fit_id,
             label = df.out[, 6], hjust = 1, size = c(rep(4,15),5)) +
    annotate("text", x = column_space[4], y = df.out2$fit_id,
             label = df.out2[, 6], hjust = 1, size = c(rep(4,15), 5)) +
    theme_classic(11) + 
    theme(axis.title.x = element_text(colour = "white"), 
                              axis.text.x = element_text(colour = "white"), axis.title.y = element_blank(), 
                              axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                              line = element_blank())
  g1 = g1 + plot_opts
  t1 = t1 + table_opts
  title = plot_title(.data, dependent, dependent_label = dependent_label, 
                     prefix = prefix, suffix = suffix)
  gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(2.5, 2), 
                          top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size), 
                                               just = "left"))
}
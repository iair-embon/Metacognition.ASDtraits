##########################################
### Mixed Logistic Regression Analysis ### TAB 2 ## CORREGIR PARA MODELO MIXTO
##########################################

###############
### library ###
###############

library(gtsummary)
library(dplyr)
# data
### load mixed logistic regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")
load(file= filepath)

table2 <- a %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_number (.x, digits = 3),
    label = list(
      "(Intercept)" ~ "Intercept",
      "aq.norm" ~ "AQ.norm",
      "Im" ~ "Gender[m]",
      "edad.norm" ~ "Age.norm",
      "aq.norm:Im" ~ "AQ.norm:Gender[m]",
      "aq.norm:edad.norm" ~ "AQ.norm:Age.norm")
  ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table2), file = "Tables/MixedLogisticRegressionAnalysis.png")
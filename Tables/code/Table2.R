##########################################
### Mixed Logistic Regression Analysis ### TAB 2 ## CORREGIR PARA MODELO MIXTO
##########################################

###############
### library ###
###############

require(gtsummary)
require(dplyr)
require(broom.mixed)

# data
### load mixed logistic regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")
load(file= filepath)

table2 <- a_log %>%
  tbl_regression(
    intercept = T,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    estimate_fun =  ~style_ratio (.x, digits = 3),
    tidy_fun = broom.mixed::tidy,
    label = list(
      "(Intercept)" ~ "Intercept",
      "confidence_key.norm" ~ "Confidence.norm",
      "confidence_key.norm:AQ.norm" ~ "AQ.norm:Confidence.norm",
      "confidence_key.norm:genero" ~ "Gender[m]:Confidence.norm",
      "confidence_key.norm:edad.norm" ~ "Age.norm:Confidence.norm",
      "confidence_key.norm:AQ.norm:genero" ~ "AQ.norm:Gender[m]:Confidence.norm",
      "confidence_key.norm:AQ.norm:edad.norm" ~ "AQ.norm:Age.norm:Confidence.norm",
      "sujetos.sd__(Intercept)" ~ "Subjects SE(Intercept)")) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = deviance)

gt::gtsave(as_gt(table2), file = "Tables/MixedLogisticRegressionAnalysis.png")
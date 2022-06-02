#########################################
### Linear Regression Analysis AUROC2 ### TAB 1
#########################################

###############
### library ###
###############

library(gtsummary)
library(dplyr)
# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/AUROC2_AQ_linear_model.RData")
load(file= filepath)


table1 <- a %>%
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

gt::gtsave(as_gt(table1), file = "Tables/AUROC2_linear_model.png")


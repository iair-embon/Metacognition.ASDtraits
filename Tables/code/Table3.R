###############################################
### Linear Regression Analysis AQ Subscales ### TAB 3
###############################################

###############
### library ###
###############

require(gtsummary)
require(dplyr)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/AQ_subscales_linear_model.RData")
load(file= filepath)


table3 <- a %>%
  tbl_regression(
               intercept = T,
               pvalue_fun = ~style_pvalue(.x, digits = 3),
               estimate_fun =  ~style_number (.x, digits = 3),
               label = list(
                 "(Intercept)" ~ "Intercept",
                 "aq_social.norm" ~ "Social Skill.norm",
                 "aq_at_sw.norm" ~ "Attention Switching.norm",
                 "aq_at_de.norm" ~ "Attention to Detail.norm",
                 "aq_com.norm" ~ "Communication.norm",
                 "aq_im.norm" ~ "Imagination.norm",
                 "Im" ~ "Gender[m]",
                 "edad.norm" ~ "Age.norm")
               ) %>%
  modify_header(label ~ "") %>%
  modify_column_unhide(column = std.error) %>%
  add_global_p() %>%
  bold_p(t = 0.05) %>%
  add_glance_table(include = c(r.squared, adj.r.squared))

gt::gtsave(as_gt(table3), file = "Tables/AQ_subscales_linear_model.png")


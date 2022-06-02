#########################################
### Linear Regression Analysis AUROC2 ### TAB 1
#########################################

###############
### library ###
###############

require(sjPlot)
#require(webshot)
require(webshot2)

# data
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("AUROC2_AQ_linear_model.RData")
load(file= filepath)

# table
tab_model(a, pred.labels = c("Intercept","AQ.norm", "Gender[m]", "Age.norm",
                             "AQ.norm:Gender[m]","AQ.norm:Age.norm"),
          dv.labels = "AUROC2 linear model",
          show.se = T,
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          file = "Tables/AUROC2_linear_model2.html")

# save as png
webshot("Tables/AUROC2_linear_model2.html", "Tables/AUROC2_linear_model2.png")
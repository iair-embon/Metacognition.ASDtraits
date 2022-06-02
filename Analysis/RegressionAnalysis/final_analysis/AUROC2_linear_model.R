#########################################
### Linear Regression Analysis AUROC2 ### 
#########################################

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

d.sin.normalizar.solo.FyM <- DF_list$d


### lineas para hacer regresion 


d1 = d.sin.normalizar.solo.FyM

d1$aq.norm <- (d1$aq - mean(d1$aq))/ sd(d1$aq)

d1$mc.norm <- (d1$mc - mean(d1$mc))/ sd(d1$mc)

d1$edad.norm <- (d1$edad - mean(d1$edad))/ sd(d1$edad)

d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)

############ metacog y AQ

# corro el modelo
a=lm(mc ~ aq.norm +
       Im +
       edad.norm+
       aq.norm: Im+
       aq.norm:edad.norm,
     data = d1) 
summary(a)

save(a, file = "Data/Regression_Results/AUROC2_AQ_linear_model.RData")

###############################################
### Linear Regression Analysis AQ Subscales ### 
###############################################

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


############ metacog y AQ subscales

d1$aq_social.norm <- (d1$aq_social - mean(d1$aq_social))/ sd(d1$aq_social)
d1$aq_at_sw.norm <- (d1$aq_atention_switch - mean(d1$aq_atention_switch))/ sd(d1$aq_atention_switch)
d1$aq_at_de.norm <- (d1$aq_atencion_detail - mean(d1$aq_atencion_detail))/ sd(d1$aq_atencion_detail)
d1$aq_com.norm <- (d1$aq_communication - mean(d1$aq_communication))/ sd(d1$aq_communication)
d1$aq_im.norm <- (d1$aq_imagination - mean(d1$aq_imagination))/ sd(d1$aq_imagination)

# corro el modelo
a=lm(mc ~ aq_social.norm+
       aq_at_sw.norm+
       aq_at_de.norm+
       aq_com.norm+
       aq_im.norm+
       Im +
       edad.norm,
     data = d1) 
summary(a)

save(a, file = "Data/Regression_Results/AQ_subscales_linear_model.RData")

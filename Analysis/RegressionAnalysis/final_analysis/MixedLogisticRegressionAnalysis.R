##########################################
### Mixed Logistic Regression Analysis ###
##########################################

###############
### library ###
###############

require(lme4)

# levanto los datos

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# only male and female genders
d <- df_total[df_total$genero == "Masculino" | df_total$genero == "Femenino",]

# modifico las variables que me interesan modificar
d$genero <- ifelse(d$genero == "Masculino",1,0)
d$discrimination_is_correct <- ifelse(d$discrimination_is_correct == TRUE, 1, 0)
d$sujetos <- factor(d$sujetos)
d$confidence_key.norm <- (d$confidence_key - 1) / 3 
d$AQ.norm <-  (d$AQ - mean(d$AQ)) / sd(d$AQ)
d$edad.norm <- (d$edad - mean(d$edad))/sd(d$edad)

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:AQ.norm +
                 confidence_key.norm:genero +
                 confidence_key.norm:edad.norm +
                 confidence_key.norm:AQ.norm:genero +
                 confidence_key.norm:AQ.norm:edad.norm +
                 (1|sujetos),
               data = d,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

save(a, file = "Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")
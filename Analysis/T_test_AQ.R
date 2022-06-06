####################################
### T_test AQ by gender Analysis ###
####################################

# levanto los datos

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

d <- DF_list$d

# only male and female genders
d.f <- d[d$Im == "Femenino",]
d.m <- d[d$Im == "Masculino" ,]

t.test(d.f$aq, d.m$aq)
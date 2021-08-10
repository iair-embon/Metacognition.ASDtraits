####### getting metacognition by reaction time filter

Auroc2_by_RT_filter <- function(d,
                                filtroRT_Disc_Sup,
                                filtroRT_Disc_Inf,
                                filtroRT_Conf_Sup,
                                filtroRT_Conf_Inf,
                                filtroTrial){
  # d = df_total
  # Superior and inferior filtro Reaction Time Discrimination task 
  # Superior and inferior filtro Reaction Time Confidence task 
  
  # voy a la carpeta del proyecto
  root <- rprojroot::is_rstudio_project
  basename(getwd())
  
  # load the type 2 ROC analysis function
  source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))
  
  ## get metacognitive sensivity
  library(dplyr)
  
  # Filter by RT
  d1 <- d[d$t_ensayo_discriminacion >= filtroRT_Disc_Inf,]
  d1 <- d1[d1$t_ensayo_discriminacion <= filtroRT_Disc_Sup,]
  d1 <- d1[d1$t_ensayo_confianza >=filtroRT_Conf_Inf,]
  d1 <- d1[d1$t_ensayo_confianza <=filtroRT_Conf_Sup,]
  
  # Filter by trails
  d1 <- d1[d1$trials > filtroTrial,]
  
  Nsuj <- length(unique(d$sujetos))
  # saving metacog = mc for each RT discarded
  mc_Rt_Discarded <- rep(NA, Nsuj)
  ExistingSubjects <- unique(d$sujetos)
  
  for (i in 1:Nsuj){
    mc_Rt_Discarded[i] <- type2roc(correct = d1$discrimination_is_correct[d1$sujetos==ExistingSubjects[i]], 
                                   conf = d1$confidence_key[d1$sujetos==ExistingSubjects[i]], 
                                   Nratings = 4)
  }
  return(list(mc_Rt_Discarded = mc_Rt_Discarded, df_total = d1))
}



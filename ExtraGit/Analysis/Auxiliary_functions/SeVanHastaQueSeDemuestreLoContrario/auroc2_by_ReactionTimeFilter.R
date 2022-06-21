####### getting metacognition by reaction time filter

Auroc2_by_RT_filter <- function(d,
                                filtroRT_Disc_Sup,
                                filtroRT_Disc_Inf,
                                filtroRT_Conf_Sup,
                                filtroRT_Conf_Inf,
                                filtroTrial,
                                cant_trial_filter){
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
  
  ###### discarding because very few trials
  cant_trials_por_sujeto <- rep(NaN, length(unique(d1$sujetos)))
  existing_subject <- unique(d1$sujetos)
  
  for (i in 1:length(cant_trials_por_sujeto)) {
    cant_trials_por_sujeto[i] <- nrow(d1[d1$sujetos == existing_subject[i],])
  }
  
  # veo quienes son
  indices_cant_trials <- which(cant_trials_por_sujeto < cant_trial_filter)
  subj_pocos_trials<- existing_subject[indices_cant_trials]
  
  d1 <- d1[! d1$sujetos %in% subj_pocos_trials,]
  
  ########### AUROC2
  
  Nsuj <- length(unique(d1$sujetos))
  # saving metacog = mc for each RT discarded
  mc_Rt_Discarded <- rep(NA, Nsuj)
  ExistingSubjects <- unique(d1$sujetos)
  
  for (i in 1:Nsuj){
    mc_Rt_Discarded[i] <- type2roc(correct = d1$discrimination_is_correct[d1$sujetos==ExistingSubjects[i]], 
                                   conf = d1$confidence_key[d1$sujetos==ExistingSubjects[i]], 
                                   Nratings = 4)
  }
  return(list(mc_Rt_Discarded = mc_Rt_Discarded, df_total = d1))
}



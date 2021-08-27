DataFrame_Filtered <- function(experimento, 
                                  filtroRT_Disc_Sup,
                                  filtroRT_Disc_Inf,
                                  filtroRT_Conf_Sup,
                                  filtroRT_Conf_Inf,
                                  filtroTrial = 0,
                                  cant_trial_filter){ 
  
  # experimento = 1,2,ambos
  # Superior and inferior filtro Reaction Time Discrimination task 
  # Superior and inferior filtro Reaction Time Confidence task 
  

  # voy a la carpeta del proyecto
  root <- rprojroot::is_rstudio_project
  basename(getwd())
  
  # elijo que exp voy a utilizar, 1, 2 (replica), o ambos
  if (experimento == 1){
    filepath <- (root$find_file("Data/Results_Exp1/df_total.Rda"))
    df_total <- load(file= filepath)} 
  if (experimento == 2){
    filepath <- (root$find_file("Data/Results_Exp2(replica)/df_total.Rda"))
    df_total <- load(file= filepath)}
  if (experimento == 'ambos'){
    ## ambos df_total:
    filepath <- (root$find_file("Data/Results_Exp1/df_total.Rda"))
    load(file= filepath)
    a <- df_total
    
    filepath <- (root$find_file("Data/Results_Exp2(replica)/df_total.Rda"))
    load(file= filepath)
    b <- df_total
    # sumo 100 a la columna sujetos, para que no se pisen los nros y este nro sea unico
    b$sujetos <- b$sujetos + 1000 
    
    ### cambio la columna sujetos por una nueva, para que no se pisen los nros y este nro sea unico
    # uno los df
    df_total <- rbind(a,b)
  }
  
  if (experimento == 'todos'){
    ## ambos df_total:
    filepath <- (root$find_file("Data/Results_Exp1/df_total.Rda"))
    load(file= filepath)
    a <- df_total
    
    filepath <- (root$find_file("Data/Exp2+3/df_total.Rda"))
    load(file= filepath)
    b <- df_total
    # sumo 100 a la columna sujetos, para que no se pisen los nros y este nro sea unico
    b$sujetos <- b$sujetos + 1000 
    
    ### cambio la columna sujetos por una nueva, para que no se pisen los nros y este nro sea unico
    # uno los df
    df_total <- rbind(a,b)
  }
  
  ## Filter by reaction times 
  source(root$find_file("Analysis/AuxiliaryFunctions/auroc2_by_ReactionTimeFilter.R"))
  list_exp <- Auroc2_by_RT_filter(d = df_total,
                                filtroRT_Disc_Sup = filtroRT_Disc_Sup,
                                filtroRT_Disc_Inf = filtroRT_Disc_Inf,
                                filtroRT_Conf_Sup = filtroRT_Conf_Sup,
                                filtroRT_Conf_Inf = filtroRT_Conf_Inf,
                                filtroTrial = filtroTrial,
                                cant_trial_filter = cant_trial_filter)
  auc2 <- list_exp$mc_Rt_Discarded
  df_total <- list_exp$df_total
  
  # tomo las variables de interes excepto auroc2
  PC <- rep(NaN, length(unique(df_total$sujetos)))
  genero <- rep(NaN, length(unique(df_total$sujetos)))
  AQ <- rep(NaN, length(unique(df_total$sujetos)))
  horasSueno <- rep(NaN, length(unique(df_total$sujetos)))
  edad <- rep(NaN, length(unique(df_total$sujetos)))
  estudio <- rep(NaN, length(unique(df_total$sujetos)))
  media_tr_discri <- rep(NaN, length(unique(df_total$sujetos)))
  media_tr_confi <- rep(NaN, length(unique(df_total$sujetos)))
  media_confidence <- rep(NaN, length(unique(df_total$sujetos)))
  sd_confidence <- rep(NaN, length(unique(df_total$sujetos)))
  
  # sujetos que quedaron
  ExistingSubjects <- unique(df_total$sujetos)
  
  for (i in 1:length(unique(df_total$sujetos))) { 
    PC[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"PC"])
    genero[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"genero"]) # 1 femenino, 2 # masculino
    AQ[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ"])
    horasSueno[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"horasSueno"])
    edad[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"edad"])
    estudio[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"estudio"])
    media_tr_discri[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_discri"])
    media_tr_confi[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_confi"])
    media_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_confidence"])
    sd_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"sd_confidence"])
  }
  
  for (i in 1:length(genero)) {
    if(genero[i] == 1){
      genero[i] = "F"
    } 
    else if(genero[i] == 2){
      genero[i] = "M"
    }
  }
  
  
  d.sin.normalizar = data.frame(sujetos = ExistingSubjects,
                                mc  = auc2,
                                Im = genero, 
                                pc  = PC,
                                aq = AQ,
                                hs = horasSueno,
                                edad = edad,
                                es = estudio,
                                tr_d = media_tr_discri,
                                tr_c = media_tr_confi,
                                m_c = media_confidence,
                                sd_c = sd_confidence)
  
  # filtro para los que tienen metacog menores a 0.5
  mean_mc <- mean(d.sin.normalizar$mc)
  sd_mc <-sd(d.sin.normalizar$mc)
  d.sin.normalizar.mc.filter <- d.sin.normalizar[d.sin.normalizar$mc >= mean_mc - sd_mc* 1.5,]
#  d.sin.normalizar.mc.filter <- d.sin.normalizar[d.sin.normalizar$mc >= 0.4,] # a partir de cuanto quiero dejar de metacog
  
  d <- d.sin.normalizar
  d.mc.filter <- d.sin.normalizar.mc.filter
  
  d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
  #d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
  d$edad <- (d$edad - mean(d$edad)) / sd(d$edad)
  d$mc <- (d$mc - mean(d$mc)) / sd(d$mc)
  d$aq <- (d$aq - mean(d$aq)) / sd(d$aq)
  d$tr_d <- (d$tr_d - mean(d$tr_d)) / sd(d$tr_d)
  d$tr_c <- (d$tr_c - mean(d$tr_c)) / sd(d$tr_c)
  d$m_c <- (d$m_c - mean(d$m_c)) / sd(d$m_c)
  d$sd_c <- (d$sd_c - mean(d$sd_c)) / sd(d$sd_c)
  
  d.mc.filter$pc <- (d.mc.filter$pc - mean(d.mc.filter$pc)) / sd(d.mc.filter$pc)
  #d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
  d.mc.filter$edad <- (d.mc.filter$edad - mean(d.mc.filter$edad)) / sd(d.mc.filter$edad)
  d.mc.filter$mc <- (d.mc.filter$mc - mean(d.mc.filter$mc)) / sd(d.mc.filter$mc)
  d.mc.filter$aq <- (d.mc.filter$aq - mean(d.mc.filter$aq)) / sd(d.mc.filter$aq)
  d.mc.filter$tr_d <- (d.mc.filter$tr_d - mean(d.mc.filter$tr_d)) / sd(d.mc.filter$tr_d)
  d.mc.filter$tr_c <- (d.mc.filter$tr_c - mean(d.mc.filter$tr_c)) / sd(d.mc.filter$tr_c)
  d.mc.filter$m_c <- (d.mc.filter$m_c - mean(d.mc.filter$m_c)) / sd(d.mc.filter$m_c)
  d.mc.filter$sd_c <- (d.mc.filter$sd_c - mean(d.mc.filter$sd_c)) / sd(d.mc.filter$sd_c)
  
  d.sin.normalizar.solo.FyM <- d.sin.normalizar[d.sin.normalizar$Im == "Masculino" | d.sin.normalizar$Im == "Femenino",]
  d.sin.normalizar.solo.FyM.mc.filter <- d.sin.normalizar.mc.filter[d.sin.normalizar.mc.filter$Im == "Masculino" | d.sin.normalizar.mc.filter$Im == "Femenino",]
  d.solo.FyM.mc.filter <- d.mc.filter[d.mc.filter$Im == 'Femenino' | d.mc.filter$Im == 'Masculino',]
  df_total.solo.FyM <-  df_total[df_total$genero == 'Femenino' | df_total$genero == 'Masculino',]

  DF_list <- list(a = df_total, b = d.sin.normalizar, c = d.sin.normalizar.mc.filter, d = d,
                  e = d.mc.filter, f = d.sin.normalizar.solo.FyM, g = d.sin.normalizar.solo.FyM.mc.filter,
                  h = d.solo.FyM.mc.filter)
  return(DF_list)
  }
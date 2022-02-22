DataFrame_Filtered_already_applied <- function(df_total){ 
  
  # tomo las variables de interes excepto auroc2
  PC <- rep(NaN, length(unique(df_total$sujetos)))
  mc <- rep(NaN, length(unique(df_total$sujetos)))
  genero <- rep(NaN, length(unique(df_total$sujetos)))
  AQ <- rep(NaN, length(unique(df_total$sujetos)))
  AQ_social <- rep(NaN, length(unique(df_total$sujetos)))
  AQ_atencion_switch <- rep(NaN, length(unique(df_total$sujetos)))
  AQ_atencion_detail <- rep(NaN, length(unique(df_total$sujetos)))
  AQ_communication <- rep(NaN, length(unique(df_total$sujetos)))
  AQ_imagination <- rep(NaN, length(unique(df_total$sujetos)))
  horasSueno <- rep(NaN, length(unique(df_total$sujetos)))
  edad <- rep(NaN, length(unique(df_total$sujetos)))
  estudio <- rep(NaN, length(unique(df_total$sujetos)))
  media_tr_discri <- rep(NaN, length(unique(df_total$sujetos)))
  media_tr_confi <- rep(NaN, length(unique(df_total$sujetos)))
  media_confidence <- rep(NaN, length(unique(df_total$sujetos)))
  sd_confidence <- rep(NaN, length(unique(df_total$sujetos)))
  m_difer_puntitos <- rep(NaN, length(unique(df_total$sujetos)))
  sd_difer_puntitos <- rep(NaN, length(unique(df_total$sujetos)))
  
  # sujetos que quedaron
  ExistingSubjects <- unique(df_total$sujetos)
  
  for (i in 1:length(unique(df_total$sujetos))) { 
    PC[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"PC"])
    mc[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"mc"])
    genero[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"genero"]) # 1 femenino, 2 # masculino
    AQ[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ"])
    AQ_social[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ_social"])
    AQ_atencion_switch[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ_atencion_switch"])
    AQ_atencion_detail[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ_atencion_detail"])
    AQ_communication[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ_communication"])
    AQ_imagination[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ_imagination"])
    horasSueno[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"horasSueno"])
    edad[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"edad"])
    estudio[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"estudio"])
    media_tr_discri[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_discri"])
    media_tr_confi[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_confi"])
    media_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_confidence"])
    sd_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"sd_confidence"])
    m_difer_puntitos[i]<- unique(mean(df_total[df_total$sujetos == ExistingSubjects[i],"diferencia_puntitos"]))
    sd_difer_puntitos[i]<- unique(sd(df_total[df_total$sujetos == ExistingSubjects[i],"diferencia_puntitos"]))
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
                                mc  = mc,
                                Im = genero, 
                                pc  = PC,
                                aq = AQ,
                                aq_social = AQ_social,
                                aq_atention_switch = AQ_atencion_switch,
                                aq_atencion_detail = AQ_atencion_detail,
                                aq_communication = AQ_communication,
                                aq_imagination = AQ_imagination,
                                hs = horasSueno,
                                edad = edad,
                                es = estudio,
                                tr_d = media_tr_discri,
                                tr_c = media_tr_confi,
                                m_c = media_confidence,
                                sd_c = sd_confidence,
                                m_dp = m_difer_puntitos,
                                sd_dp = sd_difer_puntitos)
  
  d <- d.sin.normalizar

  d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
  #d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
  d$edad <- (d$edad - mean(d$edad)) / sd(d$edad)
  d$mc <- (d$mc - mean(d$mc)) / sd(d$mc)
  d$aq <- (d$aq - mean(d$aq)) / sd(d$aq)
  d$aq_social <- (d$aq_social - mean(d$aq_social)) / sd(d$aq_social)
  d$aq_atention_switch <- (d$aq_atention_switch - mean(d$aq_atention_switch)) / sd(d$aq_atention_switch)
  d$aq_atencion_detail <- (d$aq_atencion_detail - mean(d$aq_atencion_detail)) / sd(d$aq_atencion_detail)
  d$aq_communication <- (d$aq_communication - mean(d$aq_communication)) / sd(d$aq_communication)
  d$aq_imagination <- (d$aq_imagination - mean(d$aq_imagination)) / sd(d$aq_imagination)
  d$tr_d <- (d$tr_d - mean(d$tr_d)) / sd(d$tr_d)
  d$tr_c <- (d$tr_c - mean(d$tr_c)) / sd(d$tr_c)
  d$m_c <- (d$m_c - mean(d$m_c)) / sd(d$m_c)
  d$sd_c <- (d$sd_c - mean(d$sd_c)) / sd(d$sd_c)
  d$m_dp <- (d$m_dp - mean(d$m_dp)) / sd(d$m_dp)
  d$sd_dp <- (d$sd_dp - mean(d$sd_dp)) / sd(d$sd_dp)
  
  d.normalizado <- d
  
  d.sin.normalizar.solo.FyM <- d.sin.normalizar[d.sin.normalizar$Im == "Masculino" | d.sin.normalizar$Im == "Femenino",]
  df_total.solo.FyM <-  df_total[df_total$genero == 'Femenino' | df_total$genero == 'Masculino',]
  
  DF_list <- list(a = df_total.solo.FyM, b = d.sin.normalizar,
                  c = d.normalizado, d = d.sin.normalizar.solo.FyM)
  return(DF_list)
}
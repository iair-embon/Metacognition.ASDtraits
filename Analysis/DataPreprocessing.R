####################################
# Metacognition and ASD experiment #
####################################

####### Data Preprocessing:

# Read the .txt results from JATOS and perform the data preprocessing.

# (To reado the .txt results from JATOS it was necesary to add an "enter" before the curly brackets
# that open the component "sincericidio").

library(jsonlite)

# Read the .txt result
root <- rprojroot::is_rstudio_project
basename(getwd())
# REEMPLAZAR:
#read each line and convert
content<-readLines(root$find_file("Data/Results_Exp1/jatos_results_20201129132347.txt"))
#content<-readLines(root$find_file("Data/Results_Exp2(replica)/jatos_results_20210519110741.txt"))
res<-lapply(content,fromJSON)

# each subject has 6 lists in order of arrival and by subjects.
# res[[1]] are the demographic data of subject 1
# res[[2]] are the data of the practice of the experiment of subject 1
# res[[3]] are the data from subject 1's experiment
# res[[4]] are the AQ data of subject 1
# res[[4]] are the data of the browser of subject 1
# res[[6]] are the email data of subject 1
# res[[7]] are the demographic data of subject 2
# res[[8]] are the data of the practice of the experiment of subject 2
# ....

iSub      <- 0
horasSuen <- c()
fechaNac  <- c()
pais      <- c()
genero    <- c()
estudio   <- c()
affeccionPsico <- c()
medicacion <- c()
AQ      <- c()
Browser <- c()
Sinc    <- c()
TeEscuchamos <- c()

# Experiment data frame
df_exp <- data.frame(t0 =character(), 
                 t_offset =character(), 
                 dots_num_left =character(), 
                 dots_num_right =character(), 
                 discrimination_is_correct =character(), 
                 discrimination_t_onset =character(), 
                 discrimination_t_keydown =character(), 
                 confidence_key =character(), 
                 confidence_t_onset=character(), 
                 confidence_t_keydown=character(), 
                 stringsAsFactors=FALSE) 

for (s in 1:(length(res)-4)){
  
  ind_suenio <- NaN  
  ind_fecha <- NaN  
  ind_pais <- NaN  
  ind_genero <- NaN  
  ind_estudio <- NaN  
  ind_affeccion <- NaN  
  ind_medicacion <- NaN  
  
  for (item in 1:length(res[[s]])){
    if (is.null(res[[s]][item]$sueno)           ==FALSE){   ind_suenio <- item   }
    if (is.null(res[[s]][item]$Cumpleanos)      ==FALSE){   ind_fecha  <- item   }
    if (is.null(res[[s]][item]$Pais)            ==FALSE){   ind_pais   <- item   }
    if (is.null(res[[s]][item]$Genero)          ==FALSE){   ind_genero <- item   }
    if (is.null(res[[s]][item]$Estudio)         ==FALSE){   ind_estudio <- item   }
    if (is.null(res[[s]][item]$AffeccionPsico)  ==FALSE){   ind_affeccion <- item   }
    if (is.null(res[[s]][item]$medicacion)      ==FALSE){   ind_medicacion <- item   }
  }
  
  # Condition 1 will be TRUE if there is a response to the first component of demographic data
  condicion1 <-  is.nan(ind_suenio) == FALSE
  # Condition 2 will be TRUE if there is an answer to the AQ questions (component 4)
  condicion2 <-  is.null(res[[s+3]][1]$question) ==FALSE
  
  if(condicion1 & condicion2 ){ # new participant
    iSub <- iSub + 1;
    # I take data from component 1 (demographic)
    horasSuen <- c(horasSuen,res[[s]][ind_suenio]$sueno)
    fechaNac  <- c(fechaNac,res[[s]][ind_fecha]$Cumpleanos)
    pais <- c(pais, res[[s]][ind_pais]$Pais)
    genero <- c(genero,res[[s]][ind_genero]$Genero)
    estudio <- c(estudio,res[[s]][ind_estudio]$Estudio)
    affeccionPsico <- c(affeccionPsico,res[[s]][ind_affeccion]$AffeccionPsico)
    medicacion <- c(medicacion,res[[s]][ind_medicacion]$medicacion)
    
    # Experiment data 
    df_exp <- rbind(df_exp, res[[s+2]])
    
    # AQ data
    AQ <- c(AQ, res[[s+3]])  
      
    if(is.null(res[[s+4]][1]$browser) ==FALSE){
    Browser <- c(Browser, res[[s+4]][1]$browser)
    }else{
      Browser <- c(Browser, NaN)}
    
    if(length(res)-s >= 5 ){
      
      if(is.null(res[[s+5]][1]$sincericidio) ==FALSE){
        Sinc <- c(Sinc, res[[s+5]][1]$sincericidio)
      }else{
        Sinc <- c(Sinc, NaN)}

      if(is.null(res[[s+5]]$TeEscuchamos) ==FALSE){
      TeEscuchamos <- c(TeEscuchamos, res[[s+5]]$TeEscuchamos)
      }else{
      TeEscuchamos <- c(TeEscuchamos, NaN)}
    }
  }
  }
####### df 

# df_DatosUnicos: for data of each subject.
# df_exp: save each trial of metacognition exp (already created in previous loop)

## df_DatosUnicos
sujetos <-  1:iSub
df_DatosUnicos <- data.frame(
  sujetos = sujetos, 
  horasSueno = horasSuen,
  fechaNac = fechaNac,
  pais = pais,
  genero = genero,
  estudio = estudio,
  affeccionPsico = affeccionPsico,
  medicacion = medicacion,
  Browser = Browser,
  sincericidio = Sinc,
  TeEscuchamos = TeEscuchamos,
  stringsAsFactors = FALSE
)

####### add subjects and trials to df_exp

# get the number of trials per subject
cant_trials <- length(res[[1+2]]$t0) 

# prepare subject column
col_sujetos <- 1:iSub
sujetos <- rep(col_sujetos, each = cant_trials)

# prepare trials column
col_trials <- 1:cant_trials
trials <- rep(col_trials, times = iSub)

# add columns to df_exp
df_exp$sujetos <- sujetos
df_exp$trials <- trials

####### get the AQ quotient 

# number of AQ sublists for each subject 
cant_componentes_por_sujetos <- 2

# number of subject 
cant_sujetos <- iSub

# location of the sublist where the responses to the AQ of the first subject are
ubicacion_comp_AQ <- 2

# load the function to get the AQ quotient  
source(root$find_file("Analysis/AuxiliaryFunctions/Nueva_funcion_AQ.R"))

# get the AQ quotient
puntaje_AQ_sujetos <- puntaje_AQ(cant_sujetos,cant_componentes_por_sujetos,ubicacion_comp_AQ)

# add to df_DatosUnicos
df_DatosUnicos$AQ <- puntaje_AQ_sujetos 


#### Explore the reaction times

## Histograms of reaction times 

library(ggplot2)
library(dplyr)

df.tiempos <- data_frame(tiempo.ensayo.discriminacion = df_exp$discrimination_t_keydown - df_exp$discrimination_t_onset,
                         tiempo.ensayo.confianza = df_exp$confidence_t_keydown -df_exp$confidence_t_onset)

# plot
ggplot(df.tiempos, aes(x=tiempo.ensayo.discriminacion))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 100)+
  ylab("count")+
  xlab("RT in discrimination task (ms)")


ggplot(df.tiempos, aes(x=tiempo.ensayo.confianza))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 100)+
  ylab("count")+
  xlab("RT in confidence task (ms)")


## Filter reaction times 
df_exp_mod <- df_exp
t_ensayo_discriminacion <- df_exp_mod$discrimination_t_keydown - df_exp_mod$discrimination_t_onset
df_exp_mod$t_ensayo_discriminacion <- t_ensayo_discriminacion 
tiempo.ensayo.discriminacion <- df_exp_mod$t_ensayo_discriminacion

# # filter by time greater than 5000ms and less than 200ms in the discrimination task
df_exp_mod <- df_exp_mod[df_exp_mod$t_ensayo_discriminacion <= 5000 &
                     df_exp_mod$t_ensayo_discriminacion >= 0,]

## Filter reaction times by standar deviation, 3 sd biger than the mean, or lesser than 200ms.
# sd.t_ensayo_discriminacion <- sd(df_exp_mod$t_ensayo_discriminacion)
# mean.t_ensayo_discriminacion <- mean(df_exp_mod$t_ensayo_discriminacion)
# df_exp_mod <- df_exp_mod[df_exp_mod$t_ensayo_discriminacion <= mean.t_ensayo_discriminacion + sd.t_ensayo_discriminacion*4 & 
#                            df_exp_mod$t_ensayo_discriminacion >= 0,]

hist(df_exp_mod$t_ensayo_discriminacion, breaks = 500, xlim = c(0, 10000))

n.trials <- nrow(df_exp)
n.trias.filter.disc.task <- nrow(df_exp_mod)

## Filter reaction times greater than 5000ms and less than 200ms in the confidence task
t_ensayo_confianza <- df_exp_mod$confidence_t_keydown -df_exp_mod$confidence_t_onset
df_exp_mod$t_ensayo_confianza <- t_ensayo_confianza 
tiempo.ensayo.confianza <- df_exp_mod$t_ensayo_confianza

# # filter by time greater than 5000ms and less than 200ms in the discrimination task
df_exp_mod <- df_exp_mod[df_exp_mod$t_ensayo_confianza <= 5000 &
                     df_exp_mod$t_ensayo_confianza >= 0,]

## Filter reaction times by standar deviation, 3 sd biger than the mean, or lesser than 200ms.
# sd.t_ensayo_confidence <- sd(df_exp_mod$t_ensayo_confianza)
# mean.t_ensayo_confidence <- mean(df_exp_mod$t_ensayo_confianza)
# df_exp_mod <- df_exp_mod[df_exp_mod$t_ensayo_confianza <= mean.t_ensayo_confidence + sd.t_ensayo_confidence*4 & 
#                            df_exp_mod$t_ensayo_confianza >= 0,]

hist(df_exp_mod$t_ensayo_confianza, breaks = 500 , xlim = c(200, 5000))

n.trias.filter.conf.task <- nrow(df_exp_mod)

# see how much trials were discarded in percentage
trials.disc.task.discarted <- ((n.trials - n.trias.filter.disc.task) * 100)/ n.trials
trials.conf.task.discarted <- ((n.trias.filter.disc.task - n.trias.filter.conf.task) * 100)/ n.trials

## Histograms of reaction times 

# after filter by reaction time
# plot
ggplot(df_exp_mod, aes(x=t_ensayo_discriminacion))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in discrimination task (ms)")

ggplot(df_exp_mod, aes(x=t_ensayo_confianza))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in confidence task (ms)")



####### metacognitive sensivity 

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))

## get metacognitive sensivity
library(dplyr)

Pc   <- rep(NA, max(df_exp_mod$sujetos)) # Percentage of correct answers
auc2 <- rep(NA, max(df_exp_mod$sujetos)) # Metacognitive sensivity

for (s in 1:max(df_exp_mod$sujetos)){
  Pc[s]   <- mean(df_exp_mod$discrimination_is_correct[df_exp_mod$sujetos==s])
  auc2[s] <- type2roc(correct = df_exp_mod$discrimination_is_correct[df_exp_mod$sujetos==s], 
                      conf = df_exp_mod$confidence_key[df_exp_mod$sujetos==s], Nratings = 4 )
}

# add to df_DatosUnicos
df_DatosUnicos$PC <- Pc
df_DatosUnicos$auc2 <- auc2

# add difference in dots in every trial to df_exp
df_exp_mod$diferencia_puntitos <- abs(df_exp_mod$dots_num_left- df_exp_mod$dots_num_right)

####### plot metacognition sensivity (red) and Percentage of correct answers (green) 
# (legends need to be corrected)

# library(ggplot2)
# library(reshape2)

# # Graphic everything messy
# sujetos <- c(1:iSub)
# y1 <- auc2 
# y2 <- Pc
# 
# ggplot(Metacog_PC_AQ, aes(sujetos)) +                   
#   geom_point(aes(y=y1), colour="red") +  
#   geom_point(aes(y=y2), colour="green") 
# 
# # graph ordering the subjects according to their metacognition sensivity
# 
# attach(Metacog_PC_AQ)
# Metacog_y_desemp_ordenado <-  Metacog_PC_AQ[order(Metacog),]
# 
# ggplot(Metacog_PC_AQ, aes(sujetos)) +                    
#   geom_point(aes(y=Metacog_y_desemp_ordenado$Metacog), colour="red") +  
#   geom_point(aes(y=Metacog_y_desemp_ordenado$PC), colour="green") +  
#   labs(title="Metacognicion y desempeno", x="Sujetos", y="proporcion", color = "Leyenda\n") +
#   scale_color_manual(labels = c("Metacognicion", "Desempeno"), values = c("red", "green")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
#         axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
#         plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))
# 

####### Unifying the format of columns values
# (horaSueno, medicacion affeccionPsico, TeEscuchamos from df_DatosUnicos)

# If the script does not recognize a certain value, it asks the user for help with it 
# (keep an eye on the console). In this case, the user must write the corresponding value 
# that is requested.
# The script saves the response in a df that the script will review the next time it is run.
# The script returns the df_DatosUnicos_mod that will have all the column values in the same format.
# In addition, in this part, the age column of the participants is created and filled
# in from their date of birth.
# It is recommended to run the script by column to unify.

## column: horaSueno

# df that will have the data in a unified and readable format.
# The values in the hoursSleep column are converted to numeric. If not possible in NA
df_DatosUnicos_mod <- transform(df_DatosUnicos, 
                                horasSueno = as.numeric(as.character(horasSueno)))

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/numeriza_col_horasSuenos.R"))

df_DatosUnicos_mod <- numeriza_col_horasSuenos(df_DatosUnicos_mod,df_DatosUnicos)

## column: edad

# save the age of the subjects in df_DatosUnicos_mod
library(eeptools)
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x),
                                                     tz = 'UTC', format = '%Y-%m-%d'))

edad <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  if(sapply(df_DatosUnicos_mod$fechaNac[i], is.convertible.to.date)){
    a <- as.Date( df_DatosUnicos_mod$fechaNac[i])
    age <- as.integer( age_calc(a, units='years') )
    edad[i] <- age
  }else{ # if it is not possible to obtain the age it is also a NA
    edad[i] <- NA
  }
}

df_DatosUnicos_mod$edad <- edad

## column: medicacion

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_medicacion.R"))

# converts the medication values in: Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_medicacion(df_DatosUnicos_mod,df_DatosUnicos)

## column: affeccionPsico

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_affeccionPsico.R"))

# converts the affeccionPsico values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_affeccionPsico(df_DatosUnicos_mod,df_DatosUnicos)

## columna: TeEscuchamos

# the corresponding function is loaded
source(root$find_file("Analysis/AuxiliaryFunctions/unifica_col_TeEscuchamos.R"))

# converts the TeEscuchamos values in Si, No , noSabe
df_DatosUnicos_mod <- unifica_col_TeEscuchamos(df_DatosUnicos_mod,df_DatosUnicos)

### Add the confidence columns to df_DatosUnicos_mod

# Confidence columns for all the subjects
confidence_key_1 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_2 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_3 <- rep(NA, nrow(df_DatosUnicos_mod))
confidence_key_4 <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  # confidence columns are created to iterate by subject
  confidence_key_1_total <- 0
  confidence_key_2_total <- 0
  confidence_key_3_total <- 0
  confidence_key_4_total <- 0
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='1',]
  confidence_key_1_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_1[i] <- confidence_key_1_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='2',]
  confidence_key_2_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_2[i] <- confidence_key_2_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='3',]
  confidence_key_3_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_3[i] <- confidence_key_3_total
  
  df_prueba <- df_exp_mod[df_exp_mod$confidence_key =='4',]
  confidence_key_4_total <- nrow(df_prueba[df_prueba$sujetos==i,])
  confidence_key_4[i] <- confidence_key_4_total
}

# Add the columns to df_DatosUnicos_mod
df_DatosUnicos_mod$confidence_key_1 <- confidence_key_1
df_DatosUnicos_mod$confidence_key_2 <- confidence_key_2
df_DatosUnicos_mod$confidence_key_3 <- confidence_key_3
df_DatosUnicos_mod$confidence_key_4 <- confidence_key_4

## Get the sd and mean of confidence by subject
media_confidence <- rep(NA, nrow(df_DatosUnicos_mod))
sd_confidence <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_confidence[i] <- mean(df_exp_mod[df_exp_mod$sujetos==i,"confidence_key"])
  sd_confidence[i] <- sd(df_exp_mod[df_exp_mod$sujetos==i,"confidence_key"])
}

df_DatosUnicos_mod$media_confidence <- media_confidence
df_DatosUnicos_mod$sd_confidence <- sd_confidence

## Get the sd and mean of reaction times by subject in the discrimination task
media_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_discri <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_discri[i] <- mean(df_exp_mod[df_exp_mod$sujetos==i,"t_ensayo_discriminacion"])
  sd_tr_discri[i] <- sd(df_exp_mod[df_exp_mod$sujetos==i,"t_ensayo_discriminacion"])
}

df_DatosUnicos_mod$media_tr_discri <- media_tr_discri
df_DatosUnicos_mod$sd_tr_discri <- sd_tr_discri

## get the sd and mean of reaction times by subject in the confidence task
media_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))
sd_tr_confi <- rep(NA, nrow(df_DatosUnicos_mod))

for(i in 1:nrow(df_DatosUnicos_mod)){
  media_tr_confi[i] <- mean(df_exp_mod[df_exp_mod$sujetos==i,"t_ensayo_confianza"])
  sd_tr_confi[i] <- sd(df_exp_mod[df_exp_mod$sujetos==i,"t_ensayo_confianza"])
}

df_DatosUnicos_mod$media_tr_confi <- media_tr_confi
df_DatosUnicos_mod$sd_tr_confi <- sd_tr_confi

####### Exclusion criteria, data from future analysis is discarded
## Comment / uncomment or modify filters as required

## Filter for hours of sleep, leaving me only with > 4
#df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$horasSueno > 4,] 

## Filter by psychological disorder, staying only with those who do not have.
df_DatosUnicos_mod2 <- df_DatosUnicos_mod[df_DatosUnicos_mod$affeccionPsico ==
                                               'No',]
## Filter by medication, leaving only with those who do not take.
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$medicacion ==
                                               'No',]
## Filter by sincericide, leaving only those who tell us that we can count on their answers.
# library (stringr)
# library (tidyverse)
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod2 %>% 
#   filter(str_detect(df_DatosUnicos_mod2$sincericidio, "Pueden")) # if start with "Pueden"
#                                                                  # it stays

## Filter by TeEscuchamos leaving only those who didnt interrup the task drastically (= ok)
# df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$TeEscuchamos ==
#                                              'ok',] 

## Filter by performance, leaving only those who have PC > 60 
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$PC > 0.60,] 

## Filter by age, leaving only those who are age > 17, < 100, and are not NA
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$edad > 17,]
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$edad < 100,]
df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[!is.na(df_DatosUnicos_mod2$edad),]

## filter in df_exp those who survived the filters applied to df_DatosUnicos_mod2
df_exp_mod2 <- df_exp_mod %>% 
  filter(df_exp_mod$sujetos %in% df_DatosUnicos_mod2$sujetos)

####### Prepare the df for the regression analysis

df_total <- df_DatosUnicos_mod2[0,]

#  sujetos que quedaron
ExistingSubjects <- unique(df_exp_mod2$sujetos)

# iterar por sujeto existente
for (i in ExistingSubjects) {
  # saco la cantidad de trials del sujeto
  sujeto_df_exp <- df_exp_mod2[df_exp_mod2$sujetos== i,]
  cant_trials <- nrow(sujeto_df_exp)
  
  # repito cada fila del sujeto segun la cantidad de trials que le quedaron
  sujeto_df_DatosUnicos_mod2 <- df_DatosUnicos_mod2[df_DatosUnicos_mod2$sujetos== i,]
  df <- as.data.frame(lapply(sujeto_df_DatosUnicos_mod2, rep, cant_trials))
  
  # lo agrago al df_total
  df_total <- rbind(df_total, df)
}

# combino las columnas de df_exp_mod2 que me interesan con el df_total
df_total <- cbind(df_total, discrimination_is_correct = df_exp_mod2$discrimination_is_correct,
                  confidence_key = df_exp_mod2$confidence_key, 
                  trials = df_exp_mod2$trials,
                  diferencia_puntitos = df_exp_mod2$diferencia_puntitos, 
                  t_ensayo_discriminacion = df_exp_mod2$t_ensayo_discriminacion,
                  t_ensayo_confianza = df_exp_mod2$t_ensayo_confianza)

## save the df_total

# # RESULTS_EXP1
# filepath <- root$find_file("Data/Results_Exp1/df_total.Rda")
# save(df_total,file = filepath)

# RESULTS_EXP2(REPLICA)
filepath <- root$find_file("Data/Results_Exp2(replica)/df_total.Rda")
save(df_total,file = filepath)

# save the df in .txt format, it is saved in the mail folder
write.table(df_total, file= 'df_total.txt')


############## HASTA ACA EL PREPROCESAMIENTO ##################################
####### Quiero observar si la una inflacion de la metacog ESTA PARTE PROBABLEMENTE NO ESTE

### Propuestas guille:

#- calcular para cada sujeto la variabilidad del est?mulo con todos los trials y luego la 
# correlacion entre la metacognicion y esa variabilidad. Tipo la Fig 4 del paper.

#- aunque no lo aconsejan ahi (no entendi por que), pondria en las regresiones que estas haciendo 
# la variabilidad tambien como variable aditiva aparte.

#- seleccionar un subconjunto de trials en los que la intensidad del estimulo se mantuvo
# relativamente constante. Quizas tomando solo la mitad de los trials (los que estan mas cercanos
# al promedio de dificultad).

media_diferencia_puntitos <- rep(NA, nrow(df_DatosUnicos_mod2))
valor_mas_frec_DifPuntitos <- rep(NA, nrow(df_DatosUnicos_mod2))
sd_diferencia_puntitos <- rep(NA, nrow(df_DatosUnicos_mod2))

for(i in 1:nrow(df_DatosUnicos_mod2)){
  s <- df_DatosUnicos_mod2$sujetos[i]
  media_diferencia_puntitos[i] <- mean(df_exp_mod2[df_exp_mod2$sujetos==s,"diferencia_puntitos"])
  la <- df_exp_mod2[df_exp_mod2$sujetos==s,"diferencia_puntitos"]
  # tomo el valor mas frecuente y lo guardo en valor_mas_frec_DifPuntitos
  valor_mas_frec_DifPuntitos[i] <- as.numeric(names(sort(summary(as.factor(la)), decreasing=T)[1]))
  sd_diferencia_puntitos[i] <- sd(df_exp_mod2[df_exp_mod2$sujetos==s,"diferencia_puntitos"])
}

df_DatosUnicos_mod2$media_diferencia_puntitos <- media_diferencia_puntitos
df_DatosUnicos_mod2$valor_mas_frec_DifPuntitos <- valor_mas_frec_DifPuntitos
df_DatosUnicos_mod2$sd_diferencia_puntitos <- sd_diferencia_puntitos

# veo si hay correlacion entre metacog y sd_diferencia_puntitos
library("ggpubr")
ggscatter(df_DatosUnicos_mod2, x = "auc2", y = "sd_diferencia_puntitos", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "metacog", ylab = "sd dif puntitos")

### no da significativo, igual rahnev y fleming 2019 usaron la metacognitive inflation en lugar de
### el puntaje de metacog, entendiendo el primero como: Metacog (all_contrast) - Metacog (1_contrast)

### saco Metacog inflation

# donde voy a meter a los df iguales a la mediana de la dificultad (dif puntitos) de cada sujeto
df_exp_mod2_1_contrast <- data.frame(t0 =character(), 
                     t_offset =character(), 
                     dots_num_left =character(), 
                     dots_num_right =character(), 
                     discrimination_is_correct =character(), 
                     discrimination_t_onset =character(), 
                     discrimination_t_keydown =character(), 
                     confidence_key =character(), 
                     confidence_t_onset=character(), 
                     confidence_t_keydown=character(),
                     sujetos=character(),
                     trials=character(),
                     t_ensayo_discriminacion=character(),
                     t_ensayo_confianza=character(),
                     diferencia_puntitos=character(),
                     stringsAsFactors=FALSE) 

for(i in 1:nrow(df_DatosUnicos_mod2)){
  s <- df_DatosUnicos_mod2$sujetos[i]
  df <- df_exp_mod2[df_exp_mod2$sujetos==s,]
  df_2 <- df[df$diferencia_puntitos == df_DatosUnicos_mod2$valor_mas_frec_DifPuntitos[i],]
  df_exp_mod2_1_contrast <- rbind(df_exp_mod2_1_contrast,df_2)
}

# veo cantidad de trial que me quedaron por sujeto
cant_trials <- rep(NA, nrow(df_DatosUnicos_mod2))
for(i in 1:nrow(df_DatosUnicos_mod2)){
  s <- df_DatosUnicos_mod2$sujetos[i]
  cant_trials[i] <- nrow(df_exp_mod2_1_contrast[df_exp_mod2_1_contrast$sujetos==s,])
}

# hay algunos con muy pocos ensayos ej: 7, 12, 8, considerar sacarlos de esta comparacion

# filtro a los sujetos quienes tienen menos de 20 trials QUEDE ACA!!!!
#df_exp_mod3_1_contrast <- df_exp_mod2_1_contrast[df_exp_mod2_1_contrast$sujetos == ]

## ahora si, indice de metacog en df de un contraste

# cargo la funcion para sacar el area bajo la curva tipo 2  
if (Sys.info()[['sysname']] =='Linux'){
  source('/home/pablo/Dropbox/trabajo/compartidos/Metacog_TEA_exp_online/Scripts/ScriptsActuales/auroc2.R')
}else{
  source('C:/Users/marcosembon/Dropbox/Metacog_TEA_exp_online/Scripts/ScriptsActuales/auroc2.R')
}

library(dplyr)
Pc   <- rep(NA, max(df_exp_mod2_1_contrast$sujetos)) # porcentaje de respuestas correctas
auc2 <- rep(NA, max(df_exp_mod2_1_contrast$sujetos)) # puntaje de metacog

for (s in 1:max(df_exp_mod2_1_contrast$sujetos)){
  Pc[s]   <- mean(df_exp_mod2_1_contrast$discrimination_is_correct[df_exp_mod2_1_contrast$sujetos==s])
  auc2[s] <- type2roc(correct = df_exp_mod2_1_contrast$discrimination_is_correct[df_exp_mod2_1_contrast$sujetos==s], 
                      conf = df_exp_mod2_1_contrast$confidence_key[df_exp_mod2_1_contrast$sujetos==s], Nratings = 4 )
}

# lo agrego al df_DatosUnicos
df_DatosUnicos_mod2$PC_1_contr <- Pc
df_DatosUnicos_mod2$auc21_contr <- auc2

# agrego metacog inflation al df : Metacog (all_contrast) - Metacog (1_contrast)
df_DatosUnicos_mod2$MetacogInflation <- df_DatosUnicos_mod2$auc2 - df_DatosUnicos_mod2$auc21_contr

# veo si hay correlacion entre MetacogInflation y sd_diferencia_puntitos
library("ggpubr")
ggscatter(df_DatosUnicos_mod2, x = "MetacogInflation", y = "sd_diferencia_puntitos", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MetacogInflation", ylab = "sd dif puntitos")


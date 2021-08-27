###############
### library ###
###############
library(dplyr)
library(ggplot2)

#################
### DataFrame ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = 1,2,ambos
DF_list <- DataFrame_Filtered(experimento = "ambos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20)

# DF_list:
# a df_total
# b d.sin.normalizar
# c d.sin.normalizar.mc.filter
# d d
# e d.mc.filter
# f d.sin.normalizar.solo.FyM
# g d.sin.normalizar.solo.FyM.mc.filter
# h d.solo.FyM.mc.filter

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b

 
# sub auroc2 small than 0.5 
indices <- which(d.sin.normalizar$mc < 0.5)

# recreating the dfs, we need the first 10 trials and not any time limit
DF_list <- DataFrame_Filtered(experimento = "ambos", 
                              filtroRT_Disc_Sup = 5000000,
                              filtroRT_Disc_Inf = 0,
                              filtroRT_Conf_Sup = 5000000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 0)

df_total <- DF_list$a

# df_total sin ningun filtro
existing_subject <- unique(df_total$sujetos)

# nos quedamos con los indices de aquellos que tienen auroc2 menor a 0.5
id_subj_smallAuroc <- existing_subject[indices]

for (i in 1:length(id_subj_smallAuroc)) {
  # vamos viendo sujeto por sujeto 
  df <- df_total [df_total$sujetos == id_subj_smallAuroc[i],] # 2 es un buen sujeto

  # nos qedamos con los trials a plotear
  df_lessTrials<- df[df$trials < 11,]
  
  
  myplot<- ggplot(df_lessTrials,aes(x= confidence_key , fill = discrimination_is_correct)) + 
    geom_histogram(data=subset(df_lessTrials,
                               discrimination_is_correct == 'TRUE'),aes(fill=discrimination_is_correct), alpha = 0.2) +
    geom_histogram(data=subset(df_lessTrials,
                               discrimination_is_correct == 'FALSE'),aes(fill=discrimination_is_correct), alpha = 0.2)+
    scale_fill_manual(name="is correct", values=c("red","blue"),labels=c("TRUE","FALSE")) +
    labs(title= paste("Participant",as.character(id_subj_smallAuroc[i]), sep = " "),x="confidence", y="trials", color = "")+
  theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(1, 1,1, 1, "cm"),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 20),
          panel.background = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.title.x = element_text(size = 30)) 
  
  # guardamos la figura:
  png(paste("Hist_ConfidenceKey_CorrectIncorrect_suj", as.character(id_subj_smallAuroc[i]) , ".png", sep = ""))
  print(myplot)
  dev.off()
}


df <- df_total [df_total$sujetos == 182,] # 2 es un buen sujeto

# nos qedamos con los trials a plotear
df_lessTrials<- df[df$trials < 11,]


myplot<- ggplot(df_lessTrials,aes(x= confidence_key , fill = discrimination_is_correct)) + 
  geom_histogram(data=subset(df_lessTrials,
                             discrimination_is_correct == 'TRUE'),aes(fill=discrimination_is_correct), alpha = 0.2) +
  scale_fill_manual(name="is correct", values=c("blue","red")) +
  labs(title= paste("Participant",as.character(2), sep = " "),x="confidence", y="trials")+ 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

if (nrow(df_lessTrials[df_lessTrials$discrimination_is_correct == "FALSE",]) >0){
  myplot +geom_histogram(data=subset(df_lessTrials,
                                     discrimination_is_correct == 'FALSE'),
                         aes(fill=discrimination_is_correct), alpha = 0.2)}

png(paste("ELIMINAME", as.character(id_subj_smallAuroc[i]) , ".png", sep = ""))
print(myplot)
dev.off()

######## tratando de simular la probabilidad de cierto puntaje de mc si esta al nivel de chance

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())


# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = 1,2,ambos
DF_list <- DataFrame_Filtered(experimento = "ambos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 20)

df_total <- DF_list$a
d.sin.normalizar <- DF_list$b

# cantidad de trials luego del descarte de los primeros X trials
cant_trials <- 110

# simulo el estimulo izq=0 der=1
stim <- sample(c(0,1), cant_trials, replace = TRUE) # 110 porque los primeros 20 se descartan

# saco el 75 % de la cantidad de trials
cant_trials_correctos <- round((75* cant_trials)/100)

# que el sujeto responda correctamente al 75 % de trials
resp <- c(stim[1:cant_trials_correctos] , rep(3, cant_trials- cant_trials_correctos))

# respuestas correctas
is_correct <-as.numeric(stim == resp)


## preparo para sacar la metacog por sujeto

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))

## get metacognitive sensivity
library(dplyr)

# nro de repeticiones que quiero simular
n_rep <- 10000

mc <- rep(NaN, n_rep)

for (i in 1:length(mc)){
  # selecciono las respuestas de confianza al azar
  conf <- sample(c(1,2,3,4), cant_trials, replace = TRUE)
  # saco metacog y guardo
  mc[i] <- type2roc(correct = is_correct, 
                    conf = conf, 
                    Nratings = 4)}

# corroboro que el histograma este bien
hist(mc)

# saco la probabilidad de que el participante obtenga cierto puntaje de metacog
# dado a que esta respondiendo con las teclas de confianza al azar

prob_metacog_AlAzar <- rep(NaN,nrow(d.sin.normalizar))

for (i in 1:length(prob_metacog_AlAzar)) {
  p <- mc[mc < d.sin.normalizar$mc[i]]
  prob_metacog_AlAzar[i] <- length(p)/n_rep
}

df_metacog <- data.frame(prob_metacog_AlAzar = prob_metacog_AlAzar,
                         mc = d.sin.normalizar$mc)

df_metacog_sorted <- df_metacog[order(df_metacog$mc),]



######## tratando de simular la mc en base a cuantos trials se considera para sacarla

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

### preparo para sacar la metacog por sujeto

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))
## get metacognitive sensivity
library(dplyr)

# creo un vector que tenga los posibles trials totales con los cuales sacare
# el puntaje de metacog
cant_trials_totales <- 4:130

# cantidad de sujetos que quiero simular. Todos con 75% de aciertos y 
# metacog de 0.5 (las respuestas de confianza al azar)
Nsim <- 32

# creo un df en donde va a guardar las columnas de la cant de trials totales y 
# las simulasiones de metacog
df_metacog_by_trials <- data_frame(cant_trials_totales = cant_trials_totales)


for (j in 1:Nsim) {
  # creo el vector que va a guardar el score de metacog para cada cantidad de trials
  mc <- rep(NaN, length(cant_trials_totales))
  
  for (i in 1:length(cant_trials_totales)) {
    
    ### repuestas correctas en un 75% de acierto
    # saco el 75 % de la cantidad de trials
    cant_trials_correctos <- round((75* cant_trials_totales[i])/100)
    
    # creo un vector con respuestas correctas
    is_correct <- c(rep(1,cant_trials_correctos), rep(0,cant_trials_totales[i]-cant_trials_correctos))
    
    # mezclo las respuestas correctas e incorrectas
    correct <- sample(is_correct)
    # selecciono las respuestas de confianza al azar
    conf <- sample(c(1,2,3,4), cant_trials_totales[i], replace = TRUE)
    # saco metacog y guardo
    mc[i] <- type2roc(correct = is_correct, 
                      conf = conf, 
                      Nratings = 4)}
  
  ### guardo la metacog en el dataframe
  # la cargo al df
  df_metacog_by_trials[ , ncol(df_metacog_by_trials) + 1] <- mc
  # la renombro
  colnames(df_metacog_by_trials)[ncol(df_metacog_by_trials)] <- paste0("mc", j) 
}

# saco la desviacion estandar de cada fila(trial)
library(dplyr)
library(matrixStats)
a <-df_metacog_by_trials%>%mutate(STDEV=rowSds(as.matrix(.[c("mc1","mc2","mc3","mc4",
                                                         "mc5","mc6","mc7","mc8",
                                                         "mc9","mc10","mc11","mc12",
                                                         "mc13","mc14","mc15","mc16",
                                                         "mc17","mc18","mc19","mc20",
                                                         "mc21","mc22","mc23","mc24",
                                                         "mc25","mc26","mc27","mc28",
                                                         "mc29","mc30","mc31","mc32")])))



# ploteo metacog segun la cant de trials con la que fue sacado
plot(cant_trials_totales,a$STDEV)

ggplot(data = a) + 
  geom_point(mapping = aes(x = cant_trials_totales, y = STDEV))+
  xlab("Cantidad de trials para calcular la mc") +
  ylab("STD de metacog calculada (32 sujetos simulados)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 15),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 15))






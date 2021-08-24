###############
### library ###
###############
library(tidyverse)
library(ggridges)
library(matrixStats)
library(dplyr)
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)

#################
### Staircase ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

# get the df list
# experimento = 1,2,ambos
DF_list <- DataFrame_Filtered(experimento = "ambos",
                              filtroRT_Disc_Sup = 20000,
                              filtroRT_Disc_Inf = 0,
                              filtroRT_Conf_Sup = 20000,
                              filtroRT_Conf_Inf = 0,
                              filtroTrial = 0)

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
d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g

### plotting the performance by trial

d1 <- df_total
d1 <- d1[d1$genero == 'Femenino' | d1$genero == 'Masculino',]

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza
d2 <- d1[d1$sujetos != 57 & d1$sujetos != 63 & d1$sujetos != 83
         & d1$sujetos != 109 & d1$sujetos != 1029 & d1$sujetos != 1121
         & d1$sujetos != 1159 & d1$sujetos != 1193 & d1$sujetos != 36
         & d1$sujetos != 170 & d1$sujetos != 1081 & d1$sujetos != 1086
         & d1$sujetos != 1095 & d1$sujetos != 1110 & d1$sujetos != 1172, ]

# sujetos que tienen menos de 90 de trials
d4 <- d2[d2$sujetos != 55 & d2$sujetos != 57 & d2$sujetos != 83 &
           d2$sujetos != 122 & d2$sujetos != 131 & d2$sujetos != 141 &
           d2$sujetos != 172 & d2$sujetos != 173 & d2$sujetos != 179 &
           d2$sujetos != 189 & d2$sujetos != 193 & d2$sujetos != 195 &
           d2$sujetos != 1010 & d2$sujetos != 1046 &
           d2$sujetos != 1069 & d2$sujetos != 1112 &
           d2$sujetos != 1127 & d2$sujetos != 1135 &
           d2$sujetos != 1154 & d2$sujetos != 1171 &
           d2$sujetos != 1191 & d2$sujetos != 1239 &
           d2$sujetos != 1250 & d2$sujetos != 1251 &
           d2$sujetos != 1260,]

d3 <- d.sin.normalizar.solo.FyM.mc.filter 
# saco a los que tienen metacog menor a 1,5 de desvio de la media para abajo
d5 <- d4[d4$sujetos %in% d3$sujetos,] # d3 es el df de datos unicos, ya con la 
# metacog filtrada a 1.5 de desvio

d5$discrimination_is_correct[d5$discrimination_is_correct=='TRUE'] <- "1"
d5$discrimination_is_correct[d5$discrimination_is_correct=='FALSE'] <- "0"

total_trials <- max(d5$trials)-min(d5$trials)

MeanPerformanceByTrial <- rep(NA,total_trials)
#sd <- rep(NA,total_trials)

for (i in 1:total_trials){
  trial_colum <- d5[d5$trials == i,]  # getting data by trial
  MeanPerformanceByTrial[i] <- mean(as.integer(trial_colum$discrimination_is_correct))
  #sd[i] <- sd(as.integer(trial_colum$discrimination_is_correct))
}

df2 <- data.frame(TrialNumber = 1:length(MeanPerformanceByTrial),
                  MeanPerformanceByTrial = MeanPerformanceByTrial)

ggplot(data=df2, aes(x=TrialNumber, y=MeanPerformanceByTrial)) +
  geom_line( color="black", size=1.2)+
  #geom_point(color="red", size=3) +
  scale_x_continuous(expand = c(0, 0)) + #scale_y_continuous(expand = c(0, 0))
  xlab("Trial number") + ylab("Performance mean")+
  geom_vline(xintercept =20, linetype="dashed", color = "black")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 


#####################
### AQ histograms ###
#####################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

#### AQ by sex
DF_list <- DataFrame_Filtered(experimento = "ambos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 100,
                              filtroTrial = 20)


d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g # 

d1 = d.sin.normalizar.solo.FyM.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza
d2 <- d1[d1$sujetos != 57 & d1$sujetos != 63 & d1$sujetos != 83
         & d1$sujetos != 109 & d1$sujetos != 1029 & d1$sujetos != 1121
         & d1$sujetos != 1159 & d1$sujetos != 1193 & d1$sujetos != 36
         & d1$sujetos != 170 & d1$sujetos != 1081 & d1$sujetos != 1086
         & d1$sujetos != 1095 & d1$sujetos != 1110 & d1$sujetos != 1172, ]

# sujetos que tienen menos de 90 de trials
d3 <- d2[d2$sujetos != 55 & d2$sujetos != 57 & d2$sujetos != 83 &
           d2$sujetos != 122 & d2$sujetos != 131 & d2$sujetos != 141 &
           d2$sujetos != 172 & d2$sujetos != 173 & d2$sujetos != 179 &
           d2$sujetos != 189 & d2$sujetos != 193 & d2$sujetos != 195 &
           d2$sujetos != 1010 & d2$sujetos != 1046 &
           d2$sujetos != 1069 & d2$sujetos != 1112 &
           d2$sujetos != 1127 & d2$sujetos != 1135 &
           d2$sujetos != 1154 & d2$sujetos != 1171 &
           d2$sujetos != 1191 & d2$sujetos != 1239 &
           d2$sujetos != 1250 & d2$sujetos != 1251 &
           d2$sujetos != 1260,]


# a
d3[d3 == "Masculino"] <- 'Male' 
d3[d3 == "Femenino"] <- 'Female' 

# ploteo para hombres y mujeres por separado

# mujeres
ggplot(d3,aes(aq))+
  geom_bar(data=subset(d3, Im == 'Female'),
           fill = "black", alpha = 0.8)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Female AQ") +
  ylab("Participants")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

# hombres
ggplot(d3,aes(aq))+
  geom_bar(data=subset(d3, Im == 'Male'),
           fill = "black", alpha = 0.8)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Male AQ") +
  ylab("Participants")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))


##########################
### AUROC2-Performance ###
##########################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered.R"))

#### AQ by sex
DF_list <- DataFrame_Filtered(experimento = "ambos", 
                              filtroRT_Disc_Sup = 5000,
                              filtroRT_Disc_Inf = 200,
                              filtroRT_Conf_Sup = 5000,
                              filtroRT_Conf_Inf = 100,
                              filtroTrial = 20)


d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g # 

d1 = d.sin.normalizar.solo.FyM.mc.filter

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza
d2 <- d1[d1$sujetos != 57 & d1$sujetos != 63 & d1$sujetos != 83
         & d1$sujetos != 109 & d1$sujetos != 1029 & d1$sujetos != 1121
         & d1$sujetos != 1159 & d1$sujetos != 1193 & d1$sujetos != 36
         & d1$sujetos != 170 & d1$sujetos != 1081 & d1$sujetos != 1086
         & d1$sujetos != 1095 & d1$sujetos != 1110 & d1$sujetos != 1172, ]

# sujetos que tienen menos de 90 de trials
d3 <- d2[d2$sujetos != 55 & d2$sujetos != 57 & d2$sujetos != 83 &
           d2$sujetos != 122 & d2$sujetos != 131 & d2$sujetos != 141 &
           d2$sujetos != 172 & d2$sujetos != 173 & d2$sujetos != 179 &
           d2$sujetos != 189 & d2$sujetos != 193 & d2$sujetos != 195 &
           d2$sujetos != 1010 & d2$sujetos != 1046 &
           d2$sujetos != 1069 & d2$sujetos != 1112 &
           d2$sujetos != 1127 & d2$sujetos != 1135 &
           d2$sujetos != 1154 & d2$sujetos != 1171 &
           d2$sujetos != 1191 & d2$sujetos != 1239 &
           d2$sujetos != 1250 & d2$sujetos != 1251 &
           d2$sujetos != 1260,]


mc.sorted <-  d3[order(d3$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(x = s, y=mc, colour="AUROC2")) +  
  geom_point(aes(x = s, y=pc, colour="Performance")) +  
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  scale_x_continuous(expand = c(.009, 0)) +
  labs(x="Participants", y="", color = "") +
  #scale_color_manual(values = c("mc" = "black", "pc" = "red"))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        #legend.title = element_text(size = 20),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

########################
### Regression model ###
########################

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
                              filtroRT_Conf_Inf = 100,
                              filtroTrial = 20)

d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g 

### lineas para hacer regresion 

d.sin.normalizar.solo.FyM.mc.filter$aq.norm <- (d.sin.normalizar.solo.FyM.mc.filter$aq - mean(d.sin.normalizar.solo.FyM.mc.filter$aq))/
  sd(d.sin.normalizar.solo.FyM.mc.filter$aq)

d.sin.normalizar.solo.FyM.mc.filter$mc.norm <- (d.sin.normalizar.solo.FyM.mc.filter$mc 
                                                - mean(d.sin.normalizar.solo.FyM.mc.filter$mc))/ sd(d.sin.normalizar.solo.FyM.mc.filter$mc)

d.sin.normalizar.solo.FyM.mc.filter$edad.norm <- (d.sin.normalizar.solo.FyM.mc.filter$edad 
                                                  - mean(d.sin.normalizar.solo.FyM.mc.filter$edad))/ sd(d.sin.normalizar.solo.FyM.mc.filter$edad)

d1 = d.sin.normalizar.solo.FyM.mc.filter
d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)

# sujetos que tienen un 85 % de trials en una misma respuesta de confianza
d2 <- d1[d1$sujetos != 57 & d1$sujetos != 63 & d1$sujetos != 83
         & d1$sujetos != 109 & d1$sujetos != 1029 & d1$sujetos != 1121
         & d1$sujetos != 1159 & d1$sujetos != 1193 & d1$sujetos != 36
         & d1$sujetos != 170 & d1$sujetos != 1081 & d1$sujetos != 1086
         & d1$sujetos != 1095 & d1$sujetos != 1110 & d1$sujetos != 1172, ]

# sujetos que tienen menos de 90 de trials
d3 <- d2[d2$sujetos != 55 & d2$sujetos != 57 & d2$sujetos != 83 &
           d2$sujetos != 122 & d2$sujetos != 131 & d2$sujetos != 141 &
           d2$sujetos != 172 & d2$sujetos != 173 & d2$sujetos != 179 &
           d2$sujetos != 189 & d2$sujetos != 193 & d2$sujetos != 195 &
           d2$sujetos != 1010 & d2$sujetos != 1046 &
           d2$sujetos != 1069 & d2$sujetos != 1112 &
           d2$sujetos != 1127 & d2$sujetos != 1135 &
           d2$sujetos != 1154 & d2$sujetos != 1171 &
           d2$sujetos != 1191 & d2$sujetos != 1239 &
           d2$sujetos != 1250 & d2$sujetos != 1251 &
           d2$sujetos != 1260,]

# corro el modelo
a=lm(mc ~ aq.norm +
       Im +
       edad.norm+
       aq.norm: Im+
       aq.norm:edad.norm,
     data = d3) 
summary(a)


plot_summs(a, coefs = c('AQ' = 'aq.norm','Gender-Male'='Im','Age' = 'edad.norm',
                        'AQ:Gender-Male'='aq.norm:Im','AQ:Age'='aq.norm:edad.norm') ,
           plot.distributions = FALSE, colors = "black")+
  ylab("") +
  xlab("Regression coefficient") +
  scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))


###################
### Interaccion ###
###################

p <- plot_model(a, type = "pred", terms = c("aq.norm", "Im"),
                axis.labels = c('AQ standardized','AUROC2'),
                legend.title = '',
                title = '', 
                axis.title = c('AQ standardized','AUROC2'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)






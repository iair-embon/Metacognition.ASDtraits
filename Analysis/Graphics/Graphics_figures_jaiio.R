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

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# a = df_total.solo.FyM
# b = d.sin.normalizar
# c = d.normalizado
# d = d.sin.normalizar.solo.FyM
d3 <- DF_list$d

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

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# a = df_total.solo.FyM
# b = d.sin.normalizar
# c = d.normalizado
# d = d.sin.normalizar.solo.FyM
d3 <- DF_list$d

mc.sorted <-  d3[order(d3$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(x = s, y=mc, colour="Metacognition")) +  
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

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# a = df_total.solo.FyM
# b = d.sin.normalizar
# c = d.normalizado
# d = d.sin.normalizar.solo.FyM
d.sin.normalizar.solo.FyM <- DF_list$d

###############
### library ###
###############
library(arm)
library(dplyr)

###########################
### Regression Analysis ###
###########################

### lineas para hacer regresion 

d1 = d.sin.normalizar.solo.FyM

d1$aq.norm <- (d1$aq - mean(d1$aq))/ sd(d1$aq)

d1$mc.norm <- (d1$mc - mean(d1$mc))/ sd(d1$mc)

d1$edad.norm <- (d1$edad - mean(d1$edad))/ sd(d1$edad)

d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)

# corro el modelo
a=lm(mc ~ aq.norm +
       Im +
       edad.norm+
       aq.norm: Im+
       aq.norm:edad.norm,
     data = d1) 
summary(a)


plot_summs(a, coefs = c('AQ' = 'aq.norm','Gender'='Im','Age' = 'edad.norm',
                        'AQ:Gender'='aq.norm:Im','AQ:Age'='aq.norm:edad.norm') ,
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
                axis.labels = c('standardized AQ','Metacognition'),
                legend.title = '',
                title = '', 
                axis.title = c('standardized AQ','Metacognition'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)


########################
### AUROC2-GENDER-AQ ###
########################

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# a = df_total.solo.FyM
# b = d.sin.normalizar
# c = d.normalizado
# d = d.sin.normalizar.solo.FyM
df_total.solo.FyM <- DF_list$a
d.sin.normalizar.solo.FyM <- DF_list$d

# create the df for plot
df_for_plot <- data.frame(Nsuj = d.sin.normalizar.solo.FyM$sujetos,
                          Gender = d.sin.normalizar.solo.FyM$Im,
                          AQ = d.sin.normalizar.solo.FyM$aq)


Nsuj <- length(unique(df_total.solo.FyM$sujetos))
ExistingSubjects <- unique(df_total.solo.FyM$sujetos)

# create empty vectors to save the cumH2 and cumF2 for each subject
cum_H2_0 <- c()
cum_H2_1 <- c()
cum_H2_2 <- c()
cum_H2_3 <- c()
cum_H2_4 <- c()
cum_FA2_0 <- c()
cum_FA2_1 <- c()
cum_FA2_2 <- c()
cum_FA2_3 <- c()
cum_FA2_4 <- c()

for (i in 1:Nsuj) {
  correct <- df_total.solo.FyM$discrimination_is_correct[df_total.solo.FyM$sujetos==ExistingSubjects[i]]
  conf <- df_total.solo.FyM$confidence_key[df_total.solo.FyM$sujetos==ExistingSubjects[i]]
  Nratings <- 4
  
  H2  <- rep(NA, Nratings)
  FA2 <- rep(NA, Nratings)
  i   <- Nratings+1
  for (c in 1:Nratings){
    H2[i-1]  <- sum(conf == c & correct) + 0.5
    FA2[i-1] <- sum(conf == c & !correct) + 0.5
    i        <- i-1
  }
  
  H2      <- H2/sum(H2)
  FA2     <- FA2/sum(FA2)
  cum_H2  <- append(0, cumsum(H2))
  cum_FA2 <- append(0, cumsum(FA2))
  
  cum_H2_0 <- c(cum_H2_0,cum_H2[1])
  cum_H2_1 <- c(cum_H2_1,cum_H2[2])
  cum_H2_2 <- c(cum_H2_2,cum_H2[3])
  cum_H2_3 <- c(cum_H2_3,cum_H2[4])
  cum_H2_4 <- c(cum_H2_4,cum_H2[5])
  cum_FA2_0 <- c(cum_FA2_0,cum_FA2[1])
  cum_FA2_1 <- c(cum_FA2_1,cum_FA2[2])
  cum_FA2_2 <- c(cum_FA2_2,cum_FA2[3])
  cum_FA2_3 <- c(cum_FA2_3,cum_FA2[4])
  cum_FA2_4 <- c(cum_FA2_4,cum_FA2[5])
}

# create empty df to save the cumH2 and cumF2 for each subject
df_for_plot$cum_H2_0  <- cum_H2_0
df_for_plot$cum_H2_1  <- cum_H2_1
df_for_plot$cum_H2_2  <- cum_H2_2
df_for_plot$cum_H2_3  <- cum_H2_3
df_for_plot$cum_H2_4  <- cum_H2_4
df_for_plot$cum_FA2_0 <- cum_FA2_0
df_for_plot$cum_FA2_1 <- cum_FA2_1
df_for_plot$cum_FA2_2 <- cum_FA2_2
df_for_plot$cum_FA2_3 <- cum_FA2_3
df_for_plot$cum_FA2_4 <- cum_FA2_4


# PLOT AUROC2 FOR MALE PARTICIPANTS
df_for_plot_male <-  df_for_plot[df_for_plot$Gender == "Masculino",]

# split by AQ first and last quartiles
df_for_plot_male_AQ_1 <- df_for_plot_male[df_for_plot_male$AQ <= quantile(df_for_plot_male$AQ)[[2]],]
df_for_plot_male_AQ_4 <- df_for_plot_male[df_for_plot_male$AQ >= quantile(df_for_plot_male$AQ)[[4]],]

cum_H2_male_AQ_1 <- c(mean(df_for_plot_male_AQ_1$cum_H2_0),
                 mean(df_for_plot_male_AQ_1$cum_H2_1),
                 mean(df_for_plot_male_AQ_1$cum_H2_2),
                 mean(df_for_plot_male_AQ_1$cum_H2_3),
                 mean(df_for_plot_male_AQ_1$cum_H2_4))

cum_FA2_male_AQ_1 <- c(mean(df_for_plot_male_AQ_1$cum_FA2_0),
                 mean(df_for_plot_male_AQ_1$cum_FA2_1),
                 mean(df_for_plot_male_AQ_1$cum_FA2_2),
                 mean(df_for_plot_male_AQ_1$cum_FA2_3),
                 mean(df_for_plot_male_AQ_1$cum_FA2_4))

cum_H2_male_AQ_4 <- c(mean(df_for_plot_male_AQ_4$cum_H2_0),
                      mean(df_for_plot_male_AQ_4$cum_H2_1),
                      mean(df_for_plot_male_AQ_4$cum_H2_2),
                      mean(df_for_plot_male_AQ_4$cum_H2_3),
                      mean(df_for_plot_male_AQ_4$cum_H2_4))

cum_FA2_male_AQ_4 <- c(mean(df_for_plot_male_AQ_4$cum_FA2_0),
                       mean(df_for_plot_male_AQ_4$cum_FA2_1),
                       mean(df_for_plot_male_AQ_4$cum_FA2_2),
                       mean(df_for_plot_male_AQ_4$cum_FA2_3),
                       mean(df_for_plot_male_AQ_4$cum_FA2_4))


AQ_1 <- mean(df_for_plot_male_AQ_1$AQ)
AQ_4 <- mean(df_for_plot_male_AQ_4$AQ)

df <- data.frame(cum_H2_male_AQ_1 = cum_H2_male_AQ_1,
                 cum_H2_male_AQ_4 = cum_H2_male_AQ_4,
                 cum_FA2_male_AQ_1 = cum_FA2_male_AQ_1,
                 cum_FA2_male_AQ_4 = cum_FA2_male_AQ_1)

ggplot(df) +                   
  geom_line(aes(x = cum_FA2_male_AQ_1, y=cum_H2_male_AQ_1, colour="Low AQ"),size = 3) +  
  geom_line(aes(x = cum_FA2_male_AQ_4, y=cum_H2_male_AQ_4, colour="High AQ"),size = 3) +  
  geom_abline(intercept = 0, slope = 1, color = "grey", size = 2) +
  scale_y_continuous(expand = expansion(mult = c(.02, .1)))+
  scale_x_continuous(expand = c(.02, 0)) +
  labs(x="Participants", y="", color = "") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 



# PLOT AUROC2 FOR FEMALE PARTICIPANTS
df_for_plot_female <-  df_for_plot[df_for_plot$Gender == "Femenino",]

# split by AQ first and last quartiles
df_for_plot_female_AQ_1 <- df_for_plot_female[df_for_plot_female$AQ <= quantile(df_for_plot_female$AQ)[[2]],]
df_for_plot_female_AQ_4 <- df_for_plot_female[df_for_plot_female$AQ >= quantile(df_for_plot_female$AQ)[[4]],]

cum_H2_female_AQ_1 <- c(mean(df_for_plot_female_AQ_1$cum_H2_0),
                      mean(df_for_plot_female_AQ_1$cum_H2_1),
                      mean(df_for_plot_female_AQ_1$cum_H2_2),
                      mean(df_for_plot_female_AQ_1$cum_H2_3),
                      mean(df_for_plot_female_AQ_1$cum_H2_4))

cum_FA2_female_AQ_1 <- c(mean(df_for_plot_female_AQ_1$cum_FA2_0),
                       mean(df_for_plot_female_AQ_1$cum_FA2_1),
                       mean(df_for_plot_female_AQ_1$cum_FA2_2),
                       mean(df_for_plot_female_AQ_1$cum_FA2_3),
                       mean(df_for_plot_female_AQ_1$cum_FA2_4))

cum_H2_female_AQ_4 <- c(mean(df_for_plot_female_AQ_4$cum_H2_0),
                      mean(df_for_plot_female_AQ_4$cum_H2_1),
                      mean(df_for_plot_female_AQ_4$cum_H2_2),
                      mean(df_for_plot_female_AQ_4$cum_H2_3),
                      mean(df_for_plot_female_AQ_4$cum_H2_4))

cum_FA2_female_AQ_4 <- c(mean(df_for_plot_female_AQ_4$cum_FA2_0),
                       mean(df_for_plot_female_AQ_4$cum_FA2_1),
                       mean(df_for_plot_female_AQ_4$cum_FA2_2),
                       mean(df_for_plot_female_AQ_4$cum_FA2_3),
                       mean(df_for_plot_female_AQ_4$cum_FA2_4))


AQ_1 <- mean(df_for_plot_female_AQ_1$AQ)
AQ_4 <- mean(df_for_plot_female_AQ_4$AQ)

df <- data.frame(cum_H2_female_AQ_1 = cum_H2_female_AQ_1,
                 cum_H2_female_AQ_4 = cum_H2_female_AQ_4,
                 cum_FA2_female_AQ_1 = cum_FA2_female_AQ_1,
                 cum_FA2_female_AQ_4 = cum_FA2_female_AQ_1)

ggplot(df) +                   
  geom_line(aes(x = cum_FA2_female_AQ_1, y=cum_H2_female_AQ_1, colour="Low AQ"),size = 3) +  
  geom_line(aes(x = cum_FA2_female_AQ_4, y=cum_H2_female_AQ_4, colour="High AQ"),size = 3) +  
  geom_abline(intercept = 0, slope = 1, color = "grey", size = 2) +
  scale_y_continuous(expand = expansion(mult = c(.02, .1)))+
  scale_x_continuous(expand = c(.02, 0)) +
  labs(x="Participants", y="", color = "") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 





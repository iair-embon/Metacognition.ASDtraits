###############
### library ###
###############
library(tidyverse)
library(ggridges)
library(matrixStats)
library(arm)
library(jtools)
library(broom.mixed)
library(TMB)
library(sjPlot)
library(dotwhisker)
library(reshape2)
library(ggExtra)

########################
### Normal densities ###  FIG 1 B
########################

# close curves
N1 <- 100000
N2 <- 100000
m1 <- 0
m2 <- 1
s1 <- 1
s2 <- 1
X1 <- rnorm(n=N1, mean=m1, sd=s1)
X2 <- rnorm(n=N2, mean=m2, sd=s2)

x <- data.frame(Stim1=X1,Stim2=X2)
data<- melt(x)

ggplot(data,aes(x=value, color = variable)) + 
  geom_density(alpha=0.25,size=2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_vline(xintercept =(m1+m2)/2, linetype="dashed", color = "black")+
  scale_color_grey(start = 0.6, end = 0) + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

# distant curves
N1 <- 100000
N2 <- 100000
m1 <- 0
m2 <- 5
s1 <- 1
s2 <- 1
X1 <- rnorm(n=N1, mean=m1, sd=s1)
X2 <- rnorm(n=N2, mean=m2, sd=s2)

x <- data.frame(Stim1=X1,Stim2=X2)
data<- melt(x)

ggplot(data,aes(x=value, color = variable)) + 
  geom_density(alpha=0.25,size=2)+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_vline(xintercept =(m1+m2)/2, linetype="dashed", color = "black")+
  scale_color_grey(start = 0.6, end = 0) + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.position = 'none',
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

###################################
### AUROC2 close-distant curves ### FIG 1 C
###################################

# close
df <- data.frame(H2 = c(0,0.2325767,0.5870690,0.8445881,1), 
                 FA2 = c(0,0.1225967,0.4390442,0.7550646,1))

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3, color = 'white') +  # la pinto de blanco y la edito en inskape
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype="dashed", 
              size = 1.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 

# distant
df <- data.frame(H2 = c(0,0.5,0.85,0.96,1), 
                 FA2 = c(0,0.1225967,0.4390442,0.7550646,1))

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3) +  
  geom_abline(intercept = 0, slope = 1, color = "grey", size = 2) +
  scale_y_continuous(expand = expansion(mult = c(.02, .1)))+
  scale_x_continuous(expand = c(.02, 0)) +
  labs(x="", y="", color = "") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 



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

d3 <- DF_list$d

d3[d3 == "Masculino"] <- 'Male' 
d3[d3 == "Femenino"] <- 'Female' 

# female
ggplot(d3, aes(x = aq))+
  geom_bar(data=subset(d3, Im == 'Female'), fill = "black")+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  #xlab("AQ") +
  #ylab("Participants")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())

# male
ggplot(d3, aes(x = aq))+
  geom_bar(data=subset(d3, Im == 'Male'), fill = "grey")+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  #xlab("AQ") +
  #ylab("Participants")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())

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

d3 <- DF_list$d

mc.sorted <-  d3[order(d3$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(x = s)) +                   
  geom_point(aes(y=mc, fill = 'Metacognition'),color="grey") +  
  geom_point(aes(y=pc, fill ="Performance"), color = "black") +  
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.position = 'none',
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

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

d.sin.normalizar.solo.FyM <- DF_list$d


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

plot_model(a, type = "pred", terms = c("aq.norm", "Im"),
           axis.labels = c('standardized AQ','Metacognition'),
           legend.title = '',
           title = '', 
           colors = "bw",
           axis.title = c('standardized AQ','AUROC2'),
           show.data = FALSE) + 
  theme_sjplot(base_size = 25)



#################################
### without gender separation ###
#################################

p <- plot_model(a, type = "pred", terms = "aq.norm",
                axis.labels = c('standardized AQ','Metacognition'),
                legend.title = '',
                title = '', 
                axis.title = c('standardized AQ','Metacognition'),
                show.data = FALSE)
p +theme_sjplot(base_size = 25)+
  ylim(.55, .70)


#######################################
### Bar plot regression coefficient ###
#######################################


coeff <- coefficients(a)
coeff <- coeff[2:6]
sd_coef <- unname(summary(a)$coefficients[2:6,2])
names.coef <- c("AQ","Gender","Age","AQ:Gender","AQ:Age")

dtf1 <- data.frame(Predictor = names.coef,
                   y = coeff,
                   sd= sd_coef)

row.names(dtf1) <- NULL

ggplot(dtf1, aes(Predictor, y)) +
  geom_bar(stat = "identity", aes(fill = Predictor), colour="black", width = 0.9) +
  scale_fill_brewer(palette = "Greys")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept=0) +
  xlab("") + ylab("") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        legend.title =element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "left",
        aspect.ratio = 2/1.5,#2/0.7,
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))


################
### Marginal ###
################

# Male

d.male <- d1[d1$Im =='1' ,]

ggplot(d.male, aes(x=aq, y=mc)) +
  geom_point() +
  xlab('AQ')+
  ylab('AUROC2')+
  ggtitle('Male participants')+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

# Female

d.female <- d1[d1$Im =='0' ,]

ggplot(d.female, aes(x=aq, y=mc)) +
  geom_point() +
  xlab('AQ')+
  ylab('AUROC2')+
  ggtitle('Female participants')+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))


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
df_total.solo.FyM <- DF_list$a
d.sin.normalizar.solo.FyM <- DF_list$d

Nsuj <- length(unique(df_total.solo.FyM$sujetos))
ExistingSubjects <- unique(df_total.solo.FyM$sujetos)

# create empty df to save the cumH2 and cumF2 for each subject
df.AUROC2 <- data.frame(cum_H2_0 = integer(),
                        cum_H2_1 = integer(),
                        cum_H2_2 = integer(),
                        cum_H2_3 = integer(),
                        cum_H2_4 = integer(),
                        cum_FA2_0 = integer(),
                        cum_FA2_1 = integer(),
                        cum_FA2_2 = integer(),
                        cum_FA2_3 = integer(),
                        cum_FA2_4 = integer())

source(root$find_file("Analysis/AuxiliaryFunctions/auroc2 (ELIMINAR).R"))

#### for each subject, with function
for (i in 1:Nsuj) {
  correct <- df_total.solo.FyM$discrimination_is_correct[df_total.solo.FyM$sujetos==ExistingSubjects[i]]
  conf <- df_total.solo.FyM$confidence_key[df_total.solo.FyM$sujetos==ExistingSubjects[i]]
  Nratings <- 4
  
  lista <- type2roc(correct,conf, 4)
  cum_H2 <- lista$cum_H2
  cum_FA2 <- lista$cum_FA2
  
  df.AUROC2[nrow(df.AUROC2) + 1,] = c(cum_H2,cum_FA2)
}

# create the df for plot
df.AUROC2$Nsuj <- d.sin.normalizar.solo.FyM$sujetos
df.AUROC2$Gender <- d.sin.normalizar.solo.FyM$Im
df.AUROC2$AQ <- d.sin.normalizar.solo.FyM$aq

# PLOT AUROC2 FOR MALE PARTICIPANTS
# split by AQ first and last quartiles

cum_H2_male_AQ_1 <- c(mean(df.AUROC2$cum_H2_0[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_H2_1[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_H2_2[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_H2_3[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_H2_4[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]))

cum_FA2_male_AQ_1 <- c(mean(df.AUROC2$cum_FA2_0[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_FA2_1[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_FA2_2[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_FA2_3[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                 mean(df.AUROC2$cum_FA2_4[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]))

cum_H2_male_AQ_4 <- c(mean(df.AUROC2$cum_H2_0[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_1[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_2[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_3[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_4[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]))

cum_FA2_male_AQ_4 <- c(mean(df.AUROC2$cum_FA2_0[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_1[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_2[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_3[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_4[df.AUROC2$Gender=='Masculino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]))

df <- data.frame(cum_H2_male_AQ_1 = cum_H2_male_AQ_1,
                 cum_H2_male_AQ_4 = cum_H2_male_AQ_4,
                 cum_FA2_male_AQ_1 = cum_FA2_male_AQ_1,
                 cum_FA2_male_AQ_4 = cum_FA2_male_AQ_1)

d <- data.frame(far = cum_FA2_male_AQ_1, hr = cum_H2_male_AQ_1, aq = "Low AQ")
d <- rbind(d, data.frame(far = cum_FA2_male_AQ_4, hr = cum_H2_male_AQ_4, aq = "High AQ"))

ggplot(data = d, mapping = aes(x = far, y = hr, color = aq)) + 
  geom_line(size = 2) +
  geom_point(size = 4) + 
  geom_abline(intercept = 0, slope = 1, color = "grey", size = 2) +
  coord_fixed()+
  labs(x = "False alarm rate", y = "Hit rate", color = "AQ level") +
  theme_bw(base_size = 18)+
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
# split by AQ first and last quartiles

cum_H2_female_AQ_1 <- c(mean(df.AUROC2$cum_H2_0[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                      mean(df.AUROC2$cum_H2_1[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                      mean(df.AUROC2$cum_H2_2[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                      mean(df.AUROC2$cum_H2_3[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                      mean(df.AUROC2$cum_H2_4[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]))

cum_FA2_female_AQ_1 <- c(mean(df.AUROC2$cum_FA2_0[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                       mean(df.AUROC2$cum_FA2_1[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                       mean(df.AUROC2$cum_FA2_2[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                       mean(df.AUROC2$cum_FA2_3[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]),
                       mean(df.AUROC2$cum_FA2_4[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ < quantile(df.AUROC2$AQ)[[2]]]))

cum_H2_female_AQ_4 <- c(mean(df.AUROC2$cum_H2_0[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_1[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_2[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_3[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                      mean(df.AUROC2$cum_H2_4[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]))

cum_FA2_female_AQ_4 <- c(mean(df.AUROC2$cum_FA2_0[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_1[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_2[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_3[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]),
                       mean(df.AUROC2$cum_FA2_4[df.AUROC2$Gender=='Femenino' & df.AUROC2$AQ >= quantile(df.AUROC2$AQ)[[4]]]))

df <- data.frame(cum_H2_female_AQ_1 = cum_H2_female_AQ_1,
                 cum_H2_female_AQ_4 = cum_H2_female_AQ_4,
                 cum_FA2_female_AQ_1 = cum_FA2_female_AQ_1,
                 cum_FA2_female_AQ_4 = cum_FA2_female_AQ_1)

d <- data.frame(far = cum_FA2_female_AQ_1, hr = cum_H2_female_AQ_1, aq = "Low AQ")
d <- rbind(d, data.frame(far = cum_FA2_female_AQ_4, hr = cum_H2_female_AQ_4, aq = "High AQ"))

ggplot(data = d, mapping = aes(x = far, y = hr, color = aq)) + 
  geom_line(size = 2) +
  geom_point(size = 4) + 
  geom_abline(intercept = 0, slope = 1, color = "grey", size = 2) +
  coord_fixed()+
  labs(x = "False alarm rate", y = "Hit rate", color = "AQ level") +
  theme_bw(base_size = 18)+
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


## box plot 

d <- d.sin.normalizar.solo.FyM
aq.q <- rep(NaN, nrow(d))
for (i in 1:nrow(d)) {
  if (d$aq[i] <= quantile(d$aq)[[2]]){
    aq.q[i] <- "<=23"
  } else if (d$aq[i]  >= quantile(d$aq)[[4]]){
    aq.q[i] <- ">=28"
  } else{
    aq.q[i] <-"<=23 & >=28"
      }
}

d$aq.q <- aq.q
ggplot(d.sin.normalizar.solo.FyM, aes(x=aq.q, y=mc, fill= Im)) + 
  geom_boxplot()+
  xlab('AQ quartils')+
  ylab("AUROC2")+
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



#####################
### RT Histograms ###
#####################

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

d <- df_total.solo.FyM

## Histograms of reaction times 
RT_disk_task = d$t_ensayo_discriminacion
RT_conf_task = d$t_ensayo_confianza
RT_disk_task_label <- rep('RT_disk_task',length(RT_disk_task))
RT_conf_task_label <- rep('RT_conf_task', length(RT_conf_task))

RT_task <- c(RT_disk_task,RT_conf_task)
RT_task_labels <- c(RT_disk_task_label,RT_conf_task_label)

# a1
# disc task
ggplot(d, aes(x=t_ensayo_discriminacion))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in discrimination task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 

# conf task
ggplot(d, aes(x=t_ensayo_confianza))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in confidence task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 




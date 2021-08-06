#################
### DataFrame ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/Graphics/DataFrames_ForGraphics.R"))

# get the df list
# experimento = 1,2,ambos
DF_list <- DataFrame_ForGraphics(experimento = "ambos", 
                                  filtroRT_Disc_Sup = 5000,
                                  filtroRT_Disc_Inf = 0,
                                  filtroRT_Conf_Sup = 5000,
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
# i df_total.sin.normalizar.solo.FyM.mc.filter

d.sin.normalizar <- DF_list$b
df_total <- DF_list$a
d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g
df_total.solo.FyM.mc.filter <- DF_list$i

###############
### library ###
###############
library(tidyverse)
library(ggridges)

################
### Graphics ###
################

####### histograms

#### AQ by sex

# a
d.sin.normalizar.solo.FyM.mc.filter[d.sin.normalizar.solo.FyM.mc.filter=="Masculino"] <- 'Male' 
d.sin.normalizar.solo.FyM.mc.filter[d.sin.normalizar.solo.FyM.mc.filter=="Femenino"] <- 'Female' 

# a1
ggplot(d.sin.normalizar.solo.FyM.mc.filter,aes(x=aq)) + 
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Female'),
           fill = "red", alpha = 0.2) +
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Male'),
           fill = "blue", alpha = 0.2) +
  facet_grid(rows = vars(Im))+
  xlab("AQ") +
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

# a2
ggplot(d.sin.normalizar.solo.FyM.mc.filter,aes(x=aq)) + 
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Female'),
           fill = "red", alpha = 0.2) +
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Male'),
           fill = "blue", alpha = 0.2) +
  facet_grid(cols = vars(Im))+
  xlab("AQ") +
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
        

## Histograms of reaction times 
RT_disk_task = df_total$t_ensayo_discriminacion
RT_conf_task = df_total$t_ensayo_confianza
RT_disk_task_label <- rep('RT_disk_task',length(RT_disk_task))
RT_conf_task_label <- rep('RT_conf_task', length(RT_conf_task))

RT_task <- c(RT_disk_task,RT_conf_task)
RT_task_labels <- c(RT_disk_task_label,RT_conf_task_label)

d <- data.frame(RT_task = RT_task,
                RT_task_labels = RT_task_labels)


# a1
# disc task
ggplot(df_total, aes(x=t_ensayo_discriminacion))+
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
ggplot(df_total, aes(x=t_ensayo_confianza))+
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

# b1
ggplot(d,aes(x=RT_task)) + 
  geom_histogram(data=subset(d,RT_task_labels == 'RT_disk_task'),
           fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(d,RT_task_labels == 'RT_conf_task'),
           fill = "blue", alpha = 0.2) +
  facet_grid(cols = vars(RT_task_labels))+
  xlab("RT_task") +
  ylab("Trials")+
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

# b2
ggplot(d,aes(x=RT_task)) + 
  geom_histogram(data=subset(d,RT_task_labels == 'RT_disk_task'),
                 fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(d,RT_task_labels == 'RT_conf_task'),
                 fill = "blue", alpha = 0.2) +
  facet_grid(rows = vars(RT_task_labels))+
  xlab("RT_task") +
  ylab("Trials")+
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


####### metacognition and performance plot

mc.sorted <-  d.sin.normalizar.mc.filter[order(d.sin.normalizar.mc.filter$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(x = s, y=mc , colour="AUROC2")) +  
  geom_point(aes(x = s, y=pc , colour="Performance"), shape = 17) +  
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  labs(x="Participants", y="", color = "") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        #legend.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

####### density plots

# Metacognition with F and M
ggplot(d.sin.normalizar.solo.FyM.mc.filter, aes(x = mc, y = aq.quartile, fill = Im, colour = Im, alpha=0.5)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.title = element_text(colour="blue", size=10, 
                                      face="bold"))+
  theme(legend.text = element_text(colour="blue", size=10, 
                                     face="bold"))

# metacognition with F

ggplot(solo.f, aes(x = mc, y = aq.quartile, fill = aq.quartile)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# metacognition with M

ggplot(d.sin.normalizar.solo.FyM, aes(x = mc, y = aq.quartile, fill = aq.quartile)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# aq by sex 

ggplot(d.sin.normalizar.solo.FyM.mc.filter, aes(x=aq, fill=Im)) +
  geom_density()


l <- d.sin.normalizar.solo.FyM.mc.filter
for (i in 1:nrow(l)) {
  if(l$Im[i] == 'Femenino'){l$Im[i]= 'Female'}
  if(l$Im[i] == 'Masculino'){l$Im[i]= 'Male'}
}

l$Gender <-l$Im 

# Use semi-transparent fill
p<-ggplot(l, aes(x=aq, fill=Gender))+ xlab("AQ") +
  geom_density(alpha=0.4) +
  scale_y_continuous(expand = expansion(mult = c(0,0)))
p
p+  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=30),
        axis.title.x = element_text(size = 30))+
  scale_fill_grey()

# distribucion de confianza para trials correctas e incorrectas 

p<-ggplot(df_total, aes(x=AQ, fill=genero))+ xlab("AQ") +
  geom_density(alpha=0.4)
p
p+  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title.y = element_text(size = 31),
        legend.title = element_text(size=31),
        legend.text = element_text(size=26),
        axis.title.x = element_text(size = 31))+
  scale_fill_grey()

# reaction times with density plots

ggplot(df_total, aes(x=t_ensayo_discriminacion)) +
  geom_density(color="darkred", fill="red")+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  ylab("")+
  xlab("RT in discrimination task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 30)) 

ggplot(df_total, aes(x=t_ensayo_confianza)) +
  geom_density(color="darkred", fill="red")+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  ylab("")+
  xlab("RT in confidence task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 30)) 


##### probando un grafico de tr por sujeto y cantidad de trials menores a 100ms

Cant_Trial_Disc <- rep(NA,length(unique(df_total$sujetos)))
Cant_Trial_Conf <- rep(NA,length(unique(df_total$sujetos)))
ExistingSubjects <- unique(df_total$sujetos)

for (s in 1:length(ExistingSubjects)){
  subj <- df_total[df_total$sujetos== ExistingSubjects[s],] # getting data by subject
  Cant_Trial_Disc[s] <- nrow(subj[subj$t_ensayo_discriminacion < 100,])
  Cant_Trial_Conf[s] <- nrow(subj[subj$t_ensayo_confianza < 100,])
}

d1 <- data.frame(Cant_trial_conf = Cant_Trial_Conf,
                 Cant_trial_disc = Cant_Trial_Disc)
d.sin.normalizar <- cbind(d.sin.normalizar, d1)

ggplot(d1, aes(x=Cant_Trial_Conf))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("Cantidad de participantes")+
  xlab("Cantidad de trials menores a 100 ms (confidence task)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) 



Cant_trial_conf.sorted <-  d.sin.normalizar[order(d.sin.normalizar$Cant_trial_conf),]
subjects <- 1:nrow(Cant_trial_conf.sorted)
Cant_trial_conf.sorted$s <- subjects

ggplot(Cant_trial_conf.sorted, aes(Cant_trial_conf)) +                   
  geom_point(aes(x = Cant_trial_conf, y=mc)) +  
  #scale_y_continuous(expand = expansion(mult = c(0, .1)))+
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
  

library(arm)

a=lm(mc ~ Cant_trial_disc, data = d.sin.normalizar)
summary(a)
display(a)

par(mar = c(5, 5, 5, 5))
plot(d.sin.normalizar$Cant_trial_disc,
     d.sin.normalizar$mc, 
     pch = 16, cex = 1, col = "black",
     xlab = "Cant fast Trials (<100ms) Disc Task", ylab = "AUROC2", 
     cex.axis = 1.7, cex.lab = 1.8)
abline(lm(d.sin.normalizar$mc ~ d.sin.normalizar$Cant_trial_disc),
       col="grey", lwd=3)


####### plotting the performance by trial
library(matrixStats)

d1 <- df_total.sin.normalizar.solo.FyM.mc.filter 
d1$discrimination_is_correct[d1$discrimination_is_correct=='TRUE'] <- "1"
d1$discrimination_is_correct[d1$discrimination_is_correct=='FALSE'] <- "0"

total_trials <- max(d1$trials)-min(d1$trials)

MeanPerformanceByTrial <- rep(NA,total_trials)
#sd <- rep(NA,total_trials)
  
for (i in 1:total_trials){
  trial_colum <- d1[d1$trials == i,]  # getting data by trial
  MeanPerformanceByTrial[i] <- mean(as.integer(trial_colum$discrimination_is_correct))
  #sd[i] <- sd(as.integer(trial_colum$discrimination_is_correct))
}

plot(MeanPerformanceByTrial, type='l', col="green", lwd=5, 
     xlab="Trial", ylab="Mean Peroformance", xlim=c(0,135), ylim=c(0.50,1))

df2 <- data.frame(TrialNumber = 1:length(MeanPerformanceByTrial),
                  MeanPerformanceByTrial = MeanPerformanceByTrial)

ggplot(data=df2, aes(x=TrialNumber, y=MeanPerformanceByTrial)) +
  geom_line( color="blue", size=1.2)+
  geom_point(color="red", size=3) +
  #geom_errorbar(aes(ymin=MeanPerformanceByTrial-sd,
  #                  ymax=MeanPerformanceByTrial+sd), 
  #              width=.2,
  #              position=position_dodge(0.05))
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


####### plotting the metacognition by trial (for different discarding rt)

d1 <- df_total.solo.FyM.mc.filter 

Nsuj <- length(unique(d1$sujetos))

# saving metacog = mc for each RT discarded
mc_Rt_Discarded_0 <- rep(NA, Nsuj)
mc_Rt_Discarded_50 <- rep(NA, Nsuj)
mc_Rt_Discarded_100 <- rep(NA, Nsuj)
mc_Rt_Discarded_150 <- rep(NA, Nsuj)
mc_Rt_Discarded_200 <- rep(NA, Nsuj)
mc_Rt_Discarded_250 <- rep(NA, Nsuj)

# load the type 2 ROC analysis function
source(root$find_file("Analysis/AuxiliaryFunctions/auroc2.R"))

## get metacognitive sensivity
library(dplyr)

ExistingSubjects <- unique(d1$sujetos)

for (i in 1:Nsuj){
  mc_Rt_Discarded_0[i] <- type2roc(correct = d1$discrimination_is_correct[d1$sujetos==ExistingSubjects[i]], 
                      conf = d1$confidence_key[d1$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
  d2 <- d1[d1$t_ensayo_discriminacion >= 50 & d1$t_ensayo_confianza >=50,]
  mc_Rt_Discarded_50[i] <- type2roc(correct = d2$discrimination_is_correct[d2$sujetos==ExistingSubjects[i]], 
                                    conf = d2$confidence_key[d2$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
  d3 <- d1[d1$t_ensayo_discriminacion >= 100 & d1$t_ensayo_confianza >= 100,]
  mc_Rt_Discarded_100[i] <- type2roc(correct = d3$discrimination_is_correct[d3$sujetos==ExistingSubjects[i]], 
                                     conf = d3$confidence_key[d3$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
  d4 <- d1[d1$t_ensayo_discriminacion >= 150 & d1$t_ensayo_confianza >=150,]
  mc_Rt_Discarded_150[i] <- type2roc(correct = d4$discrimination_is_correct[d4$sujetos==ExistingSubjects[i]], 
                                     conf = d4$confidence_key[d4$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
  d5 <- d1[d1$t_ensayo_discriminacion >= 200 & d1$t_ensayo_confianza >=200,]
  mc_Rt_Discarded_200[i] <- type2roc(correct = d5$discrimination_is_correct[d5$sujetos==ExistingSubjects[i]], 
                                     conf = d5$confidence_key[d5$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
  d6 <- d1[d1$t_ensayo_discriminacion >= 250 & d1$t_ensayo_confianza >=250,]
  mc_Rt_Discarded_250[i] <- type2roc(correct = d6$discrimination_is_correct[d6$sujetos==ExistingSubjects[i]], 
                                     conf = d6$confidence_key[d6$sujetos==ExistingSubjects[i]], Nratings = 4 )
  
}



plot(mc_Rt_Discarded_200, type='l', col="green", lwd=5, 
     xlab="Trial", ylab="Mean Peroformance", xlim=c(0,135), ylim=c(0.50,1))

df2 <- data.frame(subjects = 1:Nsuj,
                  mc_Rt_Discarded_0 = mc_Rt_Discarded_0,
                  mc_Rt_Discarded_50 = mc_Rt_Discarded_50,
                  mc_Rt_Discarded_100 = mc_Rt_Discarded_100,
                  mc_Rt_Discarded_150 = mc_Rt_Discarded_150,
                  mc_Rt_Discarded_200 = mc_Rt_Discarded_200,
                  mc_Rt_Discarded_250 = mc_Rt_Discarded_250)
                  
ggplot(df2, aes(x=subjects)) + 
  geom_line(aes(y = mc_Rt_Discarded_0), color = "darkred",size=0.8) + 
  geom_line(aes(y = mc_Rt_Discarded_250), color="steelblue", linetype="twodash",size=0.8) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        #axis.text.y = element_text(size = 30),
        #axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

AUROC2 <- c(mc_Rt_Discarded_0,mc_Rt_Discarded_50,mc_Rt_Discarded_100,
            mc_Rt_Discarded_150,mc_Rt_Discarded_200,
            mc_Rt_Discarded_250)

a <- rep("0RT",Nsuj)
b <- rep("50RT",Nsuj)
c <- rep("100RT",Nsuj)
d <- rep("150RT",Nsuj)
e <- rep("200RT",Nsuj)
f <- rep("250RT",Nsuj)

AUROC2_Labels <- c(a,b,c,d,e,f)

df3 <- data.frame(AUROC2 = AUROC2,
                  AUROC2_Labels = AUROC2_Labels)

ggplot(df3, aes(x=AUROC2_Labels, y=AUROC2,fill=AUROC2_Labels)) +
  geom_boxplot()+
  labs(x = "Limite inferior de TR")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

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
library(dplyr)
library(lme4)
library(tibble)
library(car)
library(DescTools)
library(gridExtra)
library(faraway)
library(knitr)
library(performance)
library(ResourceSelection)
library(lattice)


########################################
### Correct and incorrect barplot ###  FIG 2 a
########################################

#### good metacognition barplot

## answers
cant_trials <- 130
percent_correct <- 75
n_correct <- round((75*cant_trials)/100)
n_incorrect <- cant_trials - n_correct
correct <- rep(1,n_correct)
incorrect <- rep(0,n_incorrect)
answers <- c(correct,incorrect)

## confidence

# percentage of conf answer for correct and incorrect
per_conf_cor_4 <- round((50*n_correct)/100)
per_conf_cor_3 <- round((30*n_correct)/100)
per_conf_cor_2 <- round((15*n_correct)/100)
per_conf_cor_1 <- round((5*n_correct)/100)
per_conf_incor_4 <- round((5*n_incorrect)/100)
per_conf_incor_3 <- round((15*n_incorrect)/100)
per_conf_incor_2 <- round((30*n_incorrect)/100)
per_conf_incor_1 <- round((50*n_incorrect)/100)
  
# confidence answers
conf_cor_4 <- rep(4,per_conf_cor_4)
conf_cor_3 <- rep(3,per_conf_cor_3)
conf_cor_2 <- rep(2,per_conf_cor_2)
conf_cor_1 <- rep(1,per_conf_cor_1)
conf_incor_4 <- rep(4,per_conf_incor_4)
conf_incor_3 <- rep(3,per_conf_incor_3)
conf_incor_2 <- rep(2,per_conf_incor_2)
conf_incor_1 <- rep(1,per_conf_incor_1)

# concatenate confidence answers vectors
conf_correct <- c(conf_cor_4,conf_cor_3,conf_cor_2,conf_cor_1)
conf_correct <-conf_correct[1:n_correct]
conf_incorrect <- c(conf_incor_4,conf_incor_3,conf_incor_2,conf_incor_1)
conf_incorrect <-conf_incorrect[1:n_incorrect]
confidence <- c(conf_correct,conf_incorrect)

# nraitings
Nratings<- 4

# calculate AUROC2
H2  <- rep(NA, Nratings)
FA2 <- rep(NA, Nratings)
i   <- Nratings+1
for (c in 1:Nratings){
  H2[i-1]  <- sum(confidence == c & answers) + 0.5
  FA2[i-1] <- sum(confidence == c & !answers) + 0.5
  i        <- i-1
}

H2      <- H2/sum(H2)
FA2     <- FA2/sum(FA2)
cum_H2  <- append(0, cumsum(H2))
cum_FA2 <- append(0, cumsum(FA2))

k <- rep(NA, Nratings)
i <- 1
for (c in 1:Nratings){
  k[i] <- (cum_H2[c+1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c+1])^2
  i    <- i+1
}
good_auroc2 <- 0.5 + 0.25*sum(k)

S2 <- c(rev(H2),rev(FA2)) # lo invierto, ya que fue no invertido para obviar la inversa de la normal
Names <- c("C1","C2","C3","C4", "C1","C2","C3","C4")
Group <- c(rep("H2", length(H2)), rep("FA2", length(FA2)))

df_good <- data.frame(S2 = S2,
                      Names = Names,
                      Group = Group
                      )
# we need these values later
good_cum_H2 <- cum_H2
good_cum_FA2 <- cum_FA2

# good metacognition bar plots
ggplot(df_good, aes(fill=Group, x=Names, y=S2)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits=c(0,0.6)) +
  scale_fill_manual(values = c("#000000","#808080"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())


#### poor metacognition histograms

## confidence

# percentage of conf answer for correct and incorrect
per_conf_cor_4 <- ceiling((25*n_correct)/100)
per_conf_cor_3 <- ceiling((25*n_correct)/100)
per_conf_cor_2 <- round((25*n_correct)/100)
per_conf_cor_1 <- round((25*n_correct)/100)
per_conf_incor_4 <- round((25*n_incorrect)/100)
per_conf_incor_3 <- round((25*n_incorrect)/100)
per_conf_incor_2 <- round((25*n_incorrect)/100)
per_conf_incor_1 <- round((25*n_incorrect)/100)

# confidence answers
conf_cor_4 <- rep(4,per_conf_cor_4)
conf_cor_3 <- rep(3,per_conf_cor_3)
conf_cor_2 <- rep(2,per_conf_cor_2)
conf_cor_1 <- rep(1,per_conf_cor_1)
conf_incor_4 <- rep(4,per_conf_incor_4)
conf_incor_3 <- rep(3,per_conf_incor_3)
conf_incor_2 <- rep(2,per_conf_incor_2)
conf_incor_1 <- rep(1,per_conf_incor_1)

# concatenate confidence answers vectors
conf_correct <- c(conf_cor_4,conf_cor_3,conf_cor_2,conf_cor_1)
conf_correct <-conf_correct[1:n_correct]
conf_incorrect <- c(conf_incor_4,conf_incor_3,conf_incor_2,conf_incor_1)
conf_incorrect <-conf_incorrect[1:n_incorrect]
confidence <- c(conf_correct,conf_incorrect)

# nraitings
Nratings<- 4

# calculate AUROC2
H2  <- rep(NA, Nratings)
FA2 <- rep(NA, Nratings)
i   <- Nratings+1
for (c in 1:Nratings){
  H2[i-1]  <- sum(confidence == c & answers) + 0.5
  FA2[i-1] <- sum(confidence == c & !answers) + 0.5
  i        <- i-1
}

H2      <- H2/sum(H2)
FA2     <- FA2/sum(FA2)
cum_H2  <- append(0, cumsum(H2))
cum_FA2 <- append(0, cumsum(FA2))

k <- rep(NA, Nratings)
i <- 1
for (c in 1:Nratings){
  k[i] <- (cum_H2[c+1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c+1])^2
  i    <- i+1
}
poor_auroc2 <- 0.5 + 0.25*sum(k)

S2 <- c(rev(H2),rev(FA2)) # lo invierto, ya que fue no invertido para obviar la inversa de la normal
Names <- c("C1","C2","C3","C4", "C1","C2","C3","C4")
Group <- c(rep("H2", length(H2)), rep("FA2", length(FA2)))

df_poor <- data.frame(S2 = S2,
                      Names = Names,
                      Group = Group
)

# we need these values later
poor_cum_H2 <- cum_H2
poor_cum_FA2 <- cum_FA2

# poor metacognition bar plots
ggplot(df_poor, aes(fill=Group, x=Names, y=S2)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits=c(0,0.6)) +
  scale_fill_manual(values = c("#000000","#808080"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())

#########################
### AUROC2 good-poor  ### FIG 2 b
#########################

# good AUROC2
df <- data.frame(H2 = good_cum_H2, 
                 FA2 = good_cum_FA2)

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3, color = 'black') +  # la pinto de blanco y la edito en inskape
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

# poor
df <- data.frame(H2 = poor_cum_H2, 
                 FA2 = poor_cum_FA2)

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3, color = 'black') +  # la pinto de blanco y la edito en inskape
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
  scale_x_continuous(expand = expansion(mult = c(0, 0)),limits = c(0, 40)) +
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
  geom_bar(data=subset(d3, Im == 'Male'), fill = "black")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),limits = c(0, 40)) +
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

### linear regression model 

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

############ metacog y AQ

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
  xlab("") +#xlab("Regression coefficient") +
  scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), #element_text(size = 20, angle = (45)), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

## regression line and scatter plot
ggplot(d1, aes(x=aq.norm, y=mc)) + 
  geom_point()+
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[2]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

############ confidence y AQ

d1$m_c.norm <- (d1$m_c - mean(d1$m_c))/ sd(d1$m_c)
#d1$sd_c.norm <- (d1$sd_c - mean(d1$sd_c))/ sd(d1$sd_c)

# corro el modelo
a=lm(m_c.norm ~ aq.norm +
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
  xlab("") +#xlab("Regression coefficient") +
  #scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), #element_text(size = 20, angle = (45)), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

## regression line and scatter plot
ggplot(d1, aes(x=aq.norm, y=m_c.norm)) + 
  geom_point()+
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[2]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

############ metacog y AQ subscales

d1$aq_social.norm <- (d1$aq_social - mean(d1$aq_social))/ sd(d1$aq_social)
d1$aq_at_sw.norm <- (d1$aq_atention_switch - mean(d1$aq_atention_switch))/ sd(d1$aq_atention_switch)
d1$aq_at_de.norm <- (d1$aq_atencion_detail - mean(d1$aq_atencion_detail))/ sd(d1$aq_atencion_detail)
d1$aq_com.norm <- (d1$aq_communication - mean(d1$aq_communication))/ sd(d1$aq_communication)
d1$aq_im.norm <- (d1$aq_imagination - mean(d1$aq_imagination))/ sd(d1$aq_imagination)

# corro el modelo
a=lm(mc ~ aq_social.norm+
       aq_at_sw.norm+
       aq_at_de.norm+
       aq_com.norm+
       aq_im.norm+
       Im +
       edad.norm,
     data = d1) 
summary(a)

plot_summs(a, coefs = c('social skill' = 'aq_social.norm',
                        'attention switching'='aq_at_sw.norm',
                        'attention to detail' = 'aq_at_de.norm',
                        'communication'='aq_com.norm',
                        'imagination'='aq_im.norm',
                        'gender'='Im',
                        'age' = 'edad.norm') ,
           plot.distributions = FALSE, colors = "black")+
  ylab("") +
  xlab("") +#xlab("Regression coefficient") +
  #scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), #element_text(size = 20, angle = (45)),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))


### regression line and scatter plot

# for all subscales
df_regression <- data.frame(id = c(1:5),
                            b0 = rep(unname(coefficients(a)[1]),5),
                            b1 = unname(coefficients(a)[2:6])) 

ggplot(df_regression)  +
  geom_abline(aes(intercept = b0, slope = b1, color=factor(id)), size = 0.7) +
  xlim(-4, 4) +
  ylim(0.5, 0.7)+
  scale_colour_grey(start = 0, end = .9) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

## for each subscale

# social
ggplot(d1, aes(x=aq_social.norm, y=mc)) + 
  geom_point()+
  xlim(-3, 4) +
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[2]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

# attention sw
ggplot(d1, aes(x=aq_at_sw.norm, y=mc)) + 
  geom_point()+
  xlim(-3, 4) +
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[3]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

# attention det
ggplot(d1, aes(x=aq_at_de.norm, y=mc)) + 
  geom_point()+
  xlim(-3, 4) +
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[4]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

# communication
ggplot(d1, aes(x=aq_com.norm, y=mc)) + 
  geom_point()+
  xlim(-3, 4) +
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[5]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())

# im
ggplot(d1, aes(x=aq_im.norm , y=mc)) + 
  geom_point()+
  xlim(-3, 4) +
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[6]))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank())



##########################################
### Mixed Logistic Regression Analysis ###
##########################################

# levanto los datos

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

# only male and female genders
d <- df_total[df_total$genero == "Masculino" | df_total$genero == "Femenino",]

# modifico las variables que me interesan modificar
d$genero <- ifelse(d$genero == "Masculino",1,0)
d$discrimination_is_correct <- ifelse(d$discrimination_is_correct == TRUE, 1, 0)
d$sujetos <- factor(d$sujetos)
d$confidence_key.norm <- (d$confidence_key - 1) / 3 
d$AQ.norm <-  (d$AQ - mean(d$AQ)) / sd(d$AQ)
d$edad.norm <- (d$edad - mean(d$edad))/sd(d$edad)

a_log <- glmer(discrimination_is_correct ~ confidence_key.norm +
                 confidence_key.norm:AQ.norm +
                 confidence_key.norm:genero +
                 confidence_key.norm:edad.norm +
                 confidence_key.norm:AQ.norm:genero +
                 confidence_key.norm:AQ.norm:edad.norm +
                 (1|sujetos),
               data = d,
               family = binomial,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))

# muestro los resultados
print(summary(a_log))

## saco los nombres de los coeficientes
lala <- coef(summary(a_log))
coef_names <- rownames(lala)

plot_summs(a_log, coefs = c('Confidence.norm'=  'confidence_key.norm',
                        'Confidence.norm:AQ.norm' = 'confidence_key.norm:AQ.norm',
                        'Confidence.norm:Gender' = 'confidence_key.norm:genero',
                        'Confidence.norm:Age.norm' = 'confidence_key.norm:edad.norm',
                        'Confidence.norm:AQ.norm:Gender' = 'confidence_key.norm:AQ.norm:genero',
                        'Confidence.norm:AQ.norm:Age' = 'confidence_key.norm:AQ.norm:edad.norm'),
           plot.distributions = FALSE, colors = "black")+
  ylab("") +
  xlab("") +#xlab("Regression coefficient") +
  #scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), #element_text(size = 20, angle = (45)), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 25))

## showing the intercept varying by subject
intercepts <- ranef(a_log)
intercepts <- intercepts$sujetos[[1]]

library(ggdist)
library(tidyquant)

df <- data.frame(subjects = 1:360,
                 intercepts = intercepts)

ggplot(df, aes(x = intercepts))+
  geom_histogram(fill = "black")+
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())

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



                      ############################
                      ### DEBAJO DE LA ALFOMRA ###
                      ############################

# figuras que no queremos borrar pero no van por ahora en el paper

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

########################
### Normal densities ###  FIG 2 a
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
### AUROC2 close-distant curves ### FIG 2 b
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

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with inclusion filters already applied
filepath <- root$find_file("Data/All_exp_inclusion_criteria/df_total.Rda")
load(file= filepath)

d <- df_total

d$discrimination_is_correct[d$discrimination_is_correct=='TRUE'] <- "1"
d$discrimination_is_correct[d$discrimination_is_correct=='FALSE'] <- "0"

total_trials <- (max(d$trials)+1)-min(d$trials)

MeanPerformanceByTrial <- rep(NA,total_trials)
#sd <- rep(NA,total_trials)

for (i in 1:total_trials){
  trial_colum <- d[d$trials == i,]  # getting data by trial
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





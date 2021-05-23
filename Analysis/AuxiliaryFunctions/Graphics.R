##### Graphics and analysis

#################
### DataFrame ###
#################

# elijo que exp voy a utilizar, 1, 2 (replica), o ambos
root <- rprojroot::is_rstudio_project
basename(getwd())

## exp1:
#filepath <- (root$find_file("Data/Results_Exp1/df_total.Rda"))

## exp2:
# filepath <- (root$find_file("Data/Results_Exp2(replica)/df_total.Rda"))

## cargo el df elegido
# load(file= filepath)

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


# tomo las variables de interes
auc2 <- rep(NaN, length(unique(df_total$sujetos)))
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

for (i in 1:length(unique(df_total$sujetos))) { # ACA ESTA EL ERROR EN GENERO, solucion: https://swcarpentry.github.io/r-novice-inflammation/12-supp-factors/
  
  auc2[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"auc2"])
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
  

d.sin.normalizar = data.frame(mc  = auc2,
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
d.sin.normalizar.mc.filter <- d.sin.normalizar[d.sin.normalizar$mc >= 0.5,]

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


d.sin.normalizar.solo.FyM <- d.sin.normalizar[d.sin.normalizar$Im == 'Femenino' | d.sin.normalizar$Im == 'Masculino',]
d.sin.normalizar.solo.FyM.mc.filter <- d.sin.normalizar.solo.FyM[d.sin.normalizar.solo.FyM$mc >= 0.5,]
d.solo.FyM.mc.filter <- d.mc.filter[d.mc.filter$Im == 'Femenino' | d.mc.filter$Im == 'Masculino',]


###############
### library ###
###############
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(plyr)
library(arm)
library(jtools)
library(ggstance)
library(broom.mixed)
library(dotwhisker)
library(sjPlot)
library(sjmisc)
library(ggeffects)

################
### Graphics ###
################

####### geom_point
ggplot(data = d) + 
  geom_point(mapping = aes(x = aq, y =mc))+
  xlab("AQ")+
  ylab("Metacognition")+
  ggtitle("AQ and Metacognition")+
  theme(plot.title = element_text(hjust = 0.5)) 
  
# by sex
ggplot(data = d) + 
  geom_point(mapping = aes(x = mc, y = aq, color = Im))

# by study level
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = auc2, y = AQ, color = estudio))

### ggplot 3 

# con transparencias
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, alpha = estudio))

# con formitas
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, shape = estudio))

# facet study level
ggplot(data = d) + 
  geom_point(mapping = aes(x = mc, y = aq)) + 
  facet_wrap(~ Im, nrow = 2)

# facet by sex
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = AQ, y = media_tr_discri)) + 
  facet_wrap(~ genero, nrow = 2)


####### geom_smooth

ggplot(data = df_DatosUnicos_mod7) + 
  geom_smooth(mapping = aes(x = auc2, y = AQ))

####### histograms

ggplot(data = d.sin.normalizar) + 
  geom_bar(mapping = aes(x = aq))

# AQ by sex
# metacognition with F

ggplot(d.sin.normalizar,aes(x=aq)) + 
  geom_bar(data=subset(d.sin.normalizar,Im == 'Femenino'),fill = "red", alpha = 0.2) +
  geom_bar(data=subset(d.sin.normalizar,Im == 'Masculino'),fill = "blue", alpha = 0.2) +
  xlab("AQ") +
  ggtitle("AQ by Sex")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(solo.FyM, aes(x=aq, fill = Im)) + 
  geom_bar(alpha = 0.5) + 
  scale_fill_manual(name="Sex",values=c("red","blue"),labels=c("F","M"))+
  ggtitle("AQ by Sex")+
  theme(plot.title = element_text(hjust = 0.5)) 


## Histograms of reaction times 

# after filter by reaction time
# plot
ggplot(df_total, aes(x=t_ensayo_discriminacion))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in discrimination task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 


ggplot(df_total, aes(x=t_ensayo_confianza))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in confidence task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 


# con estadisticos para una discreta
ggplot(data = df_DatosUnicos_mod2) + 
  stat_summary(
    mapping = aes(x = genero, y = auc2),
    fun.min = min,
    fun.max = max,
    fun = median
  )


####### boxplot 

# study level

ggplot(data = solo.FyM, mapping = aes(x = Im, y = aq)) + 
  geom_boxplot()+
  
# sex

ggplot(data = d.sin.normalizar, mapping = aes(x = Im, y = m_c)) + 
  geom_boxplot()

# boxplot gruped
ggplot(solo.FyM, aes(x=aq.quartile, y=mc, fill=Im)) + 
  xlab("AQ: 1= high, 4= low") +
  ylab("Metacognition") +
  geom_boxplot()+
  ggtitle("AQ and Metacognition by Sex")+
  theme(plot.title = element_text(hjust = 0.5)) 


####### metacognition and performance plot

mc.sorted <-  d.sin.normalizar[order(mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(y=mc), colour="red") +  
  geom_point(aes(y=pc), colour="green") +  
  labs(title="Metacognition and performance", x="Subjects", y="Performance (green) - Metacognition (red) ", color = "Leyenda\n") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
         plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

# ademas le saco el fondo y algunas lineas

mc.sorted <-  d.sin.normalizar.mc.filter[order(d.sin.normalizar.mc.filter$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(y=mc), colour="red") +  
  geom_point(aes(y=pc), colour="green") +  
# labs(title="Metacognition and performance", x="Subjects", y="Performance (green) - Metacognition (red) ", color = "Leyenda\n") +
  labs(x="Subjects", y="Metacog(red) - Perf (green)", color = "Leyenda\n") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 
  
####### violin plots r

# to know the aq quantiles
cuantiles <- quantile(d.sin.normalizar$aq) ### revisar funcion, anda mal, no los cuartiles 
#no dejan por debajo lo que deberian

aq.quartile <- rep(NaN, nrow(d))

for (i in 1:length(aq.quartile)) {
  if (d.sin.normalizar$aq[i] <= 23){
    aq.quartile[i] <- 4
  } else if (d.sin.normalizar$aq[i] > 23 & d.sin.normalizar$aq[i] <= 25){
    aq.quartile[i] <- 3
  }else if (d.sin.normalizar$aq[i] > 25 & d.sin.normalizar$aq[i] <= 28){
    aq.quartile[i] <- 2
  }else if (d.sin.normalizar$aq[i] > 28 & d.sin.normalizar$aq[i] <= 37){
    aq.quartile[i] <- 1
  }
}

table(aq.quartile) # aca se ve el error

d.sin.normalizar$aq.quartile <- as.factor(aq.quartile)

# plot violin grouped

d.sin.normalizar %>%
  mutate(aq.quartile = fct_reorder(aq.quartile, m_c)) %>%
  mutate(aq.quartile = factor(aq.quartile, levels=c("1", "2", "3", "4"))) %>%
  ggplot(aes(fill=Im, y=m_c, x=aq.quartile)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T, name="") +
  theme_ipsum()  +
  xlab("AQ: 1= high, 4= low") +
  ylab("confidence") +
  ylim(0,5)


####### density plots

# Metacognition with F and M
ggplot(solo.FyM, aes(x = mc, y = aq.quartile, fill = Im, colour = Im, alpha=0.5)) +
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

ggplot(d.sin.normalizar.solo.FyM, aes(x=aq, fill=Im)) +
  geom_density()
# Use semi-transparent fill
p<-ggplot(d.sin.normalizar.solo.FyM, aes(x=aq, fill=Im)) +
  geom_density(alpha=0.4)
p
p+  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 


################
### Analysis ###
################

### lineas para hacer regresion 

a=lm(mc ~aq +  aq: Im , data = d.sin.normalizar.solo.FyM.mc.filter) # sin normalizar no da interaccion con sexo
summary(a)
display(a)

res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)

a=lm(sd_c ~ aq  , data = d)
summary(a)
display(a)

a=lm(tr_c ~ aq  , data = d)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + aq:Im   , data = d)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + mc + aq:mc   , data = d)
summary(a)
display(a)

a=lm(aq~ mc*es  , data = d)
summary(a)

a=lm(mc ~ aq+ aq:Im, data = d)
summary(a)

# solo f
a=lm(mc~ aq, data = solo.f)
summary(a)
display(a)

plot (solo.f$aq, solo.f$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

# solo m
a=lm(mc~ aq, data = d.sin.normalizar)
summary(a)
display(a)

plot (solo.m$aq, solo.m$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

plot (d.sin.normalizar$aq, d.sin.normalizar$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

# pruebo hacer algunas regresiones luego de sacar los que tienen metacog menor a 0.5

a=lm(mc ~  aq + aq:Im, data = d.solo.FyM.mc.filter) ## DA SIGNIFICATIVO AQ, Y LA INTERACCION
summary(a)
display(a)

res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)

a=lm(m_c ~ aq  , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq  + mc, data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + aq:Im   , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(tr_c ~ aq + Im + mc + aq:mc   , data = d.solo.FyM.mc.filter)
summary(a)
display(a)

a=lm(aq~ mc*es  , data = d.solo.FyM.mc.filter)
summary(a)

a=lm(mc ~ aq+ aq:Im, data = d.solo.FyM.mc.filter)
summary(a)


# ploteo los coeficientes con plot_summs

a.1=lm(mc ~ aq+aq:Im, data = d.solo.FyM.mc.filter)
display(a.1)
plot_summs(a.1, plot.distributions = TRUE)+
  ylab("confidence") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

a.2=lm(mc ~ aq+ aq:Im, data = d.sin.normalizar.solo.FyM.mc.filter)
display(a.2)
plot_summs(a.1, a.2, plot.distributions = TRUE)


# ploteo los coeficientes con dotwhisker
a.1=lm(mc ~ aq+ aq:Im, data = d.solo.FyM.mc.filter)
display(a.1)

dwplot((a.1),       
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
  relabel_predictors(c(aq = "AQ",                       
                       Im = "Sex:")) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

# ploteo con plot model

df <- d.solo.FyM.mc.filter
data(df)
theme_set(theme_sjplot())

# make categorical
df$Im <- to_factor(df$Im)

# fit model with interaction
fit <- lm(mc ~ aq + aq * Im, data = df)

plot_model(fit, type = "pred", terms = c("aq", "Im"))
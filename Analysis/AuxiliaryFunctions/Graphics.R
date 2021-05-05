##### Graphics and analysis


################
### Graphics ###
################

# jugando con graficos del libro https://r4ds.had.co.nz/data-visualisation.html 
# y los datos del exp de metacog

library(tidyverse)

### ggplot 1
ggplot(data = d) + 
  geom_point(mapping = aes(x = aq, y =mc))+
  xlab("AQ")+
  ylab("Metacognition")+
  ggtitle("AQ and Metacognition")+
  theme(plot.title = element_text(hjust = 0.5)) 
  

### ggplot 2 

# segun genero
ggplot(data = d) + 
  geom_point(mapping = aes(x = mc, y = aq, color = Im))

# segun estudio
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = auc2, y = AQ, color = estudio))

### ggplot 3 

# con transparencias
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, alpha = estudio))

# con formitas
ggplot(data = df_DatosUnicos_mod7) + 
  geom_point(mapping = aes(x = auc2, y = AQ, shape = estudio))

### ggplot 4

# facet estudios
ggplot(data = d) + 
  geom_point(mapping = aes(x = mc, y = aq)) + 
  facet_wrap(~ Im, nrow = 2)

# facet genero
ggplot(data = df_DatosUnicos_mod2) + 
  geom_point(mapping = aes(x = AQ, y = media_tr_discri)) + 
  facet_wrap(~ genero, nrow = 2)


### ggplot 5

# geom

# 1
ggplot(data = df_DatosUnicos_mod7) + 
  geom_smooth(mapping = aes(x = auc2, y = AQ))


### ggplot 6

## histogramas

ggplot(data = d.sin.normalizar) + 
  geom_bar(mapping = aes(x = aq))

# AQ by sex
# metacognition with F
solo.f <- d.sin.normalizar[d.sin.normalizar$Im == "Femenino",]
solo.m <- d.sin.normalizar[d.sin.normalizar$Im == "Masculino" |d.sin.normalizar$Im == "Masculino",]
solo.FyM <- d.sin.normalizar[d.sin.normalizar$Im == "Masculino" |d.sin.normalizar$Im == "Femenino",]


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


# con estadisticos para una discreta
ggplot(data = df_DatosUnicos_mod2) + 
  stat_summary(
    mapping = aes(x = genero, y = auc2),
    fun.min = min,
    fun.max = max,
    fun = median
  )


### ggplot 7

# boxplot estudio

ggplot(data = solo.FyM, mapping = aes(x = Im, y = aq)) + 
  geom_boxplot()+
  


# boxplot genero

ggplot(data = d.sin.normalizar, mapping = aes(x = Im, y = m_c)) + 
  geom_boxplot()

# boxplot gruped
ggplot(solo.FyM, aes(x=aq.quartile, y=mc, fill=Im)) + 
  xlab("AQ: 1= high, 4= low") +
  ylab("Metacognition") +
  geom_boxplot()+
  ggtitle("AQ and Metacognition by Sex")+
  theme(plot.title = element_text(hjust = 0.5)) 


## metacognition and performance plot

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



## violin plots r
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
library(ggplot2)
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)



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

## density plots

library(ggridges)
library(ggplot2)

# Metacognition with F and M
ggplot(solo.FyM, aes(x = mc, y = aq.quartile, fill = Im, colour = Im, alpha=0.5)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.title = element_text(colour="blue", size=10, 
                                      face="bold"))+
  theme(legend.text = element_text(colour="blue", size=10, 
                                     face="bold"))

# metacognition with F
solo.f <- d[d$Im == "Femenino",]

ggplot(solo.f, aes(x = mc, y = aq.quartile, fill = aq.quartile)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# metacognition with M
solo.m <- d[d$Im == "Masculino",]

ggplot(solo.m, aes(x = mc, y = aq.quartile, fill = aq.quartile)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


################
### analysis ###
################

### lineas para normalizar variables y hacer regresion 

library(arm)


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

for (i in 1:length(unique(df_total$sujetos))) {
  
  auc2[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"auc2"])
  PC[i] <- unique(df_total[df_total$sujetos == ExistingSubjects[i],"PC"])
  genero[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"genero"])
  AQ[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"AQ"])
  horasSueno[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"horasSueno"])
  edad[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"edad"])
  estudio[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"estudio"])
  media_tr_discri[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_discri"])
  media_tr_confi[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_tr_confi"])
  media_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"media_confidence"])
  sd_confidence[i]<- unique(df_total[df_total$sujetos == ExistingSubjects[i],"sd_confidence"])
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

d <- d.sin.normalizar 
d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
#d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
d$edad <- (d$edad - mean(d$edad)) / sd(d$edad)
d$mc <- (d$mc - mean(d$mc)) / sd(d$mc)
d$aq <- (d$aq - mean(d$aq)) / sd(d$aq)
d$tr_d <- (d$tr_d - mean(d$tr_d)) / sd(d$tr_d)
d$tr_c <- (d$tr_c - mean(d$tr_c)) / sd(d$tr_c)
d$m_c <- (d$m_c - mean(d$m_c)) / sd(d$m_c)
d$sd_c <- (d$sd_c - mean(d$sd_c)) / sd(d$sd_c)

a=lm(sd_c~ aq+as.factor(Im) + aq: as.factor(Im)  , data = d)
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

a=lm(mc~ hs, data = d)
summary(a)

##

filepath <- root$find_file("Data/d_sin_normalizar.Rda")
save(d,file = filepath)


## 
solo.f <- d[d$Im == "Femenino",]
solo.m <- d[d$Im == "Masculino",]

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



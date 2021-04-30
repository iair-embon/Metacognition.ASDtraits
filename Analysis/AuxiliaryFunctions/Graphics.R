##### Graphics and analysis


################
### Graphics ###
################

# jugando con graficos del libro https://r4ds.had.co.nz/data-visualisation.html 
# y los datos del exp de metacog

library(tidyverse)

### ggplot 1
ggplot(data = d) + 
  geom_point(mapping = aes(x = mc, y =tr_d))

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

# histogramas

ggplot(data = df_DatosUnicos_mod2) + 
  geom_bar(mapping = aes(x = AQ))

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

ggplot(data = df_DatosUnicos_mod2, mapping = aes(x = estudio, y = auc2)) + 
  geom_boxplot()

# boxplot genero

ggplot(data = df_DatosUnicos_mod2, mapping = aes(x = genero, y = sd_tr_confi)) + 
  geom_boxplot()

# plotear 

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
}


d = data.frame(mc  = auc2,
               Im = genero, 
               pc  = PC,
               aq = AQ,
               hs = horasSueno,
               edad = edad,
               es = estudio,
               tr_d = media_tr_discri,
               tr_c = media_tr_confi)

d$pc <- (d$pc - mean(d$pc)) / sd(d$pc)
#d$hs <- (d$hs - mean(d$hs)) / sd(d$hs)
d$edad <- (d$edad - mean(d$edad)) / sd(d$edad)
d$mc <- (d$mc - mean(d$mc)) / sd(d$mc)
d$aq <- (d$aq - mean(d$aq)) / sd(d$aq)
d$tr_d <- (d$tr_d - mean(d$tr_d)) / sd(d$tr_d)
d$tr_c <- (d$tr_c - mean(d$tr_c)) / sd(d$tr_c)

a=lm(mc~ aq+as.factor(Im) + aq: as.factor(Im)  , data = d)
summary(a)
display(a)

res <- resid(a)
plot(fitted(a), res)
abline(0,0)
hist(res)

a=lm(mc ~ edad  , data = d)
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
a=lm(mc~ aq, data = solo.m)
summary(a)
display(a)

plot (solo.m$aq, solo.m$mc, xlab="aq", ylab="mc")
curve (coef(a)[1] + coef(a)[2]*x, add=TRUE)

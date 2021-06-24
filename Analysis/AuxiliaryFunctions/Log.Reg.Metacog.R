#### EJERCICIO 4


df_total.solo.FyM <- df_total[df_total$genero == "Masculino" | df_total$genero == "Femenino",]


### lineas para normalizar variables y hacer regresion 

library(arm)

root <- rprojroot::is_rstudio_project
basename(getwd())

## df_total:
filepath <- (root$find_file("Data/Results_Unified/Temporary.df/d.sin.normalizar.solo.FyM.Rda"))
load(file= filepath)

filepath <- (root$find_file("Data/Results_Unified/Temporary.df/df_total.solo.FyM.Rda"))
load(file= filepath)

# hago una regresion logistica para saber que tanto se puede predecir si acerto o no
# a partir de la confianza en general (no sujeto por sujeto)

fit.4.0 <- glm (discrimination_is_correct ~ confidence_key, 
                family=binomial(link="logit"), data = df_total.solo.FyM)
display(fit.4.0)
summary(fit.4.0)

fit.4.1 <- glm (discrimination_is_correct ~ confidence_key + AQ + genero +
                  confidence_key:genero + confidence_key:AQ, family=binomial(link="logit"), 
                data = df_total.solo.FyM)
summary(fit.4.1)
display(fit.4.1)

fit.4.2 <- glm (discrimination_is_correct ~confidence_key + AQ + genero +
                  confidence_key:genero + confidence_key:AQ+ confidence_key:AQ:genero, 
                family=binomial(link="logit"),
                data = df_total.solo.FyM)

fit.4.2 <- glm (discrimination_is_correct ~confidence_key + AQ  +
                  confidence_key:AQ, 
                family=binomial(link="logit"),
                data = df_total.solo.FyM)


display(fit.4.2)
summary(fit.4.2)

fit.4.4 <- glm (discrimination_is_correct ~ confidence_key + AQ + genero + 
                   edad, family=binomial(link="logit"), data = df_total.solo.FyM)
display(fit.4.4)


# corro una regresion logistica por sujeto para obtener su coeficiente de confianza
# la idea es despues correlacionarla con su nivel de metacog
reg.coef.conf <- rep(NaN, length(unique(df_total.solo.FyM$sujetos)))
sujetos.existentes <- unique(df_total.solo.FyM$sujetos)

for (i in 1:length(unique(df_total.solo.FyM$sujetos))) {
  f <- sujetos.existentes[i] 
  df_total.subset <- df_total.solo.FyM[df_total.solo.FyM$sujetos==f,]
  fit.4 <- glm (discrimination_is_correct ~ confidence_key, 
                family=binomial(link="logit"), data = df_total.subset)
  b <- coef (fit.4)
  reg.coef.conf[i] <- b[2]
}

## agrego los coeficientes de la regresion al d.sin.normalizar.solo.FyM

d.sin.normalizar.solo.FyM$reg.coef.conf <- reg.coef.conf

d.sin.normalizar.solo.FyM.mc.filter <- d.sin.normalizar.solo.FyM[d.sin.normalizar.solo.FyM$mc >= 0.5,]

a = lm(reg.coef.conf ~  aq + aq:Im ,data = d.sin.normalizar.solo.FyM.mc.filter)
summary(a)

# preparo el density plot

l <- d.sin.normalizar.solo.FyM.mc.filter
for (i in 1:nrow(l)) {
  if(l$Im[i] == 'Femenino'){l$Im[i]= 'Female'}
  if(l$Im[i] == 'Masculino'){l$Im[i]= 'Male'}
}

l$Gender <-l$Im 

p<-ggplot(l, aes(x=reg.coef.conf, fill=Gender))+ xlab("Regression Coefficient") +
  geom_density(alpha=0.4) +
  xlim(-4, 4)+
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


# pruebo normalizar ambas medidas de metacognicion

d.metacog.normalizado <- data.frame(mc.curva.roc = d.sin.normalizar.solo.FyM$mc,
                                    mc.reg.log = d.sin.normalizar.solo.FyM$reg.coef.conf)

mean.mc <- mean(d.metacog.normalizado$mc.curva.roc) 
sd.mc <- sd(d.metacog.normalizado$mc.curva.roc)
d.metacog.normalizado$mc.curva.roc <- (d.metacog.normalizado$mc.curva.roc - mean.mc)/ sd.mc
# antes de normaliar la metacog sacada desde la regresion, saco al sujeto 42, que apreto siempre 4

library(tidyverse)

d.metacog.normalizado <- d.metacog.normalizado %>% drop_na()

mean.mc <- mean(d.metacog.normalizado$mc.reg.log)
sd.mc <- sd(d.metacog.normalizado$mc.reg.log)
d.metacog.normalizado$mc.reg.log <- (d.metacog.normalizado$mc.reg.log - mean.mc)/sd.mc

# vuelvo a plotear
plot(d.metacog.normalizado$mc.curva.roc, d.metacog.normalizado$mc.reg.log, ylim = c(-1,1), xlim = c(-2.5,3.5))

# hago una regresion entre metacog y reg.coef.conf PREGUNTAR DUDA, CAMBIO DE PREDICTOR
fit.4.2 = lm(d.metacog.normalizado$mc.curva.roc ~  d.metacog.normalizado$mc.reg.log)
display(fit.4.2)

plot (d.metacog.normalizado$mc.reg.log, d.metacog.normalizado$mc.curva.roc, xlab="mc.reg.log", ylab="mc.curva.roc")
curve (coef(fit.4.2)[1] + coef(fit.4.2)[2]*x, add=TRUE) 

# hay varios outliers en mc.reg.log, pruebo que pasa si los elimino

# METODO 1, por cuartiles

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
   H <- 1.5 * IQR(x, na.rm = na.rm)
   y <- x
   y[x < (qnt[1] - H)] <- NA
   y[x > (qnt[2] + H)] <- NA
   y
}
#
new.mc.reg.log <- remove_outliers (d.metacog.normalizado$mc.reg.log)
d.metacog.normalizado$new.mc.reg.log <- new.mc.reg.log

# saco los na

d.metacog.normalizado.NA.droped <- d.metacog.normalizado %>% drop_na()

# METODO 2

# remove outliers above or below 3 standard deviations from mean
x <- d.metacog.normalizado

remove_outliers_1 <- x[x$mc.reg.log > (mean(x$mc.reg.log) - 3*sd(x$mc.reg.log)) & 
                         x$mc.reg.log < (mean(x$mc.reg.log) + 3*sd(x$mc.reg.log)),]

# remove outliers above or below 2 standard deviations from mean
remove_outliers_2 <- x[x$mc.reg.log > (mean(x$mc.reg.log) - 2*sd(x$mc.reg.log)) & 
                         x$mc.reg.log < (mean(x$mc.reg.log) + 2*sd(x$mc.reg.log)),]


# pruebo el metodo 1

# corro una nueva regresion
fit.4.3 = lm(d.metacog.normalizado.NA.droped$mc.curva.roc ~  d.metacog.normalizado.NA.droped$new.mc.reg.log)
display(fit.4.3)

# ploteo
plot (d.metacog.normalizado.NA.droped$new.mc.reg.log, d.metacog.normalizado.NA.droped$mc.curva.roc, xlab="new.mc.reg.log", ylab="mc.curva.roc")
curve (coef(fit.4.3)[1] + coef(fit.4.3)[2]*x, add=TRUE) 


# corro una correlacion entre metacog area bajo la curva tipo 2 y la de regresion logistica
x <- d.metacog.normalizado$mc.reg.log
y <- d.metacog.normalizado$mc.curva.roc
cor.test(x, y, method=c("pearson"))

# hago lo mismo pero sacando outliers
x <- d.metacog.normalizado.NA.droped$mc.reg.log
y <- d.metacog.normalizado.NA.droped$mc.curva.roc
cor.test(x, y, method=c("pearson"))

# pruebo el metodo 2
x <- remove_outliers_1$mc.reg.log
y <- remove_outliers_1$mc.curva.roc
cor.test(x, y, method=c("pearson"))

plot(x,y)


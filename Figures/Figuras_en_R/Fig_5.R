##########################################
### Mixed Logistic Regression Analysis ### FIG 5
##########################################

###############
### library ###
###############

require(tidyverse)
require(lme4)
require(jtools)
require(broom.mixed)

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
coeff <- coef(summary(a_log))
coef_names <- rownames(coeff)

plot_summs(a_log, coefs = c('AQ' = 'confidence_key.norm:AQ.norm',
                            'Gender[m]' = 'confidence_key.norm:genero',
                            'Age' = 'confidence_key.norm:edad.norm',
                            'AQ:Gender[m]' = 'confidence_key.norm:AQ.norm:genero',
                            'AQ:Age' = 'confidence_key.norm:AQ.norm:edad.norm'),
           plot.distributions = FALSE, colors = "black")+
  ylab("") +
  xlab("Regression coefficient") +
  #scale_x_continuous(breaks=seq(-0.03,0.03,0.02))+
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

ggsave("Figures/Figuras_en_R/5.png", 
       width = 10, height = 6)

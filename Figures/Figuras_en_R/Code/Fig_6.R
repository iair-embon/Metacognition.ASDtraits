###########################
### Regression Analysis ### FIG 6
###########################

###############
### library ###
###############
require(tidyverse)
require(jtools)
require(broom.mixed)

### linear regression model 

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frames with filters already applied
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

d.sin.normalizar.solo.FyM <- DF_list$d
### lineas para hacer regresion 

d1 = d.sin.normalizar.solo.FyM

d1$aq.norm <- (d1$aq - mean(d1$aq))/ sd(d1$aq)

d1$mc.norm <- (d1$mc - mean(d1$mc))/ sd(d1$mc)

d1$edad.norm <- (d1$edad - mean(d1$edad))/ sd(d1$edad)

d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)


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

plot_summs(a, coefs = c('Social Skill' = 'aq_social.norm',
                        'Attention Switching'='aq_at_sw.norm',
                        'Attention to Detail' = 'aq_at_de.norm',
                        'Communication'='aq_com.norm',
                        'Imagination'='aq_im.norm',
                        'Gender'='Im',
                        'Age' = 'edad.norm') ,
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

ggsave("Figures/Figuras_en_R/Figures/6.png", 
       width = 10, height = 6)

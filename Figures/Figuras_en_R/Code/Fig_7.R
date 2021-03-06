###################################
### Regression model confidence ### FIG 7
###################################

###############
### library ###
###############
require(tidyverse)
require(jtools)
require(broom.mixed)

### load linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/ConfidenceMean_AQ_linear_model.RData")
load(file= filepath)

plot_summs(a, coefs = c('AQ' = 'aq.norm',
                        'Gender[m]'='Im',
                        'Age' = 'edad.norm',
                        'AQ:Gender[m]'='aq.norm:Im',
                        'AQ:Age'='aq.norm:edad.norm'),
           colors = "black")+
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
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25), 
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25))

ggsave("Figures/Figuras_en_R/Figures/7a.png", 
       width = 10, height = 6)

## regression line and scatter plot

# load the dataframe
filepath <- root$find_file("Data/All_exp_exclusion_criteria/df_total.Rda")
load(file= filepath)
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_Filtered_already_applied.R"))
DF_list <- DataFrame_Filtered_already_applied(df_total)

d.sin.normalizar.solo.FyM <- DF_list$d
d1 = d.sin.normalizar.solo.FyM
d1$aq.norm <- (d1$aq - mean(d1$aq))/ sd(d1$aq)
d1$mc.norm <- (d1$mc - mean(d1$mc))/ sd(d1$mc)
d1$edad.norm <- (d1$edad - mean(d1$edad))/ sd(d1$edad)
d1[d1 == "Masculino"] <- "1"
d1[d1 == "Femenino"] <- "0"
d1$Im <- as.integer(d1$Im)
d1$m_c.norm <- (d1$m_c - mean(d1$m_c))/ sd(d1$m_c)

# convert the normalized AQ scores to the original scores
intercept <- coefficients(a)[[1]]
slope <- coefficients(a)[[2]]

converted_intercept <- intercept-slope*mean(d1$aq)/sd(d1$aq)
converted_slope <- slope/sd(d1$aq)

# figure
ggplot(d1, aes(x=aq, y=m_c.norm)) + 
  geom_point()+
  geom_abline(intercept = converted_intercept, 
              slope = converted_slope)+
  ylab("Confidence mean") +
  xlab("AQ") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 30),
        axis.text.x=element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30))

ggsave("Figures/Figuras_en_R/Figures/7b.png", 
       width = 10, height = 6)
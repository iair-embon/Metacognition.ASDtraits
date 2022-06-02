########################
### Regression model ### FIG 4
########################

###############
### library ###
###############
require(tidyverse)
require(jtools)
require(broom.mixed)

### load linear regression model 
root <- rprojroot::is_rstudio_project
basename(getwd())               
filepath <- root$find_file("Data/Regression_Results/AUROC2_AQ_linear_model.RData")
load(file= filepath)

### FIG 4a

plot_summs(a, coefs = c('AQ' = 'aq.norm',
                        'Gender[m]'='Im',
                        'Age' = 'edad.norm',
                        'AQ:Gender[m]'='aq.norm:Im',
                        'AQ:Age'='aq.norm:edad.norm'),
           colors = "black")+
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
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30), 
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30))

ggsave("Figures/Figuras_en_R/Figures/4a.png", 
       width = 10, height = 6)

## regression line and scatter plot
ggplot(d1, aes(x=aq, y=mc)) + 
  geom_point()+
  geom_abline(intercept = unname(coefficients(a)[1]), 
              slope = unname(coefficients(a)[2]))+
  ylab("AUROC2") +
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

ggsave("Figures/Figuras_en_R/Figures/4b.png", 
       width = 10, height = 6)
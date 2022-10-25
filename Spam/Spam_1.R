#####
# PLOTEO AL PARTICIPANTE DE MEJOR Y PEOR METACOG CON REG LOG
#####

library(tidyverse)

root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# agarro el de mejor  y peor metacognicion
best_mc <- d %>%
  arrange(mc) %>%
  tail(1) %>% 
  pull(Participant)

worst_mc <- d %>%
  arrange(mc) %>%
  head(1) %>% 
  pull(Participant)

# ploteo el de mejor metacog
df_total_best_mc <- df_total %>%
  filter(Participant == best_mc)

p_best <- ggplot(df_total_best_mc, aes(confidence_key,
                             as.numeric(discrimination_is_correct))) +
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_y_continuous("p(correct)" ,
                     limits = c(0,1))+
  xlab("confidence") +
  expand_limits(y = 0)+ 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 15),
        axis.text.x=element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# ploteo el de peor metacog
df_total_worst_mc <- df_total %>%
  filter(Participant == worst_mc)

p_worst <- ggplot(df_total_worst_mc, aes(confidence_key,
                                       as.numeric(discrimination_is_correct))) +
  geom_point()+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  scale_y_continuous("p(correct)" ,
                     limits = c(0,1))+
  xlab("confidence") +
  expand_limits(y = 0)+ 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.title.x=element_text(size = 15),
        axis.text.x=element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15))

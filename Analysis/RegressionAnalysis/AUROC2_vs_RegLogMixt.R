# cargo las librerias
library(tidyverse)
require(lme4)

# selecciono la ruta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# cargo el df_total
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

# Obtengo el vector del numero de casos
Conf_num_cases = df_total %>% 
  group_by(Participant) %>% 
  summarise(num_cases = n()) %>% 
  pull(num_cases)

# cargo el modelo de regresion mixta 
filepath <- root$find_file("Data/Regression_Results/MixedLogisticRegressionAnalysis.RData")
load(file= filepath)

# Obtengo intercepts aleatorias del modelo mixto a_log
lmm_intercepts = ranef(a_log)[['Participant']][,1] + fixef(a_log)[['(Intercept)']]

# Cargo el df_total con los puntajes de metacognicion estimados por AUROC2
source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total)

# Grafico AUROC2 vs efectos aleatorios, incluyendo el tamano de muestra como size
tibble(lmm = lmm_intercepts, auroc2 =  d$mc, 
       num_cases = Conf_num_cases, id_participant = d$Participant,
       gender = d$gender) %>%
  arrange(auroc2) %>%
  pivot_longer(c(auroc2, lmm)) %>%
  mutate(rn = row_number()) %>% 
  ggplot(aes(x = rn/2, y = value, color = name)) +
  geom_point(aes(shape = gender), alpha = 0.5) +
  xlab("Participantes") +
  ylab("Intercept aleatoria + fija") +
  theme_bw()

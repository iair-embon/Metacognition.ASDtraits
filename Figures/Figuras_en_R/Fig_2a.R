######################################
### Correct and incorrect barplot  ###  FIG 2 a
######################################

###############
### library ###
###############
library(tidyverse)

#### poor metacognition barplot (Top)

## confidence

# percentage of conf answer for correct and incorrect
per_conf_cor_4 <- ceiling((25*n_correct)/100)
per_conf_cor_3 <- ceiling((25*n_correct)/100)
per_conf_cor_2 <- round((25*n_correct)/100)
per_conf_cor_1 <- round((25*n_correct)/100)
per_conf_incor_4 <- round((25*n_incorrect)/100)
per_conf_incor_3 <- round((25*n_incorrect)/100)
per_conf_incor_2 <- round((25*n_incorrect)/100)
per_conf_incor_1 <- round((25*n_incorrect)/100)

# confidence answers
conf_cor_4 <- rep(4,per_conf_cor_4)
conf_cor_3 <- rep(3,per_conf_cor_3)
conf_cor_2 <- rep(2,per_conf_cor_2)
conf_cor_1 <- rep(1,per_conf_cor_1)
conf_incor_4 <- rep(4,per_conf_incor_4)
conf_incor_3 <- rep(3,per_conf_incor_3)
conf_incor_2 <- rep(2,per_conf_incor_2)
conf_incor_1 <- rep(1,per_conf_incor_1)

# concatenate confidence answers vectors
conf_correct <- c(conf_cor_4,conf_cor_3,conf_cor_2,conf_cor_1)
conf_correct <-conf_correct[1:n_correct]
conf_incorrect <- c(conf_incor_4,conf_incor_3,conf_incor_2,conf_incor_1)
conf_incorrect <-conf_incorrect[1:n_incorrect]
confidence <- c(conf_correct,conf_incorrect)

# nraitings
Nratings<- 4

# calculate AUROC2
H2  <- rep(NA, Nratings)
FA2 <- rep(NA, Nratings)
i   <- Nratings+1
for (c in 1:Nratings){
  H2[i-1]  <- sum(confidence == c & answers) + 0.5
  FA2[i-1] <- sum(confidence == c & !answers) + 0.5
  i        <- i-1
}

H2      <- H2/sum(H2)
FA2     <- FA2/sum(FA2)
cum_H2  <- append(0, cumsum(H2))
cum_FA2 <- append(0, cumsum(FA2))

k <- rep(NA, Nratings)
i <- 1
for (c in 1:Nratings){
  k[i] <- (cum_H2[c+1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c+1])^2
  i    <- i+1
}
poor_auroc2 <- 0.5 + 0.25*sum(k)

S2 <- c(rev(H2),rev(FA2)) # lo invierto, ya que fue no invertido para obviar la inversa de la normal
Names <- c("C1","C2","C3","C4", "C1","C2","C3","C4")
Group <- c(rep("H2", length(H2)), rep("FA2", length(FA2)))

df_poor <- data.frame(S2 = S2,
                      Names = Names,
                      Group = Group
)

# we need these values later
poor_cum_H2 <- cum_H2
poor_cum_FA2 <- cum_FA2

# poor metacognition bar plots
ggplot(df_poor, aes(fill=Group, x=Names, y=S2)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits=c(0,0.6)) +
  scale_fill_manual(values = c("#000000","#808080"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 20),
        axis.title.x = element_blank())


ggsave("Figures/Figuras_en_R/2a_top.png", 
       width = 6, height = 4)


#### good metacognition barplot (Botton)

## answers
cant_trials <- 130
percent_correct <- 75
n_correct <- round((75*cant_trials)/100)
n_incorrect <- cant_trials - n_correct
correct <- rep(1,n_correct)
incorrect <- rep(0,n_incorrect)
answers <- c(correct,incorrect)

## confidence

# percentage of conf answer for correct and incorrect
per_conf_cor_4 <- round((50*n_correct)/100)
per_conf_cor_3 <- round((30*n_correct)/100)
per_conf_cor_2 <- round((15*n_correct)/100)
per_conf_cor_1 <- round((5*n_correct)/100)
per_conf_incor_4 <- round((5*n_incorrect)/100)
per_conf_incor_3 <- round((15*n_incorrect)/100)
per_conf_incor_2 <- round((30*n_incorrect)/100)
per_conf_incor_1 <- round((50*n_incorrect)/100)

# confidence answers
conf_cor_4 <- rep(4,per_conf_cor_4)
conf_cor_3 <- rep(3,per_conf_cor_3)
conf_cor_2 <- rep(2,per_conf_cor_2)
conf_cor_1 <- rep(1,per_conf_cor_1)
conf_incor_4 <- rep(4,per_conf_incor_4)
conf_incor_3 <- rep(3,per_conf_incor_3)
conf_incor_2 <- rep(2,per_conf_incor_2)
conf_incor_1 <- rep(1,per_conf_incor_1)

# concatenate confidence answers vectors
conf_correct <- c(conf_cor_4,conf_cor_3,conf_cor_2,conf_cor_1)
conf_correct <-conf_correct[1:n_correct]
conf_incorrect <- c(conf_incor_4,conf_incor_3,conf_incor_2,conf_incor_1)
conf_incorrect <-conf_incorrect[1:n_incorrect]
confidence <- c(conf_correct,conf_incorrect)

# nraitings
Nratings<- 4

# calculate AUROC2
H2  <- rep(NA, Nratings)
FA2 <- rep(NA, Nratings)
i   <- Nratings+1
for (c in 1:Nratings){
  H2[i-1]  <- sum(confidence == c & answers) + 0.5
  FA2[i-1] <- sum(confidence == c & !answers) + 0.5
  i        <- i-1
}

H2      <- H2/sum(H2)
FA2     <- FA2/sum(FA2)
cum_H2  <- append(0, cumsum(H2))
cum_FA2 <- append(0, cumsum(FA2))

k <- rep(NA, Nratings)
i <- 1
for (c in 1:Nratings){
  k[i] <- (cum_H2[c+1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c+1])^2
  i    <- i+1
}
good_auroc2 <- 0.5 + 0.25*sum(k)

S2 <- c(rev(H2),rev(FA2)) # lo invierto, ya que fue no invertido para obviar la inversa de la normal
Confidence <- c("C1","C2","C3","C4", "C1","C2","C3","C4")
Group <- c(rep("Correct", length(H2)), rep("Incorrect", length(FA2)))

df_good <- data.frame(S2 = S2,
                      Confidence = Confidence,
                      Group = Group)

# we need these values later
good_cum_H2 <- cum_H2
good_cum_FA2 <- cum_FA2

# good metacognition bar plots
ggplot(df_good, aes(fill=Group, x=Confidence, y=S2)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.2)),
                   breaks = c("C1","C4"),
                   labels = c("Low","High")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), 
                     limits=c(0,0.6)) +
  scale_fill_manual(values = c("#000000","#808080"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 20))


ggsave("Figures/Figuras_en_R/2a_botton.png", 
       width = 6, height = 4)


#########################
### AUROC2 poor-good  ### FIG 2 b
#########################

# poor metacognition AUROC2

df <- data.frame(H2 = poor_cum_H2, 
                 FA2 = poor_cum_FA2)

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3, color = 'black') +  # la pinto de blanco y la edito en inskape
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype="dashed", 
              size = 1.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 

# good metacognition AUROC2

df <- data.frame(H2 = good_cum_H2, 
                 FA2 = good_cum_FA2)

ggplot(df) +                   
  geom_line(aes(x = FA2, y=H2),size = 3, color = 'black') +  # la pinto de blanco y la edito en inskape
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype="dashed", 
              size = 1.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 

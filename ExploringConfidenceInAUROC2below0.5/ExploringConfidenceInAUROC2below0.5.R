library(ggplot2)

# sub auroc2 small than 0.5 
indices <- c(16  ,23 ,
             35  ,42  ,48 , 
             54 , 60  ,63 , 
             83 , 84  ,95 ,
             138, 146 ,147, 
             159, 165, 177, 
             194, 228, 251,
             261, 270, 281,
             305, 334, 343, 
             348, 351, 379)

# df_total sin ningun filtro
existing_subject <- unique(df_total$sujetos)

# nos quedamos con los indices de aquellos que tienen auroc2 menor a 0.5
id_subj_smallAuroc <- existing_subject[indices]

for (i in 1:length(id_subj_smallAuroc)) {
  # vamos viendo sujeto por sujeto 
  df <- df_total [df_total$sujetos == id_subj_smallAuroc[i],] # 2 es un buen sujeto

  # nos qedamos con los trials a plotear
  df_lessTrials<- df[df$trials < 11,]
  
  
  myplot<- ggplot(df_lessTrials,aes(x= confidence_key , fill = discrimination_is_correct)) + 
    geom_histogram(data=subset(df_lessTrials,
                               discrimination_is_correct == 'TRUE'),aes(fill=discrimination_is_correct), alpha = 0.2) +
    geom_histogram(data=subset(df_lessTrials,
                               discrimination_is_correct == 'FALSE'),aes(fill=discrimination_is_correct), alpha = 0.2)+
    scale_fill_manual(name="is correct", values=c("red","blue"),labels=c("FALSE","TRUE")) +
    labs(title= paste("Participant",as.character(id_subj_smallAuroc[i]), sep = " "),x="confidence", y="trials", color = "")+
  theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(1, 1,1, 1, "cm"),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 20),
          panel.background = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 30),
          axis.title.y = element_text(size = 30),
          axis.title.x = element_text(size = 30)) 
  
  # guardamos la figura:
  png(paste("Hist_ConfidenceKey_CorrectIncorrect_suj", as.character(id_subj_smallAuroc[i]) , ".png", sep = ""))
  print(myplot)
  dev.off()
}


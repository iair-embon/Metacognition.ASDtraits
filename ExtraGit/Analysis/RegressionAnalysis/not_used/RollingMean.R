#################
### DataFrame ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/Graphics/DataFrames_ForGraphics.R"))

# get the df list
# experimento = 1,2,ambos
# filtro = 0,100,200
DF_list <- DataFrame_ForGraphics(experimento = "ambos", filtro = 100)

# DF_list:
# a df_total
# b d.sin.normalizar
# c d.sin.normalizar.mc.filter
# d d
# e d.mc.filter
# f d.sin.normalizar.solo.FyM
# g d.sin.normalizar.solo.FyM.mc.filter
# h d.solo.FyM.mc.filter
# i df_total.sin.normalizar.solo.FyM.mc.filter

df_total.sin.normalizar.solo.FyM.mc.filter <- DF_list$i
d <- DF_list$d
d.sin.normalizar.solo.FyM.mc.filter <- DF_list$g

####################
### rolling mean ###
####################

## analisis idea con nico

type2roc <- function(correct, conf, Nratings){
  # correct - vector of 1 x ntrials, 0 for error, 1 for correct
  # conf - vector of 1 x ntrials of confidence ratings taking values 1:Nratings
  # Nratings - how many confidence levels available
  
  
  H2  <- rep(NA, Nratings)
  FA2 <- rep(NA, Nratings)
  i   <- Nratings+1
  for (c in 1:Nratings){
    H2[i-1]  <- sum(conf == c & correct) + 0.5
    FA2[i-1] <- sum(conf == c & !correct) + 0.5
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
  auroc2 <- 0.5 + 0.25*sum(k)
  
  return(auroc2) 
}

# rolling mean function
rolling_mean <- function(data,total_trials,nsuj,ExistingSubjects, trial.wind,
                         starting.trial){
  
  # data <- df 
  # total_trials <- max(data$trials)-min(data$trials)
  # nsuj <- length(unique(df_total$sujetos))
  # ExistingSubjects <- unique(df_total$sujetos)
  
  rmean_auroc2 <- matrix(data=NA, nrow = nsuj, ncol=total_trials-(starting.trial+trial.wind-1))
  
  for(i in 1:nsuj){
    subj <- data[data$sujetos== ExistingSubjects[i],] # getting data by subject
    indx1 <- starting.trial  # from  # error index starting
    indx2 <- starting.trial + trial.wind  # until
    rolling_auroc2 <- c()
    
    while(indx2 <= total_trials){
      subj_filtered <- subj[subj$trials >= indx1 & subj$trials <= indx2,] # selecting window of trials  error
      
      rolling_auroc2 <- c(rolling_auroc2, type2roc(correct=subj_filtered$discrimination_is_correct,
                                                   conf=subj_filtered$confidence_key, Nratings=4))
      
      indx1 <- indx1 + 1
      indx2 <- indx2 + 1
    }
    
    rmean_auroc2[i,] <- rolling_auroc2
    
  }
  
  
  rmean_TaskDifficulty <- matrix(data=NA, nrow = nsuj, ncol=total_trials-
                                   (starting.trial + trial.wind-1))
  
  for(i in 1:nsuj){
    subj <- data[data$sujetos== ExistingSubjects[i],] # getting data by subject
    indx1 <- starting.trial  # from
    indx2 <- starting.trial + trial.wind  # until
    rolling_TaskDifficulty <- c()
    
    while(indx2 <= total_trials){
      subj_filtered <- subj[subj$trials >= indx1 & subj$trials <= indx2,] # selecting window of trials  errorrr
      
      rolling_TaskDifficulty <- c(rolling_TaskDifficulty, mean(subj_filtered$diferencia_puntitos))
      
      indx1 <- indx1 + 1
      indx2 <- indx2 + 1
    }
    
    rmean_TaskDifficulty[i,] <- rolling_TaskDifficulty
    
  }
  
  return(list(rmean_auroc2,rmean_TaskDifficulty))
}

#----- discard initial trials 
data <- df_total[df_total$trials > 15,]
#data <- data[data$genero == 'Femenino',]

# quartile
a <- quantile(data$AQ) 

#----- First quartile
data.q1 <- data[data$AQ < a[[2]],]

#other parameters
total_trials <- max(data.q1$trials)-min(data.q1$trials)
nsuj <-  length(unique(data.q1$sujetos))
ExistingSubjects <- unique(data.q1$sujetos)
trial.wind <- 20
starting.trial <- 16

rolling_mean_quart_1 <- rolling_mean(data.q1,total_trials,nsuj,ExistingSubjects,
                                     trial.wind, starting.trial)

#----- fourth quartile
data.q2 <- data[data$AQ > a[[4]],]

#other parameters
total_trials <- max(data.q2$trials)-min(data.q2$trials)
nsuj <-  length( unique(data.q2$sujetos))
ExistingSubjects <- unique(data.q2$sujetos)
trial.wind <- 20
starting.trial <- 16

rolling_mean_quart_4 <- rolling_mean(data.q2,total_trials,nsuj,ExistingSubjects, 
                                     trial.wind,starting.trial)

#----- Plotting
library(matrixStats)

x <- c(1,11,21,31,41,51,61,71,81,91)
# for q1
error_bar_q1 <- colSds(rolling_mean_quart_1[[1]])/sqrt(nrow(rolling_mean_quart_1[[1]])) 
y.q1 <- colMeans(rolling_mean_quart_1[[1]])
y.errorSd.q1 <- c()
y.arrowPoints.q1 <- c()

# for q4
error_bar_q4 <- colSds(rolling_mean_quart_4[[1]])/sqrt(nrow(rolling_mean_quart_4[[1]])) 
y.q4 <- colMeans(rolling_mean_quart_4[[1]])
y.errorSd.q4 <- c()
y.arrowPoints.q4 <- c()


for (i in 1:length(x)) {
  y.arrowPoints.q1 <- c(y.arrowPoints.q1, y.q1[x[i]])
  y.errorSd.q1 <- c(y.errorSd.q1, error_bar_q1[x[i]])
  
  y.arrowPoints.q4 <- c(y.arrowPoints.q4, y.q4[x[i]])
  y.errorSd.q4 <- c(y.errorSd.q4, error_bar_q4[x[i]])
}


plot(colMeans(rolling_mean_quart_1[[1]]), type='l', col="green", lwd=5, 
     xlab="Trial", ylab="rolling mean AUROC2", xlim=c(0,100), ylim=c(0.55,0.70))
lines(colMeans(rolling_mean_quart_4[[1]]), col="red", lwd=2)
legend(65,0.645,c("AQ Quartile 1","AQ Quartile 4"), lwd=c(5,2), col=c("green","red"))
title(paste("Rolling Mean Auroc2",trial.wind ,"trials - AQ quertiles", sep = " "))
# Add error bars
arrows(x0=x, y0=y.arrowPoints.q1-y.errorSd.q1,
       x1=x, y1=y.arrowPoints.q1+y.errorSd.q1, 
       code=3, angle=90, length=0.1)
arrows(x0=x, y0=y.arrowPoints.q4-y.errorSd.q4,
       x1=x, y1=y.arrowPoints.q4+y.errorSd.q4, 
       code=3, angle=90, length=0.1)

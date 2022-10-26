library(dplyr)

# load dataframe
root <- rprojroot::is_rstudio_project
basename(getwd())
load("./Data/All_exp_exclusion_criteria/df_total.Rda")

# stimuli presented
df_total$left_right_stimuli <- ifelse(df_total$dots_num_left > df_total$dots_num_right,
                             "left","right")

# response key
left_right_response_key <- rep(NaN, nrow(df_total))
for (i in 1:nrow(df_total)) {

  if(df_total$dots_num_left[i] > df_total$dots_num_right[i] &
     df_total$discrimination_is_correct[i] == TRUE)
  {left_right_response_key[i] <- "left"} 
  
  if(df_total$dots_num_left[i] > df_total$dots_num_right[i] &
     df_total$discrimination_is_correct[i] == FALSE)
  {left_right_response_key[i] <- "right"}
  
  if(df_total$dots_num_left[i] < df_total$dots_num_right[i] &
     df_total$discrimination_is_correct[i] == TRUE)
  {left_right_response_key[i] <- "right"}
  
  if(df_total$dots_num_left[i] < df_total$dots_num_right[i] &
     df_total$discrimination_is_correct[i] == FALSE)
  {left_right_response_key[i] <- "left"}
}

df_total$left_right_response_key <-left_right_response_key

lala <- df_total %>%
  group_by(sujetos, left_right_stimuli, left_right_response_key, confidence_key) %>%
  summarise(confidence_n = n())

df_total_preprocessed <- data.frame(sujetos = numeric(),
                                    left_right_stimuli = character(),
                                    left_right_response_key = character(),
                                    confidence_key = numeric(),
                                    confidence_n = numeric())

conf_ratings <- c(1,2,3,4)

nombre_provisorio <- function(lala_per_subj, stimuli, response){
  
  direction_df <- lala_per_subj %>%
    filter(left_right_stimuli == stimuli & left_right_response_key == response)
  
  if(nrow(direction_df) < 4){
    index <- direction_df$confidence_key != conf_ratings
    

      df <- data.frame(sujetos = rep(lala$sujetos[i], length(conf_ratings[index])),
                       left_right_stimuli = rep(stimuli,length(conf_ratings[index])),
                       left_right_response_key = rep(response,length(conf_ratings[index])),
                       confidence_key = conf_ratings[index], ### ERROR, CUANDO HAY MAS DE UN INDICE QUE NO ESTA
                       confidence_n = rep(0,length(conf_ratings[index])))
        df_total_provisorio <- rbind(direction_df, df)
   
  }else{
    df_total_provisorio <- direction_df}

  return(df_total_provisorio)    
}

for (i in 1:nrow(lala)) {
  
  lala_per_subj <- lala %>%
    filter(sujetos == sujetos[i]) 
  
  if(nrow(lala_per_subj)<16){
    
    # left left 
    df_total_provisorio <- nombre_provisorio(lala_per_subj = lala_per_subj, 
                                             stimuli = "left",
                                             response = "left")
    
    df_total_preprocessed <- rbind(df_total_preprocessed, df_total_provisorio)
    
    # left right 
    df_total_provisorio <- nombre_provisorio(lala_per_subj = lala_per_subj, 
                                             stimuli = "left",
                                             response = "right")
    
    df_total_preprocessed <- rbind(df_total_preprocessed, df_total_provisorio)
    
    # right left 
    df_total_provisorio <- nombre_provisorio(lala_per_subj = lala_per_subj, 
                                             stimuli = "right",
                                             response = "left")
    
    df_total_preprocessed <- rbind(df_total_preprocessed, df_total_provisorio)
    
    # right right 
    df_total_provisorio <- nombre_provisorio(lala_per_subj = lala_per_subj, 
                                             stimuli = "right",
                                             response = "right")
    
    df_total_preprocessed <- rbind(df_total_preprocessed, df_total_provisorio)
    
  } else {
    df_total_preprocessed <- rbind(df_total_preprocessed, lala_per_subj)
  }
  
}



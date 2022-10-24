library(dplyr)

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
  summarise(confidence_n = n()) %>%
  filter(sujetos == 2)

lala_meta_d <- data.frame(Stim = c(),
                          resp = c(),
                          conf_key = c())


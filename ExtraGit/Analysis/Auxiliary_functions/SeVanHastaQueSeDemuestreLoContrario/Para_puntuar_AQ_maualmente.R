calif <- rep(NaN, length(c(agree,disagree)))
for (i in 1:length(calif)) {
  if (i %in% agree){
    calif[i] <- "agree"
  }else{
    calif[i] <- "disagree"
  }
}


df_borrar <- data.frame(AQ_resp= AQ[6]$value,
                        AQ_Nro_Preg= AQ[5]$question,
                        calif = calif)
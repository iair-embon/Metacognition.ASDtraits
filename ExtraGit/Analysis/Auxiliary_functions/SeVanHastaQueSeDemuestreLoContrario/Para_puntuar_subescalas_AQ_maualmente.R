
# aca van los que puntuan para esa subescala
agree <- c(20,21,41,42)
disagree <- c(3,8,14,24,40,50)

calif <- rep(NaN,50)
for (i in 1:length(calif)) {
  if (i %in% agree){
    calif[i] <- "agree"
  }
  if(i %in% disagree){
    calif[i] <- "disagree"
  } 
}

# aca hay que modificar los componentes para que vaya el sujeto de quien queres comprobar
# su puntaje
df_borrar <- data.frame(AQ_resp= AQ[4]$value,
                        AQ_Nro_Preg= AQ[3]$question,
                        calif = calif)
### Funcion para numerizar la columna de horasSuenos

numeriza_col_horasSuenos <- function (df_DatosUnicos_mod,df_DatosUnicos){
# agarra el df_DatosUnicos_mod, columna horasSueno y convierte 
# toda la columna en numericos. Si no puede le pregunta al usuario que es ese valor.
# el usuario escribe el valor correspondiente (Ej: 6 y media ==> 6.3). Eso lo guarda en un df 
# que va a revisar la proxima vez antes de preguntarte por ese valor. Si lo encuentra, no pregunta
# pero si no lo encuentra le pregunta el ausuario. 
  # cargo el df_string_y_numeric que que guarda los valores no numericos de la columna horasSueno
  # y el valor numerico con el que es reemplazado por el usuario. 
  

  ## filepath del df que guarda los valores que ya fueron modificados
  root <- rprojroot::is_rstudio_project
  basename(getwd())
  otro_filepath <- (root$find_file("Analysis/AuxiliaryFunctions/df_string_y_numeric_horasSueno.Rda"))
  # cargo el df
  load(file=otro_filepath)
  
  # creo un df que al final va a guardar el df_string_y_numeric_horasSueno.Rda 
  # dependiendo si cambio algo o no en este
  df_paraVerSiCambio <- df_string_y_numeric 
  
  
  for (i in 1:length(df_DatosUnicos_mod$horasSueno)){
    
    # si el valor es un NA
    condicion <-  is.na(df_DatosUnicos_mod$horasSueno[i])
    
    if(condicion){
      
      # que se fije en  df_string_y_numeric a ver si alguien no reemplazo ese valor por un imput.
      indice <- which(df_string_y_numeric$string.value == df_DatosUnicos$horasSueno[i])
      # si encuentra el valor en cuestion en df_string_y_numeric, que lo reemplace y ya.
      if(length(indice)> 0){
        df_DatosUnicos_mod$horasSueno[i] <- df_string_y_numeric$numeric.value[indice]
      }
      # si no lo encuentra:
      else{
        # que pregunte a la persona para que escriba en numeros
        # el valor en cuestion, y espere el imput de la persona
        cat('Que es esto? ' , df_DatosUnicos$horasSueno[i])
        word <- readline(prompt="Ej: 6 y media ==> 6.3 ")
        
        # lo meto en el df
        df_DatosUnicos_mod$horasSueno[i] <- as.numeric(word) 
        
        # a ese imput, como a lo reemplazado que lo meta en un df
        # valores.string.numeric Data Frame    
        valores.string.numeric <- data.frame(df_DatosUnicos$horasSueno[i], as.numeric(word) )      
        
        #Naming the Data Frame 
        names(valores.string.numeric) <- c("string.value", "numeric.value")  
        
        #Using rbind() function to insert above observation  
        df_string_y_numeric <- rbind(df_string_y_numeric, valores.string.numeric)
      }
    } 
  }
  
  # si cambio algo en df_string_y_numeric que lo guarde o sino no
  # por ejemplo si se equivoco que no lo guarde y que lo corra devuelta
  if (nrow(df_paraVerSiCambio) < nrow(df_string_y_numeric)){
    # guardo el df_string_y_numeric
    save(df_string_y_numeric,file = otro_filepath)
  }
  
  
  # Le pido que me devuelva df_DatosUnicos_mod con la columna horasSuenos totalmente numerizada
  return(df_DatosUnicos_mod)
}

### Funcion para unificar la columna de TeEscuchamos  

unifica_col_TeEscuchamos <- function (df_DatosUnicos_mod,df_DatosUnicos){
  # agarra el df_DatosUnicos_mod, columna TeEscuchamos y convierte 
  # toda la columna en datos unificiados legibles del tipo ok, descartar, noSabe
  # Si no puede le pregunta al usuario que es ese valor.
  # el usuario escribe el valor correspondiente. Eso lo guarda en un df 
  # que va a revisar la proxima vez antes de preguntarte por ese valor. 
  # Si lo encuentra, no pregunta pero si no lo encuentra le pregunta al usuario. 
  
# # para crear el df_string_y_stringMod. Luego de crearlo la primera vez, comentarlo.
# df_string_y_stringMod <- data.frame(string=character(),
#                 stringMod=character(),
#                   stringsAsFactors=FALSE)
  

  ## filepath del df que guarda los valores que ya fueron modificados
  root <- rprojroot::is_rstudio_project
  basename(getwd())
  otro_filepath <- (root$find_file("Analysis/AuxiliaryFunctions/df_string_y_stringMod_TeEscuchamos.Rda"))
  # cargo el df
  load(file=otro_filepath)
  
  # creo un df que al final va a guardar el df dependiendo si cambio algo o no en este
  df_paraVerSiCambio <- df_string_y_stringMod
  
  
  ok <- c(NaN,'No','NO','ninguna','Ninguna','-','nada','Nada', 'ok', 'Ok') 
  
  
  for (i in 1:length(df_DatosUnicos_mod$TeEscuchamos)){ 
    
    # si el valor esta en ok
    condicion <-  df_DatosUnicos$affeccionPsico[i] %in% ok
    
    if(condicion){
      df_DatosUnicos_mod$TeEscuchamos[i] <- 'ok'} 
    else{ # si el valor no esta en ok
      
      # que se fije en  df_string_y_stringMod a ver si alguien no reemplazo ese valor por 
      # un imput.
      indice <- which(df_string_y_stringMod$string.value == df_DatosUnicos$TeEscuchamos[i])
      
      # si encuentra el valor en cuestion en df_string_y_stringMod, que lo reemplace y ya.
      if(length(indice)> 0){
        df_DatosUnicos_mod$TeEscuchamos[i] <- df_string_y_stringMod$stringMod.value[indice]
      }
      # si no lo encuentra:
      else{
        # que pregunte a la persona para que escriba como ok, descartar o noSabe
        # el valor en cuestion, y espere el imput de la persona
        cat('A partir de lo siguiente, puedo quedarme con el sujeto? ', df_DatosUnicos$TeEscuchamos[i])
        word <- readline(prompt="Escribir como ok, descartar o noSabe, Ej: No preste nada de atencion ==> descartar ")
        
        # lo meto en el df
        df_DatosUnicos_mod$TeEscuchamos[i] <- word 
        
        # a ese imput, como a lo reemplazado que lo meta en un df
        # valores.string.stringMod Data Frame    
        valores.string.stringMod <- data.frame(df_DatosUnicos$TeEscuchamos[i], word )      
        
        #Naming the Data Frame 
        names(valores.string.stringMod) <- c("string.value", "stringMod.value")  
        
        #Using rbind() function to insert above observation  
        df_string_y_stringMod <- rbind(df_string_y_stringMod, valores.string.stringMod)
      }
    }
  } 
  
  # si cambio algo en df_string_y_stringMod que lo guarde
  if (nrow(df_paraVerSiCambio) < nrow(df_string_y_stringMod)){
    # guardo el df_string_y_stringMod
    save(df_string_y_stringMod,file = otro_filepath)
  }
  
  # Le pido que me devuelva df_DatosUnicos_mod con la columna medicacion totalmente unificada
  return(df_DatosUnicos_mod)
}


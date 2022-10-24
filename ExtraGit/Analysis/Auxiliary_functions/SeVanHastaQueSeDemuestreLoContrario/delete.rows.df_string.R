# rows to delete
df_string_y_stringMod <- df_string_y_stringMod[-c(35,36,38,39), ]

# where you will save it
root <- rprojroot::is_rstudio_project
basename(getwd())
otro_filepath <- (root$find_file("Analysis/AuxiliaryFunctions/df_string_y_stringMod_medicacion.Rda"))

# save it
save(df_string_y_stringMod,file = otro_filepath)

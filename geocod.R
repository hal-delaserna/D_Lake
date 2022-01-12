geocod <-
  read.table(
    file = paste(sep = "",
                 "./D_Lake/",
                 "GeoCodigos_IBGE.csv"),
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = FALSE,
    encoding = "ANSI",
    quote = "\"",
    colClasses = c(
      'integer',	     # Cod_UF
      'character',	   # UF
      'character',	   # UF_sigla
      'integer',	     # Cod_Mesorregião.Geográfica
      'character',	   # Mesorregião
      'integer',	     # Cod_Microrregião
      'character',	   # Microrregião
      'integer',	     # GEOCOD
      'character',	   # Município
      'character'	     # Região_Administrativa_SP
    )
  )


geocod$GEOCOD_6 <-
  as.integer(gsub(geocod$GEOCOD, pattern = ".$", replacement = ""))
  
  
  
  
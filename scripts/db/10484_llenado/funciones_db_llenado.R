# 
# ini <- as.Date("2025-02-19")
# fin <- as.Date("2025-03-01")
# df <- historico_llenado %>% 
#   filter((Fecha >= ini) & ( Fecha <= fin)) %>% 
#   rename(dia = Fecha)

# df <- read_delim("archivos/10484_llenado/2025-02-19 a 2025-03-01 llenado (2).csv",
#                                        delim = "\t", escape_double = FALSE,
#                                        trim_ws = TRUE)


eliminar_ultimo_dia_llenado <- function(df) {
  # Encontrar la fecha más alta en la columna 'dia'
  
  # df$dia <- as.Date(df$dia)
  
  primer_dia <- min(df$dia, na.rm = TRUE)  
  ultimo_dia <- max(df$dia, na.rm = TRUE)
  
  diferencia_dias <- as.integer(ultimo_dia - primer_dia)
  
    # Filtrar el dataframe para eliminar todas las filas que coincidan con esa fecha
    df <- df %>% 
      filter(dia < ultimo_dia)
  
  return(df)
}

funcion_actualizar_llenado_10484 <- function(
    archivos_nuevos,
    ruta_carpeta_archivos){
  
  
  lista_data_frames_llenado <- map(archivos_nuevos, function(x) {
    
    # Si 'x' empieza con una letra de unidad seguida de ":" (o con "/" en sistemas Unix),
    # se asume que ya es una ruta absoluta.
    if (grepl("^(?:[A-Za-z]:|/)", x)) {
      full_path <- x
    } else {
      full_path <- file.path(ruta_carpeta_archivos, x)
    }
    
    # Leer el archivo usando la ruta completa
    tabla_actual_llenado <- read_delim(full_path,
                                       delim = "\t", 
                                       escape_double = FALSE,
                                       trim_ws = TRUE,
                                       locale = locale(encoding = "ISO-8859-1"))
    
    # Aplicar la función para eliminar el último día del reporte (porque carga información incompleta)
    tabla_actual_llenado <- eliminar_ultimo_dia_llenado(tabla_actual_llenado)

    
    # Retornar el dataframe modificado
    tabla_actual_llenado
  })
  
  llenado_nuevo <- bind_rows(!!!lista_data_frames_llenado)
  
  ## Borro los duplicados
  llenado_nuevo <- llenado_nuevo %>% 
    distinct()
  
  # Transformo la fecha a formato fecha
  llenado_nuevo$dia <- as.Date(llenado_nuevo$dia, format = "%A/%m/%d")
  llenado_nuevo$fecha_pasaje <- as.POSIXct(llenado_nuevo$fecha_pasaje, format = "%d-%m-%Y %H:%M:%S")
  
  # Transformo el GID a texto
  llenado_nuevo$gid <- as.character(llenado_nuevo$gid)
  
  
  ### Los que son las condiciones de el estado, las concateno
  llenado_nuevo <- llenado_nuevo %>%
    group_by(across(-desc_cond)) %>%  # Agrupa por todas las columnas excepto la que vas a concatenar
    summarize(
      desc_cond = paste(sort(na.omit(unique(desc_cond))), collapse = ";"),  # Ordenar alfabéticamente antes de concatenar
      .groups = 'drop'
    )
  
  llenado_nuevo <- llenado_nuevo %>% 
    arrange(desc(dia),nomenclatura_circuito,posicion,desc(fecha_pasaje))
  
  # modifico las coordenadas
  llenado_nuevo <- modificar_the_geom(llenado_nuevo)
  
  ## Filtro los días superiores, porque el informe es hasta el nocturno del dia anterior, y si no filtro toma el matutino
  # llenado_nuevo <- llenado_nuevo %>% 
  #   filter(dia <= fecha_informe_dia_anterior)
  
  llenado_nuevo <- llenado_nuevo %>%
    mutate(desc_turno = factor(desc_turno, levels = c("MATUTINO", "VESPERTINO", "NOCTURNO"))) %>%
    arrange(desc(dia),nomenclatura_circuito,posicion,desc(desc_turno))  # Ordenar el dataframe por `desc_turno`
  
  llenado_nuevo <- llenado_nuevo %>% 
    rename(
      Fecha = dia,
      Circuito = nomenclatura_circuito,
      Posicion = posicion,
      Direccion = ubicacion,
      Levantado = levantado,
      Turno_levantado = desc_turno,
      Fecha_hora_pasaje = fecha_pasaje,
      Incidencia = desc_motivo,
      Porcentaje_llenado = porcentaje_llenado,
      Numero_caja = caja,
      Id_pesada = pesada,
      Peso_Neto = peso_neto,
      Fecha_pesada = fecha_pesada,
      Id_motivo_inactiva = cod_motivo_inactiva,
      Id_viaje = id_viaje,
      Condicion = desc_cond
    )
  
  
  return(llenado_nuevo)
  
}


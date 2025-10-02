

# 
# library(readr)
# X2025_09_15 <- read_csv("archivos/gol_reportes/2025-09-15.csv", 
#                         col_types = cols(dia_viaje = col_date(format = "%Y-%m-%d"), 
#                                          fecha_pasaje = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
# 
# 
# historico_llenado_setiembre <- historico_llenado %>% 
#   filter(Fecha == "2025-09-15")
#   
#   set_gol <- X2025_09_15 %>% 
#     filter(!grepl("^B_0[1-7]$", circuito))




funcion_actualizar_llenadoGOL <- function(
    archivos_nuevos,
    ruta_carpeta_archivos){
  
 ### ----
  # Función que modifica como está escrito "the_geom".
  modificar_the_geom_solo_llenadoGOL <- function(df) {
    # Verificamos si la columna 'the_geom' existe en el dataframe
    if(!"the_geom" %in% colnames(df)) {
      stop("La columna 'the_geom' no está presente en el dataframe.")
    }
    
    # Modificar la columna 'the_geom' eliminando el espacio después de 'POINT(' y la coma entre las coordenadas
    df <- df %>%
      mutate(the_geom = gsub("POINT \\(\\s+", "POINT (", gsub(",\\s*", " ", the_geom)))
    
    return(df)
  }
  
  
  
  
  lista_data_frames_llenado <- map(archivos_nuevos, function(x) {

    # Si 'x' empieza con una letra de unidad seguida de ":" (o con "/" en sistemas Unix),
    # se asume que ya es una ruta absoluta.
    if (grepl("^(?:[A-Za-z]:|/)", x)) {
      full_path <- x
    } else {
      full_path <- file.path(ruta_carpeta_archivos, x)
    }
    
    llenado_nuevo <- read_delim(
      file = full_path,
      delim = ",",
      locale = locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      show_col_types = FALSE
    )


    # Retornar el dataframe modificado
    llenado_nuevo
  })

  llenado_nuevo <- bind_rows(!!!lista_data_frames_llenado)

  ## Borro los duplicados
  llenado_nuevo <- llenado_nuevo %>%
    distinct()

  # Transformo la fecha a formato fecha
  llenado_nuevo$dia_viaje <- as.Date(llenado_nuevo$dia_viaje, format = "%A/%m/%d")
  llenado_nuevo$fecha_pasaje <- as.POSIXct(llenado_nuevo$fecha_pasaje, format = "%d-%m-%Y %H:%M:%S")

  # Transformo el GID a texto
  llenado_nuevo$contenedor_gid <- as.character(llenado_nuevo$contenedor_gid)
  
  llenado_nuevo <- llenado_nuevo %>%
    mutate(
      condiciones_contenedor = condiciones_contenedor %>%
        str_replace_all("\\s*,\\s*", ";") %>%  # , con o sin espacios -> ;
        str_replace_all("\\s*;\\s*", ";") %>%  # limpia espacios alrededor de ;
        str_replace_all(";{2,}", ";") %>%      # colapsa ;; repetidos
        str_trim()                              # recorta extremos
    )

  llenado_nuevo <- llenado_nuevo %>%
    mutate(
      cod_recorrido = circuito %>%
        str_trim() %>%
        str_replace("^(CH|[A-G])_", "\\1_DU_RM_CL_")
    )
  
  llenado_nuevo <- llenado_nuevo %>%
    mutate(
      Municipio = circuito %>%
        str_trim() %>%
        str_to_upper() %>%
        str_extract("^(CH|[A-G])")
    )
  
  orden <- c("dia_viaje","cod_recorrido","posicion","ubicacion","levantado","turno","fecha_pasaje",
             "motivo_no_levante","porcentaje_llenado","numero_caja","contenedor_activo","id_viaje",
             "the_geom","contenedor_gid","condiciones_contenedor","Municipio","circuito","prioridad")
  
  llenado_nuevo <- dplyr::select(llenado_nuevo, any_of(orden))
  
  llenado_nuevo <- modificar_the_geom_solo_llenadoGOL(llenado_nuevo)
  

  llenado_nuevo <- llenado_nuevo %>%
    arrange(desc(dia_viaje),cod_recorrido,posicion,desc(fecha_pasaje))

  llenado_nuevo <- llenado_nuevo %>%
    mutate(turno = factor(turno, levels = c("Matutino", "Vespertino", "Nocturno"))) %>%
    arrange(desc(dia_viaje),cod_recorrido,posicion,desc(turno))  # Ordenar el dataframe por `desc_turno`

  llenado_nuevo <- llenado_nuevo %>%
    rename(
      Fecha = dia_viaje,
      Circuito = cod_recorrido,
      Posicion = posicion,
      Direccion = ubicacion,
      Levantado = levantado,
      Turno_levantado = turno,
      Fecha_hora_pasaje = fecha_pasaje,
      Incidencia = motivo_no_levante,
      Porcentaje_llenado = porcentaje_llenado,
      Numero_caja = numero_caja,
      contenedor_activo = contenedor_activo,
      Id_viaje_GOL = id_viaje,
      gid = contenedor_gid,
      Condicion = condiciones_contenedor,
      Circuito_corto = circuito
    ) %>% 
    select(-prioridad)
  
  llenado_nuevo <- dplyr::mutate(llenado_nuevo, DB = "GOL")
  

 # prueba_global_llenado <- bind_rows(historico_llenado, llenado_nuevo)

  return(llenado_nuevo)

}
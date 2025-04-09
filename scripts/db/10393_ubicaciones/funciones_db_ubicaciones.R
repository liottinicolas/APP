
funcion_actualizar_ubicaciones_10393 <- function(
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
    tabla_actual_ubicaciones <- read_delim(full_path,
                                       delim = "\t", 
                                       escape_double = FALSE,
                                       trim_ws = TRUE,
                                       locale = locale(encoding = "ISO-8859-1"))
    
    
    fecha_nombre_archivo <- file_path_sans_ext(basename(full_path))
    
    
    tabla_actual_ubicaciones <- tabla_actual_ubicaciones %>%
      mutate(Fecha = fecha_nombre_archivo)
    
    tabla_actual_ubicaciones$Fecha <- as.Date(tabla_actual_ubicaciones$Fecha)
    
    tabla_actual_ubicaciones <- tabla_actual_ubicaciones %>% 
    rename(
      gid = GID,
      Circuito = Recorrido    
      )
    
    tabla_actual_ubicaciones$gid <- as.character(tabla_actual_ubicaciones$gid)
    
    
    # Retornar el dataframe modificado
    tabla_actual_ubicaciones
  })
  
  ubicaciones_nuevo <- bind_rows(!!!lista_data_frames_llenado)
  
  ubicaciones_nuevo <- arreglar_direcciones(ubicaciones_nuevo)
  
  ## Borro los duplicados
  ubicaciones_nuevo <- ubicaciones_nuevo %>% 
    distinct() 
  
  ## Borro los que no tengan datos de calle, número ni observaciones
  ubicaciones_nuevo <- ubicaciones_nuevo %>%
    filter(!( (is.na(Calle) | Calle == "") &
                (is.na(Numero) | Numero == "") ))
  
  return(ubicaciones_nuevo)
  
}


# ruta_modificacion <- ruta_RDS_modificaciones_historicas
# ubicaciones <- historico_ubicaciones
# llenado <- historico_llenado
funcion_guardar_historico_modificaciones <- function(ruta_modificacion,ubicaciones,llenado){
  
  historicos_modificados <- if (file.exists(ruta_modificacion)) {
    readRDS(ruta_modificacion)
  } else {
    character(0)
  }
  
  if (length(historicos_modificados) > 0) {
    
    ## Busco el último valor que tiene
    ultimo_dia_con_modificacion <- max(historicos_modificados$Fecha)
    inicio_dia_con_modificacion <- ultimo_dia_con_modificacion + 1
    
    fecha_fin <- max(llenado$Fecha)
    
    if(ultimo_dia_con_modificacion < fecha_fin){
    
    ## si no hay modificaciones?
    fechas <- sort(unique(ubicaciones$Fecha[ubicaciones$Fecha > ultimo_dia_con_modificacion]))
    
    if(length(fechas)>0){
    
    # Lista para almacenar los cambios de cada comparación
    lista_cambios <- list()
    
    # Iterar desde el segundo día hasta el final
    for(i in seq.Date(inicio_dia_con_modificacion, fecha_fin, by = "day"))  {
      
      i_nuevo <- i-1
      
      dia_anterior <- as.Date(i_nuevo, origin = "1970-01-01")
      dia_actual <- as.Date(i, origin = "1970-01-01")
      
      
      # dia_anterior <- fechas[i - 1]
      # dia_actual   <- fechas[i]
      
      # Filtrar registros del día anterior y del día actual
      df_anterior <- ubicaciones %>% filter(Fecha == dia_anterior)
      df_actual   <- ubicaciones %>% filter(Fecha == dia_actual)
      
      # nuevos <- funcion_obtener_contenedores_agregados(dia_actual)
      
      ## Contenedores nuevos, mantenimiento y activos.
      contenedores_con_cambio_de_estado <- df_actual %>%
        anti_join(
          df_anterior,
          by = c("gid","Estado")  # Única columna para comparar
        )
      
      contenedores_nuevos_agregados <- contenedores_con_cambio_de_estado %>% 
        anti_join(
          df_anterior,
          by = c("gid")
        )
      
      contenedores_activos_inactivos <- contenedores_con_cambio_de_estado %>% 
        anti_join(
          contenedores_nuevos_agregados,
          by = c("gid")
        )
      
      
      ### Agrego el "Motivo"
      contenedores_activos_inactivos <- contenedores_activos_inactivos %>%
        mutate(Motivo = case_when(
          Estado == "Mantenimiento" ~ "Inactivo",
          is.na(Estado) ~ "Activo",
          TRUE ~ NA_character_
        ))
      
      contenedores_nuevos_agregados <- contenedores_nuevos_agregados %>%
        mutate(Motivo = "Agregado")
      
      
      #dia_actual_ubicaciones_sin_contenedores_nuevos_agregados <- anti_join(df_actual,nuevos ,by = "gid")
      
      contenedores_eliminados <- df_anterior %>% 
        anti_join(df_actual, by = "gid")
      
      contenedores_eliminados$Fecha <- as.Date(contenedores_eliminados$Fecha, format = "%Y-%m-%d") + 1
      
      contenedores_eliminados <- contenedores_eliminados %>%
        mutate(Motivo = "Eliminado")
      
      todos_los_cambios <- bind_rows(contenedores_activos_inactivos, contenedores_nuevos_agregados, contenedores_eliminados)
      
      
      
      
      
      # Almacenar los resultados si existen cambios
      if(nrow(todos_los_cambios) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- todos_los_cambios
      }
    }
    
    # Combinar todos los cambios en un único dataframe
    datos_nuevos <- bind_rows(lista_cambios)
    
    historicos_modificados <- bind_rows(historicos_modificados, datos_nuevos)
    
    }
    
    }
    
  } else {
    
    ## Si no existe
    
    # Obtener las fechas únicas ordenadas
    fechas <- sort(unique(ubicaciones$Fecha))
    
    # Lista para almacenar los cambios de cada comparación
    lista_cambios <- list()
    
    # Iterar desde el segundo día hasta el final
    for(i in 2:length(fechas)) {
      
      # i <- 2
      
      dia_anterior <- fechas[i - 1]
      dia_actual   <- fechas[i]
      
      # Filtrar registros del día anterior y del día actual
      df_anterior <- ubicaciones %>% filter(Fecha == dia_anterior)
      df_actual   <- ubicaciones %>% filter(Fecha == dia_actual)
      
      # nuevos <- funcion_obtener_contenedores_agregados(dia_actual)
      
      ## Contenedores nuevos, mantenimiento y activos.
      contenedores_con_cambio_de_estado <- df_actual %>%
        anti_join(
          df_anterior,
          by = c("gid","Estado")  # Única columna para comparar
        )
      
      contenedores_nuevos_agregados <- contenedores_con_cambio_de_estado %>% 
        anti_join(
          df_anterior,
          by = c("gid")
        )
      
      contenedores_activos_inactivos <- contenedores_con_cambio_de_estado %>% 
        anti_join(
          contenedores_nuevos_agregados,
          by = c("gid")
        )
      
      
      ### Agrego el "Motivo"
      contenedores_activos_inactivos <- contenedores_activos_inactivos %>%
        mutate(Motivo = case_when(
          Estado == "Mantenimiento" ~ "Inactivo",
          is.na(Estado) ~ "Activo",
          TRUE ~ NA_character_
        ))
      
      contenedores_nuevos_agregados <- contenedores_nuevos_agregados %>%
        mutate(Motivo = "Agregado")
      
      
      #dia_actual_ubicaciones_sin_contenedores_nuevos_agregados <- anti_join(df_actual,nuevos ,by = "gid")
      
      contenedores_eliminados <- df_anterior %>% 
        anti_join(df_actual, by = "gid")
      
      contenedores_eliminados$Fecha <- as.Date(contenedores_eliminados$Fecha, format = "%Y-%m-%d") + 1
      
      
      contenedores_eliminados <- contenedores_eliminados %>%
        mutate(Motivo = "Eliminado")
      
      todos_los_cambios <- bind_rows(contenedores_activos_inactivos, contenedores_nuevos_agregados, contenedores_eliminados)
    
      
      
      # Almacenar los resultados si existen cambios
      if(nrow(todos_los_cambios) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- todos_los_cambios
      }
    }
    
    # Combinar todos los cambios en un único dataframe
    historicos_modificados <- bind_rows(lista_cambios)
    
  }

  
  
  # Guardar el resultado
  saveRDS(historicos_modificados , file = ruta_modificacion)
  
  return(historicos_modificados)
  
}

funcion_eliminar_asteriscos <- function(df){
  
  df <- df %>%
    separate(Calle, 
             into = c("Calle", "Observaciones"), 
             sep = " *\\(\\*\\) *", 
             extra = "merge", 
             fill = "right")
  
  return(df)
  
}


arreglar_direcciones <- function(df){
  
  # ubicaciones_arregladas <- df %>%
  #   mutate(
  #     Calle = if_else(gid %in% c(180848, 143673) & Fecha == "2025-02-27", "URUGUAYANA", Calle),
  #     Observaciones = if_else(gid %in% c(180848, 143673) & Fecha == "2025-02-27",
  #                             "(*) RE-UBICAR OPTIMIZAR RUTA. EN 3520",
  #                             Observaciones)
  #   )
  
  ubicaciones_arregladas <- funcion_eliminar_asteriscos(df)
  
  ubicaciones_arregladas <- ubicaciones_arregladas %>%
    mutate(
      cond = is.na(Calle) & Observaciones == "MILAN 3600" & gid == "180015",
      Calle = if_else(cond, "MILAN", Calle),
      Numero = if_else(cond, 3600, Numero)
    ) %>%
    select(-cond)
  
  ubicaciones_arregladas <- ubicaciones_arregladas %>%
    mutate(
      Calle = if_else(gid %in% c(107430, 107487)  & Fecha == "2025-03-04", "RBLA REPUBLICA DE CHILE", Calle),
      Observaciones = if_else(gid %in% c(107430, 107487) & Fecha == "2025-03-04",
                              "EN 4605 VEREDA",
                              Observaciones)
      )
      
  ubicaciones_arregladas <- ubicaciones_arregladas %>%
    # Extraemos el número que sigue a "Calle" (o "CALLE") al inicio de la cadena.
    # La expresión (?i)^Calle\\s+(\\d+) es insensible a mayúsculas:
    #   - ^Calle: la cadena debe comenzar con "Calle" (o "CALLE", etc.)
    #   - \\s+: uno o más espacios
    #   - (\\d+): captura uno o más dígitos
    mutate(num_desconocido = str_match(Calle, regex("(?i)^Calle\\s+(\\d+)"))[,2]) %>%
    # Actualizamos solo cuando:
    # - Numero es 9583 (numérico)
    # - gid es 176724
    # - Se pudo extraer el número (num_desconocido no es NA)
    mutate(
      Calle = if_else(Numero == 9583 & gid == 176724 & !is.na(num_desconocido),
                      "CALLE 9583",
                      Calle),
      Numero = if_else(Numero == 9583 & gid == 176724 & !is.na(num_desconocido),
                       as.numeric(num_desconocido),
                       Numero)
    ) %>%
    select(-num_desconocido)
  
  
  ubicaciones_arregladas <- ubicaciones_arregladas %>% 
    mutate(Calle = if_else(gid == 180871 & Fecha == as.Date("2025-03-18"), 
                           "JUAN RODRIGUEZ CORREA", 
                           Calle)) %>% 
    mutate(Calle = if_else(gid == 117165 & Fecha == as.Date("2025-03-13"), 
                           "ARQ JUAN G GIURIA", 
                           Calle))


}





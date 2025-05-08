# nolint start: line_length_linter, object_name_linter

ruta_archivo <- file.path(ruta_proyecto, "scripts", "para_mapear", "circuitos_planificados.rds")


## Cargar los datos


# ===================================================================
# Función para crear los circuitos con sus correspondientes turnos planificados.
# 
#
# Entrada: Ninguno
# Salida: Dataframe separada por circuito, con el turno planificado.
# ===================================================================

turnos_planificados_por_circuito <- function(){
  municipios <- c("A", "B", "C", "CH", "D", "E", "F", "G")

  municipios_numeros <- list(
    A = c(101:114,116,118:119,201:206),
    B = 101:103,
    C = 101:116,
    CH = 101:123,
    D = 101:121,
    E = c(101:116,136),
    F = 101:115,
    G = c(101,103:116)
  )
  
  # Función para crear un dataframe para cada municipio
  crear_df_municipio <- function(municipio, numeros) {
    nombre_circuito <- paste0(municipio, "_DU_RM_CL_", numeros)
    data.frame(
      cod_recorrido = nombre_circuito,
      municipio = municipio,
      numero_circuito = numeros,
      turno_planificado = "M",
      stringsAsFactors = FALSE
    )
  }
  
  # Crear un dataframe para cada municipio y combinarlos
  df_list <- lapply(names(municipios_numeros), function(municipio) {
    crear_df_municipio(municipio, municipios_numeros[[municipio]])
  })
  
  # Combinar todos los dataframes en uno solo
  df_final <- do.call(rbind, df_list)
  
  df_final <- df_final %>%
    mutate(turno_planificado = case_when(
      cod_recorrido == "A_DU_RM_CL_106" ~ "N",
      cod_recorrido == "A_DU_RM_CL_119" ~ "V",
      municipio == "B" ~ "N",
      cod_recorrido == "C_DU_RM_CL_101" ~ "N",
      cod_recorrido == "C_DU_RM_CL_102" ~ "N",
      cod_recorrido == "C_DU_RM_CL_107" ~ "N",
      cod_recorrido == "C_DU_RM_CL_108" ~ "N",
      cod_recorrido == "C_DU_RM_CL_109" ~ "N",
      cod_recorrido == "C_DU_RM_CL_110" ~ "N",
      cod_recorrido == "C_DU_RM_CL_111" ~ "N",
      cod_recorrido == "C_DU_RM_CL_112" ~ "N",
      cod_recorrido == "C_DU_RM_CL_113" ~ "N",
      cod_recorrido == "C_DU_RM_CL_114" ~ "N",
      cod_recorrido == "C_DU_RM_CL_115" ~ "N",
      cod_recorrido == "C_DU_RM_CL_116" ~ "N",
      municipio == "CH" ~ "N",
      cod_recorrido == "D_DU_RM_CL_101" ~ "N",
      cod_recorrido == "D_DU_RM_CL_103" ~ "N",
      municipio == "E" ~ "N",
      cod_recorrido == "E_DU_RM_CL_136" ~ "M",
      cod_recorrido == "E_DU_RM_CL_102" ~ "V",
      cod_recorrido == "E_DU_RM_CL_103" ~ "V",
      cod_recorrido == "E_DU_RM_CL_106" ~ "V",
      cod_recorrido == "E_DU_RM_CL_107" ~ "V",
      cod_recorrido == "F_DU_RM_CL_101" ~ "V",
      cod_recorrido == "F_DU_RM_CL_102" ~ "V",
      cod_recorrido == "F_DU_RM_CL_103" ~ "V",
      cod_recorrido == "F_DU_RM_CL_104" ~ "V",
      municipio == "G" ~ "V",
      TRUE ~ as.character(turno_planificado)
    )
    )
  
  df_final <- df_final %>% 
    mutate(circuito_corto = ifelse(
      # Si tiene 3 digitos
      substring(cod_recorrido,nchar(cod_recorrido)-3,nchar(cod_recorrido)-3) == "_",
      #valor verdadero
      substring(cod_recorrido,nchar(cod_recorrido)-2,nchar(cod_recorrido)),
      substring(cod_recorrido,nchar(cod_recorrido)-1,nchar(cod_recorrido))
    )) %>%
    mutate(circuito_corto = paste0(municipio,"_",circuito_corto))
  
  df_final <- df_final %>% 
    select(cod_recorrido,circuito_corto,municipio,numero_circuito,turno_planificado)
  
  # Crear mapeo de frecuencias específicas para cada circuito
  frecuencias_por_circuito <- list(
    "A_103" = 3.50, "A_104" = 3.50, "A_105" = 3.50, "A_106" = 3.50,
    "A_107" = 3.00, "A_108" = 3.00, "A_109" = 3.00, "A_110" = 3.00,
    "A_111" = 3.00, "A_112" = 3.00, "A_113" = 3.00, "A_114" = 3.00,
    "A_116" = 3.00, "A_119" = 3.00, "A_201" = 3.00, "A_202" = 3.00,
    "A_203" = 3.00, "A_204" = 3.00, "A_205" = 3.00, "A_206" = 3.00,
    "B_101" = 3.00, "B_102" = 3.00, "B_103" = 7.00,
    "C_101" = 3.00, "C_102" = 3.00, "C_103" = 3.00, "C_104" = 3.00,
    "C_105" = 3.00, "C_106" = 3.00, "C_107" = 3.00, "C_108" = 3.00,
    "C_109" = 3.00, "C_110" = 3.00, "C_111" = 3.00, "C_112" = 3.00,
    "C_113" = 3.00, "C_114" = 3.00, "C_115" = 3.00, "C_116" = 3.00,
    "CH_101" = 7.00, "CH_102" = 7.00, "CH_103" = 7.00, "CH_104" = 7.00,
    "CH_105" = 7.00, "CH_106" = 7.00, "CH_107" = 7.00, "CH_108" = 7.00,
    "CH_109" = 7.00, "CH_110" = 3.50, "CH_111" = 3.50, "CH_112" = 3.50,
    "CH_113" = 3.50, "CH_114" = 3.50, "CH_115" = 3.50, "CH_116" = 3.50,
    "CH_117" = 3.50, "CH_118" = 3.50, "CH_119" = 3.50, "CH_120" = 3.50,
    "CH_121" = 3.50, "CH_122" = 7.00, "CH_123" = 7.00,
    "D_101" = 3.50, "D_102" = 3.50, "D_103" = 3.50, "D_104" = 3.50,
    "D_105" = 3.50, "D_106" = 3.50, "D_107" = 3.50, "D_108" = 3.50,
    "D_109" = 3.50, "D_110" = 3.50, "D_111" = 3.50, "D_112" = 3.50,
    "D_113" = 3.50, "D_114" = 3.00, "D_115" = 3.00, "D_116" = 3.00,
    "D_117" = 3.00, "D_118" = 3.00, "D_119" = 3.00, "D_120" = 3.00,
    "D_121" = 3.00,
    "E_101" = 3.00, "E_102" = 3.00, "E_103" = 3.00, "E_104" = 3.00,
    "E_105" = 3.00, "E_106" = 3.00, "E_107" = 3.00, "E_108" = 3.00,
    "E_109" = 2.33, "E_110" = 3.00, "E_111" = 3.00, "E_112" = 2.33,
    "E_113" = 2.33, "E_114" = 2.33, "E_115" = 2.33, "E_116" = 2.33,
    "E_136" = 7.00,
    "F_101" = 3.00, "F_102" = 3.00, "F_103" = 3.00, "F_104" = 3.00,
    "F_105" = 3.00, "F_106" = 3.00, "F_107" = 3.00, "F_108" = 3.00,
    "F_109" = 3.00, "F_110" = 3.00, "F_111" = 3.00, "F_112" = 3.00,
    "F_113" = 3.00, "F_114" = 3.00, "F_115" = 3.00,
    "G_101" = 3.00, "G_103" = 3.00, "G_104" = 3.00, "G_105" = 3.00,
    "G_106" = 3.00, "G_107" = 3.00, "G_108" = 3.00, "G_109" = 3.00,
    "G_110" = 3.00, "G_111" = 3.00, "G_112" = 3.00, "G_113" = 3.00,
    "G_114" = 3.00, "G_115" = 3.00, "G_116" = 3.00
  )
  
  # Asignar frecuencias específicas a cada circuito
  df_final$Frecuencia <- sapply(df_final$circuito_corto, function(circuito) {
    if (circuito %in% names(frecuencias_por_circuito)) {
      as.character(frecuencias_por_circuito[[circuito]])
    } else {
      "7" # Valor predeterminado si no está en la lista
    }
  })
  
  # Calcular la columna "Periodo" como 7/Frecuencia
  df_final$Periodo <- round(7 / as.numeric(df_final$Frecuencia), 2)
  
  df_final$Fecha_inicio <- as.Date("2024-10-10")
  df_final$FechaFin <- as.Date(NA)
  
  return(df_final)
}

# Función para registrar cambios históricos
# Si se actualiza la Frecuencia, también se recalcula el Periodo
actualizar_circuito <- function(df_actual, cod_recorrido, nuevos_valores, fecha_cambio = Sys.Date()) {
  # Encuentra el circuito actual y marca su fecha de fin
  idx <- which(df_actual$cod_recorrido == cod_recorrido & is.na(df_actual$FechaFin))
  
  if(length(idx) > 0) {
    # Cerrar el registro anterior
    df_actual[idx, "FechaFin"] <- fecha_cambio - 1
    
    # Crear nueva fila con los cambios
    nueva_fila <- df_actual[idx[1],]
    for(col in names(nuevos_valores)) {
      nueva_fila[[col]] <- nuevos_valores[[col]]
    }
    
    # Si se actualizó la frecuencia, recalcular el periodo
    if("Frecuencia" %in% names(nuevos_valores)) {
      nueva_fila$Periodo <- round(7 / as.numeric(nueva_fila$Frecuencia), 2)
    }
    
    nueva_fila$Fecha_inicio <- fecha_cambio
    nueva_fila$FechaFin <- as.Date(NA)
    
    # Añadir la nueva fila al dataframe
    df_actual <- rbind(df_actual, nueva_fila)
    
    # Guardar el dataframe actualizado con manejo de errores
    tryCatch({
      # Verificar que la ruta existe
      directorio <- dirname(ruta_archivo)
      if (!dir.exists(directorio)) {
        dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Intentar guardar el archivo
      saveRDS(df_actual, ruta_archivo)
      message("Archivo guardado exitosamente en: ", normalizePath(ruta_archivo))
    }, error = function(e) {
      # Capturar el error y mostrar mensaje
      message("Error al guardar el archivo: ", e$message)
      message("Ruta de intento: ", normalizePath(ruta_archivo, mustWork = FALSE))
      message("Directorio de trabajo actual: ", getwd())
      message("Ruta del proyecto: ", ruta_proyecto)
      message("¿Tienes permisos de escritura en este directorio?")
      
      # Intentar guardar en el directorio de usuario como alternativa
      backup_path <- file.path(Sys.getenv("HOME"), "circuitos_planificados_backup.rds")
      tryCatch({
        saveRDS(df_actual, backup_path)
        message("Se guardó una copia de respaldo en: ", backup_path)
      }, error = function(e2) {
        message("También falló el guardado de respaldo: ", e2$message)
      })
    })
  }
  
  return(df_actual)
}

cargar_o_crear_circuitos <- function() {
  if (file.exists(ruta_archivo)) {
    return(readRDS(ruta_archivo))
  } else {
    df_inicial <- turnos_planificados_por_circuito()
    
    # Asegurarse de que el directorio existe
    directorio <- dirname(ruta_archivo)
    if (!dir.exists(directorio)) {
      dir.create(directorio, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Intentar guardar con manejo de errores
    tryCatch({
      saveRDS(df_inicial, ruta_archivo)
      message("Archivo inicial creado en: ", normalizePath(ruta_archivo))
    }, error = function(e) {
      message("Error al crear el archivo inicial: ", e$message)
      message("Ruta de intento: ", normalizePath(ruta_archivo, mustWork = FALSE))
      message("Directorio de trabajo actual: ", getwd())
      message("Ruta del proyecto: ", ruta_proyecto)
    })
    
    return(df_inicial)
  }
}

datos_circuitos <- cargar_o_crear_circuitos()
# 
# # Ejemplo de cómo registrar un cambio
# # Cambiar turno y frecuencia del circuito A_101
# prueba <- actualizar_circuito(prueba, 
#                             "A_DU_RM_CL_101", 
#                             list(turno_planificado = "V", Frecuencia = "3.5"),
#                             as.Date("2024-11-01"))
# 
# 
# # nolint end

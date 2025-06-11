funcion_obtener_df_DFR <- function(type,nombre_archivo){
  
  url <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"
  archivo_json <- paste0(nombre_archivo, ".json")
  
  # Tu nombre de usuario (el mismo de QGIS) y tu contraseña (pedís al administrador si no la sabés)
  usuario <- "im4445285"
  contrasena <- "Nico1919*"
  
  # Parámetros de consulta WFS (el mismo tipo que QGIS)
  query <- list(
    service = "WFS",
    version = "1.0.0",
    request = "GetFeature",
    typeName = type,
    srsname = "EPSG:32721",
    outputFormat = "application/json"
  )
  
  # Hacer la consulta autenticada
  respuesta <- GET(
    url,
    query = query,
    authenticate(usuario, contrasena)
  )
  
  # Guardar el GeoJSON temporalmente
  writeBin(content(respuesta, "raw"), archivo_json)
  
  # Leer como objeto espacial
  ret <- st_read(archivo_json)
  
  return(ret)
  
}


guarda_posiciones_diarias <- function(consulta,
                                      nombre_archivo,
                                      fecha_inicio   = NULL,
                                      ruta_historico = paste0(nombre_archivo, ".rds")) {
  
  ############################## FUNCIONES INTERNAS #############################
  
  
  
  corregir_df_ubicacionesDFR <- function(df_paraarreglar){
    
    df_arreglado <- df_paraarreglar %>% 
      mutate(
        Estado = case_when(
          COD_MOTIVO_INACTIVA == 0 ~ NA_character_,
          COD_MOTIVO_INACTIVA == 1 ~ "Mantenimiento",
          COD_MOTIVO_INACTIVA == 2 ~ "Sin instalar",
          COD_MOTIVO_INACTIVA == 4 ~ "Roto, espera sustitución",
          TRUE                      ~ NA_character_   # cualquier otro caso → NA
        )
      ) %>% 
      select(GID,COD_RECORRIDO,POSICION,Estado,COD_MOTIVO_INACTIVA,Direccion_dfr,OBSERVACIONES,Fecha,COD_MUNICIPIO,geometry,UCREA,FCREA,UACT,FACT,FECHA_DESDE) %>% 
      rename(gid = GID,
             Circuito = COD_RECORRIDO,
             Posicion = POSICION,
             Observaciones = OBSERVACIONES,
             Municipio = COD_MUNICIPIO) %>% 
      arrange(
        desc(Fecha),
        Circuito,
        Posicion
      )
    
    return(df_arreglado)
    
  }
  ##################################################################################
  
  ultima_fecha_registro <- max(historico_llenado$Fecha, na.rm = TRUE)
  
  
  # 0) Asegurar Date
  fecha_fin <- as.Date(ultima_fecha_registro)
  
  # 1) Si no se pasa fecha_inicio, intentar extraer del histórico
  if (is.null(fecha_inicio) && file.exists(ruta_historico)) {
    historico <- readRDS(ruta_historico)
    ultima_fecha <- max(historico$Fecha, na.rm = TRUE)
    fecha_inicio <- ultima_fecha + 1
  }
  # 2) Si sigue NULL (primera corrida), procesar solo ayer
  if (is.null(fecha_inicio)) {
    fecha_inicio <- fecha_fin
  } else {
    fecha_inicio <- as.Date(fecha_inicio)
  }
  
  # 3) Si no hay rango válido, salir
  if (fecha_inicio > fecha_fin) {
    message("ℹ️ No hay días nuevos por procesar (", fecha_inicio, " > ", fecha_fin, ").")
    return(historico)
  }
  
  # 4) Generar secuencia de fechas
  fechas <- seq.Date(from = fecha_inicio, to = fecha_fin, by = "day")
  
  # 5) Mapear cada fecha y acumular
  nuevas_posiciones <- purrr::map_dfr(fechas, function(fecha) {
    posiciones_dfr <- funcion_obtener_df_DFR(consulta, nombre_archivo) %>%
      dplyr::mutate(GID = as.character(GID)) %>%
      dplyr::rename(Direccion_dfr = DIRECCION)
    
    ubicaciones_deldia <- historico_ubicaciones %>%
      dplyr::filter(Fecha == fecha) %>%
      dplyr::mutate(
        Direccion_consulta = ifelse(
          is.na(Numero),
          Calle,
          paste(Calle, Numero)
        )
      )
    
    posiciones_dfr %>%
      dplyr::left_join(
        ubicaciones_deldia %>% dplyr::select(gid, Direccion_consulta),
        by = c("GID" = "gid")
      ) %>%
      dplyr::mutate(
        Direccion_dfr = dplyr::coalesce(Direccion_dfr, Direccion_consulta, "NO HAY DIRECCION"),
        Fecha = fecha
      ) %>%
      dplyr::select(-Direccion_consulta)
  }) %>%
    dplyr::distinct()
  
  nuevas_posiciones <- corregir_df_ubicacionesDFR(nuevas_posiciones)

  # 6) Crear o actualizar histórico
  if (!file.exists(ruta_historico)) {
    saveRDS(nuevas_posiciones, file = ruta_historico)
    message("📦 Histórico creado en: ", ruta_historico)
    return(nuevas_posiciones)
  } else {
    historico <- readRDS(ruta_historico)
    historico_actualizado <- dplyr::bind_rows(nuevas_posiciones,historico) %>%
      dplyr::distinct()
    saveRDS(historico_actualizado, file = ruta_historico)
    message("🔄 Histórico actualizado en: ", ruta_historico)
    return(historico_actualizado)
  }
}



#---------------------------------------------------------------
# Función: actualizar_posiciones_historico
#
# Parámetros:
#   consulta      : el string con el nombre de la consulta 
#                   (p.ej. "dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO")
#   nombre_archivo: nombre base para el archivo RDS donde se guardará 
#                   la versión procesada (p.ej. "posiciones_historico")
#
# Comportamiento:
#   1) Si "<nombre_archivo>.rds" NO existe:
#       - Descarga y procesa TODO.
#       - Añade Fecha_agregado = Sys.Date() a cada fila.
#       - Guarda el dataframe completo en "<nombre_archivo>.rds".
#       - Devuelve ese dataframe completo.
#
#   2) Si "<nombre_archivo>.rds" ya existe:
#       - Lee la versión anterior desde disco (df_anterior).
#       - Descarga y procesa TODO de nuevo (df_actual).
#       - Calcula filas nuevas (anti_join por GID).
#           · Si hay filas nuevas:
#               a) A esas filas nuevas les agrega Fecha_agregado = Sys.Date().
#               b) Concatena al RDS previo y sobrescribe "<nombre_archivo>.rds".
#               c) Devuelve el dataframe completo actualizado (viejos + nuevos).
#           · Si NO hay filas nuevas:
#               a) No modifica el RDS.
#               b) Devuelve el dataframe completo tal cual estaba (df_anterior).
#---------------------------------------------------------------
actualizar_posiciones_historico <- function(consulta, 
                                            nombre_archivo,
                                            ruta_historico = paste0(nombre_archivo, ".rds")) {
  # 1) Definir la ruta al archivo RDS local
  #archivo_rds <- paste0(nombre_archivo, ".rds")
  
  # 2) Función interna que descarga y aplica TODO tu pipeline original,
  #    reemplazando rows_update() por un mutate(...) con if_else().
  procesar_df_completo <- function() {
    # 2.1) Descargar crudo desde el servidor
    df <- funcion_obtener_df_DFR(consulta, nombre_archivo)
    
    # 2.2) Corregir años "00xx" → "20xx" y convertir a Date
    df <- df %>%
      mutate(
        FECHA_DESDE = if_else(
          substr(as.character(FECHA_DESDE), 1, 2) == "00",
          sub("^00", "20", as.character(FECHA_DESDE)),
          as.character(FECHA_DESDE)
        ),
        FECHA_HASTA = if_else(
          substr(as.character(FECHA_HASTA), 1, 2) == "00",
          sub("^00", "20", as.character(FECHA_HASTA)),
          as.character(FECHA_HASTA)
        ),
        FECHA_DESDE = as.Date(FECHA_DESDE, format = "%Y-%m-%d"),
        FECHA_HASTA  = as.Date(FECHA_HASTA,  format = "%Y-%m-%d")
      ) %>%
      # 2.3) Convertir FCREA y FACT a POSIXct
      mutate(
        FCREA = ymd_hms(FCREA),
        FACT  = ymd_hms(FACT)
      )
    
    # 2.4) Filtrar por prefix y número extraído de COD_RECORRIDO
    df <- df %>%
      mutate(
        prefix = sub("^(.*?)_.*$", "\\1", COD_RECORRIDO),
        numero = as.numeric(sub(".*_(\\d+)$", "\\1", COD_RECORRIDO))
      ) %>%
      filter(
        prefix %in% c("A","C","D","E","F","G","CH"),
        numero >= 100
      ) %>%
      select(-prefix, -numero)
    
    # 2.5) Corregir FECHA_HASTA “mal” si es > hoy, usando mutate() + if_else()
    hoy <- Sys.Date()
    df <- df %>%
      mutate(
        FECHA_HASTA = if_else(
          FECHA_HASTA > hoy,
          as.Date(FACT),   # reemplazo por la fecha de FACT
          FECHA_HASTA
        )
      )
    
    return(df)
  }
  
  # 3) Si NO existe el RDS, primera descarga + guardado
  if (!file.exists(ruta_historico)) {
    message("No se encontró '", ruta_historico, "'. Descargando y procesando TODO por primera vez...")
    
    df_completo <- procesar_df_completo()
    # 3.1) Agregar Fecha_agregado = Sys.Date() a todas las filas
    df_completo <- df_completo %>%
      mutate(Fecha_agregado = Sys.Date()) %>% 
      arrange(desc(FECHA_HASTA),desc(FACT))
    

    # 3.2) Guardar en disco
    saveRDS(df_completo, ruta_historico)
    
    # 3.3) Devolver el dataframe completo
    return(df_completo)
  }
  
  # 4) Si el RDS ya existe, sólo buscar diferencias y actualizar si hace falta
  message("El archivo existe. Leyendo versión previa ('", ruta_historico, "')...")
  df_anterior <- readRDS(ruta_historico)
  
  message("Descargando y procesando la versión actual nuevamente...")
  df_actual <- procesar_df_completo()
  
  # 4.1) Identificar filas nuevas (por GID)
  df_nuevos <- df_actual %>%
    filter(! GID %in% df_anterior$GID)
  
  # 4.2) Si hay filas nuevas, agrego Fecha_agregado y actualizo el RDS
  if (nrow(df_nuevos) > 0) {
    message(nrow(df_nuevos), " registros nuevos encontrados. Actualizando '", ruta_historico, "'...")
    
    df_nuevos <- df_nuevos %>%
      mutate(Fecha_agregado = Sys.Date())
    
    df_actualizado <- bind_rows(df_anterior, df_nuevos) %>% 
      arrange(desc(FECHA_HASTA),desc(FACT))
    saveRDS(df_actualizado, ruta_historico)
      
    # 4.3) Devolver siempre el dataframe completo actualizado
    return(df_actualizado)
  }
  
  # 4.4) Si NO hay filas nuevas, devolver el dataframe completo tal cual estaba
  message("No hay registros nuevos (por GID). Devolviendo el histórico completo sin cambios.")
  return(df_anterior)
}






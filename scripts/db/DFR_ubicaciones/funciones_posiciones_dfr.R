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
  print("[DEBUG] INICIO guarda_posiciones_diarias")
  flush.console()
  ############################## FUNCIONES INTERNAS #############################
  
  
  
  corregir_df_ubicacionesDFR <- function(df_paraarreglar){
    print("[DEBUG] Entrando a corregir_df_ubicacionesDFR")
    flush.console()
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
    print("[DEBUG] Saliendo de corregir_df_ubicacionesDFR")
    flush.console()
    return(df_arreglado)
    
  }
  ##################################################################################
  
  ultima_fecha_registro <- max(historico_llenado$Fecha, na.rm = TRUE)
  print(paste("[DEBUG] ultima_fecha_registro:", ultima_fecha_registro))
  flush.console()
  # 0) Asegurar Date
  fecha_fin <- as.Date(ultima_fecha_registro)
  
  # 1) Si no se pasa fecha_inicio, intentar extraer del histórico
  if (is.null(fecha_inicio) && file.exists(ruta_historico)) {
    print("[DEBUG] Leyendo histórico existente para fecha_inicio")
    flush.console()
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
    print("[DEBUG] No hay días nuevos por procesar")
    flush.console()
    return(historico)
  }
  
  # 4) Generar secuencia de fechas
  print("[DEBUG] Generando secuencia de fechas")
  flush.console()
  fechas <- seq.Date(from = fecha_inicio, to = fecha_fin, by = "day")
  print(fechas)
  flush.console()
  # 5) Mapear cada fecha y acumular
  print("[DEBUG] Mapeando fechas y acumulando nuevas posiciones")
  flush.console()
  nuevas_posiciones <- purrr::map_dfr(fechas, function(fecha) {
    print(paste("[DEBUG] Procesando fecha:", fecha))
    flush.console()
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
  print("[DEBUG] Nuevas posiciones generadas")
  flush.console()
  nuevas_posiciones <- corregir_df_ubicacionesDFR(nuevas_posiciones)
  print("[DEBUG] Nuevas posiciones corregidas")
  flush.console()
  # 6) Crear o actualizar histórico
  if (!file.exists(ruta_historico)) {
    print("[DEBUG] No existe el archivo histórico, guardando por primera vez")
    flush.console()
    saveRDS(nuevas_posiciones, ruta_historico)
    return(nuevas_posiciones)
  } else {
    print("[DEBUG] Archivo histórico existe, leyendo y actualizando")
    flush.console()
    historico <- readRDS(ruta_historico)
    historico <- bind_rows(historico, nuevas_posiciones) %>%
      dplyr::distinct()
    saveRDS(historico, ruta_historico)
    return(historico)
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
  print("[DEBUG] INICIO actualizar_posiciones_historico")
  flush.console()
  # 1) Definir la ruta al archivo RDS local
  #archivo_rds <- paste0(nombre_archivo, ".rds")
  
  # 2) Función interna que descarga y aplica TODO tu pipeline original,
  #    reemplazando rows_update() por un mutate(...) con if_else().
  procesar_df_completo <- function() {
    print("[DEBUG] Entrando a procesar_df_completo")
    flush.console()
    # 2.1) Descargar crudo desde el servidor
    df <- funcion_obtener_df_DFR(consulta, nombre_archivo)
    print("[DEBUG] Descargado df de funcion_obtener_df_DFR")
    flush.console()
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
    print("[DEBUG] Fechas corregidas y convertidas")
    flush.console()
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
    print("[DEBUG] Filtrado por prefix y numero")
    flush.console()
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
    print("[DEBUG] FECHA_HASTA corregida")
    flush.console()
    return(df)
  }
  
  # 3) Si NO existe el RDS, primera descarga + guardado
  if (!file.exists(ruta_historico)) {
    print("[DEBUG] No existe el archivo histórico, descargando y procesando todo")
    flush.console()
    df_completo <- procesar_df_completo()
    print("[DEBUG] Dataframe completo procesado")
    flush.console()
    df_completo <- df_completo %>%
      mutate(Fecha_agregado = Sys.Date()) %>% 
      arrange(desc(FECHA_HASTA),desc(FACT))
    print("[DEBUG] Dataframe completo con Fecha_agregado")
    flush.console()
    saveRDS(df_completo, ruta_historico)
    print("[DEBUG] Dataframe guardado en disco")
    flush.console()
    return(df_completo)
  }
  print("[DEBUG] El archivo existe. Leyendo versión previa")
  flush.console()
  df_anterior <- readRDS(ruta_historico)
  print("[DEBUG] Dataframe anterior leído")
  flush.console()
  print("[DEBUG] Descargando y procesando la versión actual nuevamente")
  flush.console()
  df_actual <- procesar_df_completo()
  print("[DEBUG] Dataframe actual procesado")
  flush.console()
  df_nuevos <- df_actual %>%
    filter(! GID %in% df_anterior$GID)
  print(paste("[DEBUG] Filas nuevas encontradas:", nrow(df_nuevos)))
  flush.console()
  # 4.1) Identificar filas nuevas (por GID)
  df_nuevos <- df_actual %>%
    filter(! GID %in% df_anterior$GID)
  
  # 4.2) Si hay filas nuevas, agrego Fecha_agregado y actualizo el RDS
  if (nrow(df_nuevos) > 0) {
    print("[DEBUG] Hay filas nuevas, actualizando archivo histórico")
    flush.console()
    df_nuevos <- df_nuevos %>%
      mutate(Fecha_agregado = Sys.Date())
    df_actualizado <- bind_rows(df_anterior, df_nuevos) %>% 
      arrange(desc(FECHA_HASTA),desc(FACT))
    saveRDS(df_actualizado, ruta_historico)
    print("[DEBUG] Archivo histórico actualizado y guardado")
    flush.console()
    return(df_actualizado)
  }
  print("[DEBUG] No hay filas nuevas, devolviendo dataframe anterior")
  flush.console()
  return(df_anterior)
}






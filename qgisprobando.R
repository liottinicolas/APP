library(httr)
library(sf)
library(dplyr)

### FUNCIONES

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
    return(invisible(NULL))
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
  
  # 6) Crear o actualizar histórico
  if (!file.exists(ruta_historico)) {
    saveRDS(nuevas_posiciones, file = ruta_historico)
    message("📦 Histórico creado en: ", ruta_historico)
    return(nuevas_posiciones)
  } else {
    historico <- readRDS(ruta_historico)
    historico_actualizado <- dplyr::bind_rows(historico, nuevas_posiciones) %>%
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
actualizar_posiciones_historico <- function(consulta, nombre_archivo) {
  # 1) Definir la ruta al archivo RDS local
  archivo_rds <- paste0(nombre_archivo, ".rds")
  
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
  if (!file.exists(archivo_rds)) {
    message("No se encontró '", archivo_rds, "'. Descargando y procesando TODO por primera vez...")
    
    df_completo <- procesar_df_completo()
    # 3.1) Agregar Fecha_agregado = Sys.Date() a todas las filas
    df_completo <- df_completo %>%
      mutate(Fecha_agregado = Sys.Date())
    
    # 3.2) Guardar en disco
    saveRDS(df_completo, archivo_rds)
    
    # 3.3) Devolver el dataframe completo
    return(df_completo)
  }
  
  # 4) Si el RDS ya existe, sólo buscar diferencias y actualizar si hace falta
  message("El archivo existe. Leyendo versión previa ('", archivo_rds, "')...")
  df_anterior <- readRDS(archivo_rds)
  
  message("Descargando y procesando la versión actual nuevamente...")
  df_actual <- procesar_df_completo()
  
  # 4.1) Identificar filas nuevas (por GID)
  df_nuevos <- df_actual %>%
    filter(! GID %in% df_anterior$GID)
  
  # 4.2) Si hay filas nuevas, agrego Fecha_agregado y actualizo el RDS
  if (nrow(df_nuevos) > 0) {
    message(nrow(df_nuevos), " registros nuevos encontrados. Actualizando '", archivo_rds, "'...")
    
    df_nuevos <- df_nuevos %>%
      mutate(Fecha_agregado = Sys.Date())
    
    df_actualizado <- bind_rows(df_anterior, df_nuevos)
    saveRDS(df_actualizado, archivo_rds)
    
    # 4.3) Devolver siempre el dataframe completo actualizado
    return(df_actualizado)
  }
  
  # 4.4) Si NO hay filas nuevas, devolver el dataframe completo tal cual estaba
  message("No hay registros nuevos (por GID). Devolviendo el histórico completo sin cambios.")
  return(df_anterior)
}


### IMPLEMENTACION

resultado <- guarda_posiciones_diarias(
  consulta       = "dfr:E_DF_POSICIONES_RECORRIDO",     # tu objeto de consulta
  nombre_archivo = "historico_posiciones_dia"     # creará/actualizará historic_posiciones.rds
)


# 1) Primera llamada → descarga todo, guarda el RDS y devuelve el DF completo con Fecha_agregado
todos_los_datos <- actualizar_posiciones_historico(
  consulta       = "dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO",
  nombre_archivo = "posiciones_historico"
)


# 
# consulta <- "dfr:E_DF_POSICIONES_RECORRIDO"
# nombre_archivo <- "posiciones_diario"
# 
# asd <- funcion_obtener_df_DFR(consulta,nombre_archivo)
# 
# asds <- asd %>%
#   group_by(GID) %>%
#   summarise(ns = n(), .groups = "drop")
# 
# asd <- asd %>%
#   mutate(
#     # 1) Arreglamos la parte del año: si empieza con "00" (ej. "0023-..."),
#     #    reemplazamos "00" por "20" para que quede "2023-..."
#     FECHA_DESDE = if_else(
#       substr(as.character(FECHA_DESDE), 1, 2) == "00",
#       sub("^00", "20", as.character(FECHA_DESDE)),
#       as.character(FECHA_DESDE)
#     ),
#     FECHA_HASTA = if_else(
#       substr(as.character(FECHA_HASTA), 1, 2) == "00",
#       sub("^00", "20", as.character(FECHA_HASTA)),
#       as.character(FECHA_HASTA)
#     ),
#     # 2) Ahora convertimos esas cadenas corregidas a Date
#     FECHA_DESDE = as.Date(FECHA_DESDE, format = "%Y-%m-%d"),
#     FECHA_HASTA  = as.Date(FECHA_HASTA,  format = "%Y-%m-%d")
#   ) %>%
#   mutate(
#     FCREA = ymd_hms(FCREA),
#     FACT  = ymd_hms(FACT)
#   )
# 
# asd_filtrado <- asd %>%
#   # 1) Crear dos columnas auxiliares:
#   #    - prefix: todo lo que va antes del primer “_”
#   #    - numero: extraer los dígitos después del último “_” y convertirlos a numérico
#   mutate(
#     prefix = sub("^(.*?)_.*$", "\\1", COD_RECORRIDO),
#     numero = as.numeric(sub(".*_(\\d+)$", "\\1", COD_RECORRIDO))
#   ) %>%
#   # 2) Filtrar según las dos condiciones:
#   #    a) prefix debe estar en el vector c("A","C","D","E","F","G","CH")
#   #    b) numero >= 100
#   filter(
#     prefix %in% c("A","C","D","E","F","G","CH"),
#     numero >= 100
#   ) %>%
#   # 3) (Opcional) Eliminar las columnas auxiliares si no las necesitas luego:
#   select(-prefix, -numero)
# 
# hoy <- Sys.Date()
# 
# asd_filtrado_malfecha <- asd_filtrado %>%
#   filter(FECHA_HASTA > hoy)
# 
# asd_corregido <- asd_filtrado_malfecha %>%
#   mutate(
#     # Reemplazamos FECHA_HASTA por la fecha extraída de FACT
#     FECHA_HASTA = as.Date(FACT)
#   )
# 
# 
# 
# consulta <- "dfr:E_DF_POSICIONES_RECORRIDO"
# nombre_archivo <- "posiciones_diario"
# 
# 
# fecha_hoy <- Sys.Date()-1
# posiciones_dfr <- funcion_obtener_df_DFR(consulta,nombre_archivo)
# posiciones_dfr <- posiciones_dfr %>%
#   mutate(GID = as.character(GID)) %>%
#   rename(Direccion_dfr = DIRECCION)
# 
# 
# ubicaciones_deldia <- historico_ubicaciones %>%
#   filter(Fecha == fecha_hoy) %>%
#   mutate(
#     Direccion_consulta = if_else(
#       is.na(Numero),
#       Calle,
#       paste(Calle, Numero)
#     )
#   )
# 
# 
# posiciones_completas_deldia <- posiciones_dfr %>%
#   # 1) Unir el histórico para traer la columna Calle según coincidencia de GID ↔ gid
#   left_join(
#     ubicaciones_deldia %>% select(gid, Direccion_consulta),
#     by = c("GID" = "gid")
#   ) %>%
#   # 2) Rellenar DIRECCION: si ya existía, mantenerla;
#   #    si era NA y llega Calle, usar Calle;
#   #    si sigue siendo NA, poner "NO HAY DIRECCION"
#   mutate(
#     Direccion_dfr = coalesce(Direccion_dfr, Direccion_consulta, "NO HAY DIRECCION")
#   ) %>%
#   mutate(Fecha = fecha_hoy)
# 
# 
# 
# 
# 
# 
# 
# 
# guarda_posiciones <- function(consulta, nombre_archivo,
#                               ruta_historico = paste0("historico_", nombre_archivo, ".rds")) {
#   # 1) Fecha objetivo (ayer)
#   fecha_hoy <- Sys.Date() - 1
# 
#   # 2) Obtener y preparar las posiciones
#   posiciones_dfr <- funcion_obtener_df_DFR(consulta, nombre_archivo) %>%
#     dplyr::mutate(GID = as.character(GID)) %>%
#     dplyr::rename(Direccion_dfr = DIRECCION)
# 
#   # 3) Filtrar ubicaciones históricas del día
#   ubicaciones_deldia <- historico_ubicaciones %>%
#     dplyr::filter(Fecha == fecha_hoy) %>%
#     dplyr::mutate(
#       Direccion_consulta = ifelse(
#         is.na(Numero),
#         Calle,
#         paste(Calle, Numero)
#       )
#     )
# 
#   # 4) Generar nuevas posiciones con dirección y fecha
#   nuevas_posiciones <- posiciones_dfr %>%
#     dplyr::left_join(
#       ubicaciones_deldia %>% dplyr::select(gid, Direccion_consulta),
#       by = c("GID" = "gid")
#     ) %>%
#     dplyr::mutate(
#       Direccion_dfr = dplyr::coalesce(Direccion_dfr, Direccion_consulta, "NO HAY DIRECCION"),
#       Fecha = fecha_hoy
#     ) %>%
#     dplyr::select(-Direccion_consulta)
# 
#   # 5) Guardar o actualizar el histórico
#   if (!file.exists(ruta_historico)) {
#     nuevas_posiciones <- nuevas_posiciones %>%
#       distinct()
#     saveRDS(nuevas_posiciones, file = ruta_historico)
#     message("📦 Histórico creado en: ", ruta_historico)
#     return(nuevas_posiciones)
#   } else {
#     historico <- readRDS(ruta_historico)
#     historico_actualizado <- dplyr::bind_rows(historico, nuevas_posiciones)
#     historico_actualizado <- historico_actualizado %>%
#       distinct()
#     saveRDS(historico_actualizado, file = ruta_historico)
#     message("🔄 Histórico actualizado en: ", ruta_historico)
#     return(historico_actualizado)
#   }
# }
# 
# 
# 




# ############################################################################################################################
# ############################### OBTENER LAS POSICIONES #################################
# ############################################################################################################################
# 
# URL base del WFS con filtro
url <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"

# Tu nombre de usuario (el mismo de QGIS) y tu contraseña (pedís al administrador si no la sabés)
usuario <- "im4445285"
contrasena <- "Nico1919*"

# Parámetros de consulta WFS (el mismo tipo que QGIS)
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "dfr:E_DF_POSICIONES_RECORRIDO",
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
writeBin(content(respuesta, "raw"), "posiciones_recorrido.json")

# Leer como objeto espacial
posiciones <- st_read("posiciones_recorrido.json")


############################################################################################################################
############################################################################################################################
############################################################################################################################

--------------
  
  library(httr)
library(sf)

# URL base del WFS
url <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"

# Credenciales
usuario <- "im4445285"
contrasena <- "Nico1919*"

# Parámetros de consulta para la capa "Límite de circuitos"
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "dfr:E_DF_ZONA_RECORRIDO",
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
writeBin(content(respuesta, "raw"), "zona_recorrido.json")

# Leer como objeto espacial
zona <- st_read("zona_recorrido.json")

### mapa

library(leaflet)

##### FUNCION QUE DIBUJA UN SOLO CIRCUITO

drawZonaLeaflet <- function(zona_df, fila = 1, tile_provider = "OpenStreetMap") {
  # zona_df: un sf con columna geometry y CRS definido
  # fila: índice de la fila que quieres dibujar
  # tile_provider: nombre de proveedor de tiles de leaflet
  
  # 1. Extraer la matriz de coordenadas
  pts <- zona_df$geometry[[fila]][[1]]
  
  # 2. Cerrar el polígono si hiciera falta
  if (!all(pts[1, ] == pts[nrow(pts), ])) {
    pts <- rbind(pts, pts[1, ])
  }
  
  # 3. Crear un objeto sf POLYGON en CRS original
  poly_sfc <- st_sfc(st_polygon(list(pts)), crs = st_crs(zona_df))
  poly_sf  <- st_sf(data.frame(id = zona_df$id[fila]), geometry = poly_sfc)
  
  # 4. Reproyectar a lon/lat (EPSG:4326) para leaflet
  poly_ll <- st_transform(poly_sf, 4326)
  
  # 5. Construir y devolver el mapa leaflet
  leaflet(poly_ll) %>%
    {
      if (tolower(tile_provider) == "cartodb") {
        addProviderTiles(., "CartoDB.Positron")
      } else if (tolower(tile_provider) == "stamen") {
        addProviderTiles(., "Stamen.TonerLite")
      } else {
        addTiles(.)
      }
    } %>%
    addPolygons(
      color       = "darkgreen",
      weight      = 2,
      fillOpacity = 0.3,
      popup       = ~paste0("<strong>ID:</strong> ", id)
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = "darkgreen",
      labels   = paste("Zona ID", zona_df$id[fila]),
      title    = "Polígono"
    )
}

drawZonasLeaflet_global <- function(zona_df, tile_provider = "OpenStreetMap") {
  # 1. Reproyectar todo a lon/lat
  zonas_ll <- sf::st_transform(zona_df, 4326)
  
  # 2. Iniciar el mapa con el proveedor de tiles
  mapa <- switch(
    tolower(tile_provider),
    cartodb = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("CartoDB.Positron"),
    stamen  = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("Stamen.TonerLite"),
    leaflet::leaflet(zonas_ll) %>% leaflet::addTiles()
  )
  
  # 3. Añadir polígonos y una sola leyenda
  mapa %>%
    leaflet::addPolygons(
      color       = "darkgreen",
      weight      = 2,
      fillOpacity = 0.3,
      popup       = ~paste0("<strong>ID:</strong> ", id)
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      colors   = "darkgreen",
      labels   = "Zonas",
      title    = "Polígonos"
    )
}

mapa <- drawZonasLeaflet_global(zona, tile_provider = "cartodb")

# Y para visualizarlo en RStudio:
mapa


zona_filtrada <- zona %>%
  select(GID,COD_RECORRIDO,MUNICIPIO,geometry)

# Con esto GDAL convertirá la geometría a WKT automáticamente
st_write(
  obj           = zona_filtrada,
  dsn           = "salida_gdal.csv",
  driver        = "CSV",
  layer_options = "GEOMETRY=AS_WKT"
)




drawPolygonsLeaflet <- function(sf_df,
                                tile_provider = c("OpenStreetMap", "CartoDB", "Stamen"),
                                color        = "darkgreen",
                                weight       = 2,
                                fillOpacity  = 0.3) {
  tile_provider <- match.arg(tile_provider)
  
  # 1. Asegurar que es POLYGON/MULTIPOLYGON
  if (!any(grepl("POLYGON", sf::st_geometry_type(sf_df)))) {
    stop("El objeto sf no contiene geometrías de tipo POLYGON o MULTIPOLYGON.")
  }
  
  # 2. Reproyectar a lon/lat (EPSG:4326) para leaflet
  sf_ll <- sf::st_transform(sf_df, 4326)
  
  # 3. Crear base de leaflet y añadir tiles
  mapa <- leaflet(sf_ll)
  if (tile_provider == "CartoDB") {
    mapa <- addProviderTiles(mapa, "CartoDB.Positron")
  } else if (tile_provider == "Stamen") {
    mapa <- addProviderTiles(mapa, "Stamen.TonerLite")
  } else {
    mapa <- addTiles(mapa)
  }
  
  # 4. Añadir polígonos sin ningún popup ni leyenda
  mapa <- mapa %>%
    addPolygons(
      color       = color,
      weight      = weight,
      fillOpacity = fillOpacity
    )
  
  # 5. Devolver el mapa
  mapa
}

mapa_zonas <- drawPolygonsLeaflet(zona, tile_provider = "CartoDB")
mapa_zonas  # lo despliega en RStudio o tu navegador
--------------------
  
  
  # URL base del WFS
  url <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"

# Tus credenciales
usuario <- "im4445285"
contrasena <- "Nico1919*"

# Parámetros de la consulta
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "dfr:E_DF_RUTAS_RECORRIDO",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
)

# Realizar consulta autenticada
respuesta <- GET(
  url,
  query = query,
  authenticate(usuario, contrasena)
)

# Guardar el archivo GeoJSON temporalmente
writeBin(content(respuesta, "raw"), "rutas_recorrido.json")

# Leer en R como objeto espacial
rutas <- st_read("rutas_recorrido.json")


### Dibujar rutas

# Función para graficar una sola ruta (LINESTRING) en leaflet
drawRutaLeaflet <- function(rutas_df, fila = 1, tile_provider = c("OpenStreetMap", "CartoDB", "Stamen")) {
  tile_provider <- match.arg(tile_provider)
  
  # 1. Tomar solo la fila indicada
  ruta_sf <- rutas_df[fila, ]
  
  # 2. Reproyectar a WGS84 (lon/lat) para leaflet
  ruta_ll <- st_transform(ruta_sf, 4326)
  
  # 3. Crear mapa leaflet
  mapa <- leaflet(ruta_ll)
  
  # 4. Añadir proveedor de tiles
  mapa <- switch(tile_provider,
                 CartoDB       = addProviderTiles(mapa, "CartoDB.Positron"),
                 Stamen        = addProviderTiles(mapa, "Stamen.TonerLite"),
                 OpenStreetMap = addTiles(mapa)
  )
  
  # 5. Dibujar la línea
  mapa <- mapa %>%
    addPolylines(
      color    = "blue",
      weight   = 3,
      opacity  = 0.7,
      popup    = ~paste0(
        "<strong>ID:</strong> ", id, "<br/>",
        "<strong>Ruta:</strong> ", NOM_RUT, "<br/>",
        "<strong>Desde:</strong> ", FECHA_DESDE
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = "blue",
      labels   = paste("Ruta ID", ruta_sf$id),
      title    = "Linea"
    )
  
  mapa
}

mapa1 <- drawRutaLeaflet(rutas, fila = 2, tile_provider = "CartoDB")
mapa1


# Función para graficar todas las rutas (LINESTRING) en leaflet
drawAllRutasLeaflet <- function(rutas_df, tile_provider = c("OpenStreetMap", "CartoDB", "Stamen")) {
  tile_provider <- match.arg(tile_provider)
  
  # 1. Reproyectar todo a WGS84 (lon/lat) para leaflet
  rutas_ll <- st_transform(rutas_df, 4326)
  
  # 2. Crear el mapa
  mapa <- leaflet(rutas_ll)
  
  # 3. Añadir proveedor de tiles
  mapa <- switch(tile_provider,
                 CartoDB       = addProviderTiles(mapa, "CartoDB.Positron"),
                 Stamen        = addProviderTiles(mapa, "Stamen.TonerLite"),
                 OpenStreetMap = addTiles(mapa)
  )
  
  # 4. Dibujar todas las líneas de una vez
  mapa <- mapa %>%
    addPolylines(
      color    = "blue",
      weight   = 3,
      opacity  = 0.7,
      popup    = ~paste0(
        "<strong>ID:</strong> ", id, "<br/>",
        "<strong>Ruta:</strong> ", NOM_RUT, "<br/>",
        "<strong>Desde:</strong> ", FECHA_DESDE
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = "blue",
      labels   = "Todas las rutas",
      title    = "Lineas"
    )
  
  # 5. Devolver el mapa
  mapa
}

# Ejemplo de uso:
mapa_todas <- drawAllRutasLeaflet(rutas, tile_provider = "CartoDB")
mapa_todas



-------------------
#   💾 Nombre de capa WFS: geomatica:v_sig_accesos_montevideo
# 
# 🏷️ Título: “Direcciones de Montevideo”
# 
# 📝 Resumen: contiene dirección unificada (calle + puerta + letra)
# 
# 🌍 CRS por defecto: EPSG:32721 (UTM zona 21S)
#   
  library(httr)
library(sf)

# URL del servidor público
url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Parámetros de la consulta
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "geomatica:v_sig_accesos_montevideo",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
 # maxFeatures = 100  # opcional: limitar cantidad para prueba
)

# Ejecutar la consulta
respuesta <- GET(url, query = query)

# Guardar como GeoJSON
writeBin(content(respuesta, "raw"), "accesos_montevideo.json")

# Leer como sf
accesos <- st_read("accesos_montevideo.json", quiet = TRUE)


-------------------
  ### demora mucho ver
  
  library(httr)
library(sf)

# URL base del GeoServer público
url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Parámetros para consultar la capa de contenedores
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "ide:V_DF_POSICIONES_MAPAWEB_GEOM",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
  ,
  maxFeatures = 100  # Cambiar o quitar si querés más datos
)

# Hacer la consulta
respuesta <- GET(url, query = query)

# Guardar temporalmente
writeBin(content(respuesta, "raw"), "contenedores.json")

# Leer como objeto sf
contenedores <- st_read("contenedores.json", quiet = TRUE)

# Ver nombres de columnas
names(contenedores)



------------
  
#   
#   # URL base del GeoServer público
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de la consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:DF_CAP_CONTENEDORES",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json",
#   maxFeatures = 100  # opcional, para prueba rápida
# )
# 
# # Hacer la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar el contenido en archivo temporal
# writeBin(content(respuesta, "raw"), "cap_contenedores.json")
# 
# # Leer como objeto espacial
# cap <- st_read("cap_contenedores.json", quiet = TRUE)
# 
# # Mostrar nombres de columnas
# names(cap)

---------------
# 
# #   📍 Descripción: Ferias vecinales de Montevideo con su ubicación y extensión según el día de la semana.
# # 🌍 CRS: EPSG:32721 (UTM zona 21 Sur)
# # 🔓 Servidor público: http://geoserver.montevideo.gub.uy/geoserver/wfs
# #     
#   # URL base del servidor WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "ide:V_SF_FERIAS_MAPAWEB",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json",
#   maxFeatures = 100  # opcional para limitar prueba
# )
# 
# # Ejecutar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar temporalmente
# writeBin(content(respuesta, "raw"), "ferias_vecinales.json")
# 
# # Leer como objeto espacial
# ferias <- st_read("ferias_vecinales.json", quiet = TRUE)
# 
# # Ver nombres de columnas
# names(ferias)
# 
# --------------------------
#   
# #   📍 Descripción: Ferias vecinales de Montevideo representadas como polígonos georreferenciados.
# # 🌐 CRS: EPSG:32721
# # 🔓 Servidor WFS público: http://geoserver.montevideo.gub.uy/geoserver/wfs
# #   
#   # URL del servidor GeoServer
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "ide:V_SF_FERIAS_GEOM",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json",
#   maxFeatures = 100  # opcional, para muestra inicial
# )
# 
# # Realizar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar respuesta en archivo temporal
# writeBin(content(respuesta, "raw"), "ferias_geom.json")
# 
# # Leer en R como objeto espacial
# ferias_geom <- st_read("ferias_geom.json", quiet = TRUE)
# 
# # Ver columnas disponibles
# names(ferias_geom)
# 

------------
  
  
#   📍 Descripción: Límite de los Municipios de Montevideo según el Decreto No. 33227
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público: http://geoserver.montevideo.gub.uy/geoserver/wfs


  # URL del servidor público de la IMM
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Parámetros para consultar la capa de municipios
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "geomatica:ide_v_sig_municipios",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
)

# Ejecutar la consulta
respuesta <- GET(url, query = query)

# Guardar temporalmente
writeBin(content(respuesta, "raw"), "municipios.json")

# Leer como objeto espacial
municipios <- st_read("municipios.json", quiet = TRUE)

# Ver columnas
names(municipios)

mapa_municipios <- drawPolygonsLeaflet(municipios, tile_provider = "CartoDB")
mapa_municipios  # lo despliega en RStudio o tu navegador

-------
# 
#   #   📍 Título: Municipios de Montevideo
#   # 📝 Descripción: División municipal del departamento de Montevideo
#   # 🌍 CRS: EPSG:32721
#   # 🔓 Acceso: Público desde http://geoserver.montevideo.gub.uy/geoserver/wfs  
#   
#   # URL del GeoServer público
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de la consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:sig_municipios",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # Ejecutar consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar como archivo temporal
# writeBin(content(respuesta, "raw"), "sig_municipios.json")
# 
# # Leer como objeto sf
# municipios_2 <- st_read("sig_municipios.json", quiet = TRUE)




----
# 
#   📍 Título: V_DF_POSICIONES_MAPAWEB2_GEOM
# 📝 Descripción: No tiene un <Abstract>, pero por el nombre y palabras clave se asocia a posiciones de contenedores (como V_DF_POSICIONES_MAPAWEB_GEOM)
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público: http://geoserver.montevideo.gub.uy/geoserver/wfs  
#   
#   # URL del servidor WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_POSICIONES_MAPAWEB2_GEOM",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # Para prueba rápida
# )
# 
# # Ejecutar consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar temporalmente
# writeBin(content(respuesta, "raw"), "posiciones_mapaweb2.json")
# 
# # Leer en R
# posiciones2 <- st_read("posiciones_mapaweb2.json", quiet = TRUE)

-
------------------
  
#   📍 Descripción: Similar o complementaria a imm:V_DF_POSICIONES_MAPAWEB2_GEOM
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público: http://geoserver.montevideo.gub.uy/geoserver/wfs
  
  # Servidor público de GeoServer
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros WFS
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_POSICIONES_MAPAWEB_GEOM",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # opcional para prueba
# )
# 
# # Ejecutar consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar respuesta
# writeBin(content(respuesta, "raw"), "posiciones_mapaweb_geom.json")
# 
# # Leer con sf
# posiciones_geom <- st_read("posiciones_mapaweb_geom.json", quiet = TRUE)


# ----------------
#   
# #   📍 Título: V_DF_POSICIONES_RECORRIDO_GEOM
# # 📝 Descripción: No está especificada, pero por el nombre, probablemente representa las posiciones planificadas de los contenedores dentro de los recorridos de recolección.
# # 🌍 CRS: EPSG:32721
# # 🔓 Servidor: http://geoserver.montevideo.gub.uy/geoserver/wfs
#   
#   # URL base del servidor GeoServer público
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Armar la consulta WFS
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_POSICIONES_RECORRIDO_GEOM",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # para prueba
# )
# 
# # Realizar la solicitud
# respuesta <- GET(url, query = query)
# 
# # Guardar resultado
# writeBin(content(respuesta, "raw"), "posiciones_recorrido_geom.json")
# 
# # Leer como sf
# posiciones_recorrido <- st_read("posiciones_recorrido_geom.json", quiet = TRUE)
# 
# # Ver columnas
# names(posiciones_recorrido)
# 


-----------------
#   
# #   
# #   📍 Título: V_DF_PROM_LLENADO_CONTENEDORES
# # 📝 Descripción: No especificada, pero por el nombre, representa claramente el promedio de llenado de los contenedores de residuos de Montevideo.
# # 🌍 CRS: EPSG:32721
# 
#   # URL del servidor público
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Armar la consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_PROM_LLENADO_CONTENEDORES",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # opcional para test
# )
# 
# # Ejecutar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar el archivo temporalmente
# writeBin(content(respuesta, "raw"), "prom_llenado_contenedores.json")
# 
# # Leer como objeto espacial
# prom_llenado <- st_read("prom_llenado_contenedores.json", quiet = TRUE)


------------
# #   
# #   📍 Título: V_DF_RUTAS_RECORRIDO
# # 📝 Descripción: No provista explícitamente, pero por el nombre se deduce que representa las rutas planificadas de los recorridos de recolección de residuos.
# # 🌍 CRS: EPSG:32721 (UTM zona 21 Sur)
# # 🔓 Acceso público: Servidor http://geoserver.montevideo.gub.uy/geoserver/wfs
#   
#   # URL base del servidor GeoServer
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros WFS
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_RUTAS_RECORRIDO",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # limitar para prueba
# )
# 
# # Ejecutar consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar resultado temporalmente
# writeBin(content(respuesta, "raw"), "rutas_recorrido.json")
# 
# # Leer en R como objeto espacial
# rutas2 <- st_read("rutas_recorrido.json", quiet = TRUE)
# 
# 


-------------
# 
#   📍 Título: Zonas de recolección por turno (geométricas)
# 📝 Descripción: No se especifica, pero por el nombre, representa zonas geográficas de recolección diferenciadas por turno (matutino, vespertino, nocturno, etc.)
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs
# 
#   # URL del servidor GeoServer
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Consulta con parámetros WFS
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_ZONAS_REC_TURNO_GEOM",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #maxFeatures = 100  # opcional
# )
# 
# # Hacer la consulta GET
# respuesta <- GET(url, query = query)
# 
# # Guardar temporalmente
# writeBin(content(respuesta, "raw"), "zonas_rec_turno.json")
# 
# # Leer con sf
# zonas_turno <- st_read("zonas_rec_turno.json", quiet = TRUE)
# 
# 
# 
# # Carga la librería
# library(writexl)
# 
# # Supongamos que tu df se llama df
# write_xlsx(zonas_turno, path = "mi_tabla.xlsx")

------------
#   
#   
#   📍 Título: Zona de Recorrido Vigente
# 📝 Descripción: Aunque no tiene abstract, por el nombre parece representar las zonas actualmente vigentes de recolección de residuos, probablemente agrupadas por recorrido.
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs
#   
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Armar parámetros de consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_DF_ZONA_RECORRIDO_VIGENTE",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json",
#   maxFeatures = 100  # opcional
# )
# 
# # Ejecutar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar resultado temporalmente
# writeBin(content(respuesta, "raw"), "zona_recorrido_vigente.json")
# 
# # Leer con sf
# zona_vigente <- st_read("zona_recorrido_vigente.json", quiet = TRUE)

-----
  
#   Título: Reclamos del Sistema Único de Reclamos (SUR)
# 📝 Descripción: Incluye todos los reclamos con ubicación, que están abiertos o cerrados en los últimos 3 meses.
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs
  
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de la consulta WFS
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_RE_RECLAMOS",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # para prueba rápida
# )
# 
# # Ejecutar consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar resultado
# writeBin(content(respuesta, "raw"), "reclamos_sur.json")
# 
# # Leer en R como objeto espacial
# reclamos <- st_read("reclamos_sur.json", quiet = TRUE)



---------------
#   
#   📍 Título: Reclamos del área Limpieza (SUR)
# 📝 Descripción: Reclamos del Sistema Único de Reclamos (SUR) relacionados con Limpieza, tanto abiertos como cerrados en los últimos 3 meses, provenientes de la vista v_re_reclamos_limp_portal.
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs
  
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Armar consulta
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "imm:V_RE_RECLAMOS_LIMP_PORTAL",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
  # ,
  # maxFeatures = 10000  # para prueba rápida
)

# Ejecutar la consulta
respuesta <- GET(url, query = query)

# Guardar contenido
writeBin(content(respuesta, "raw"), "reclamos_limpieza.json")

# Leer con sf
reclamos_limp <- st_read("reclamos_limpieza.json", quiet = TRUE)

reclamos_limp$FECHA_INGRESO_RECLAMO <- as.Date(
  reclamos_limp$FECHA_INGRESO_RECLAMO,
  format = "%d/%m/%Y"
)

reclamos_limp_traslado <- reclamos_limp %>% 
  filter(DESC_TIPO_PROBLEMA == "Solicitar traslado de contenedor")

----
# #   
# #   📍 Título: V_RE_RECLAMOS_FID
# # 📝 Descripción: No tiene Abstract, pero por su nombre, parece una vista especial de reclamos del SUR con un identificador único (FID → Feature ID), posiblemente usada para seguimiento, vinculación o auditoría espacial.
# # 🌍 CRS: EPSG:32721
# # 🔓 Servidor WFS público: http://geoserver.montevideo.gub.uy/geoserver/wfs
#   
#   # URL base
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros de consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_RE_RECLAMOS_FID",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   #,
#   #maxFeatures = 100  # opcional para prueba
# )
# 
# # Ejecutar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar contenido en archivo temporal
# writeBin(content(respuesta, "raw"), "reclamos_fid.json")
# 
# # Leer con sf
# reclamos_fid <- st_read("reclamos_fid.json", quiet = TRUE)
# 
# 
# reclamos_limp$FECHA_INGRESO_RECLAMO <- as.Date(reclamos_limp$FECHA_INGRESO_RECLAMO, format = "%d/%m/%Y")
# reclamos_limp$FECHA_DESDE_EN_ESTADO <- as.Date(reclamos_limp$FECHA_DESDE_EN_ESTADO, format = "%d/%m/%Y")
# 
# 
# library(dplyr)
# 
# asd <- reclamos_limp %>% 
#   filter(DESC_TIPO_PROBLEMA == "Solicitar traslado de contenedor") %>% 
#   filter(FECHA_INGRESO_RECLAMO > "2025-05-01") %>% 
#   filter(NUMERO_RECLAMO == "52818325")
# 
# # 1. Confirmamos que tiene CRS (si no lo tiene, lo asignamos primero)
# # Si sabés que es EPSG:32721 (UTM zona 21 Sur)
# st_crs(asd) <- 32721
# 
# # 2. Transformamos a WGS84 (lat/lon)
# asd_wgs84 <- st_transform(asd, crs = 4326)
# 
# # 3. Creamos el mapa
# leaflet(asd_wgs84) %>%
#   addTiles() %>%
#   addCircleMarkers(radius = 5,
#                    color = "blue",
#                    fillOpacity = 0.7,
#                    label = ~paste("Reclamo:", NUMERO_RECLAMO,
#                                   "<br>Tipo:", DESC_TIPO_PROBLEMA,
#                                   "<br>Estado:", DESC_ESTADO))


library(httr)
library(sf)

# 1) Parámetros WFS
url   <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "imm:V_DF_POSICIONES_MAPAWEB2_GEOM",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 2) Descargar con barra de progreso
cat("🔽 Iniciando descarga de la capa...\n")
resp <- GET(url, query = query, progress())
stop_for_status(resp)
cat("✅ Descarga completada. Guardando en disco...\n")

writeBin(content(resp, "raw"), "contenedores.json")
cat("📝 Archivo 'contenedores.json' generado.\n\n")

# 3) Leer con st_read y ver el mensaje de lectura
cat("📂 Leyendo el GeoJSON con sf::st_read() (quiet = FALSE)...\n")
contenedores <- st_read("contenedores.json", quiet = FALSE)
cat("✅ Lectura completada.\n\n")

# 4) Explorar un poco
cat("📊 Columnas disponibles:\n")
print(names(contenedores))
cat("\n📈 Primeras 5 filas:\n")
print(head(contenedores, 5))




# 1. Cargar librerías
library(httr)
library(sf)

# 2. URL base del WFS
url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 3. Parámetros de consulta para la capa analisisdatos:ad_lim_recorridos
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "analisisdatos:ad_lim_recorridos",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 4. Hacer la consulta (en este caso no se requiere autenticación)
respuesta <- GET(url, query = query)

# 5. Guardar el GeoJSON en disco
writeBin(content(respuesta, "raw"), "ad_lim_recorridos.json")

# 6. Leer el GeoJSON como un objeto sf
lim_recorridos <- st_read("ad_lim_recorridos.json")


--------------------
  
  
  # 1. URL base del WFS
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 2. Par�metros para la capa lim_capa_contenedoresinactivos
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "analisisdatos:lim_capa_contenedoresinactivos",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 3. Hacer la consulta
respuesta <- GET(url, query = query)

# 4. Guardar GeoJSON localmente
writeBin(content(respuesta, "raw"), "contenedores_inactivos.json")

# 5. Leer como objeto sf
cont_inactivos <- st_read("contenedores_inactivos.json")

--------------------
  
  
  # URL del WFS
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Parámetros para la capa lim_capa_ultlevantes
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "analisisdatos:lim_capa_ultlevantes",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 1) Hacer la petición
respuesta <- GET(url, query = query)

# 2) Guardar como GeoJSON
writeBin(content(respuesta, "raw"), "ultlevantes.json")

# 3) Leer con sf
ultlevantes <- st_read("ultlevantes.json")

--------------
  ##################################################################################  
######################### ULTIMO LEVANTE #########################################  
##################################################################################  
  
  # 1. Definir URL del WFS
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 2. Parámetros para v_lim_ultlevantes
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "analisisdatos:v_lim_ultlevantes",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 3. Hacer la petición
respuesta <- GET(url, query = query)

# 4. Guardar GeoJSON temporalmente
writeBin(content(respuesta, "raw"), "v_lim_ultlevantes.json")

# 5. Leer como objeto sf
v_ultlevantes <- st_read("v_lim_ultlevantes.json")


##################################################################################  
##################################################################################  
##################################################################################  
  
# 1. URL del WFS
url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 2. Parámetros para la capa imm:v_gce_basurales
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "imm:v_gce_basurales",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 3. Ejecutar la petición
respuesta <- GET(url, query = query)

# 4. Guardar el GeoJSON
writeBin(content(respuesta, "raw"), "gce_basurales.json")

# 5. Leer con sf
gce_basurales <- st_read("gce_basurales.json")

----------------------
  # 1. URL del WFS
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 2. Parámetros para la capa imm:v_gce_basurales
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "analisisdatos:v_ad_lim_recorridos",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 3. Ejecutar la petición
respuesta <- GET(url, query = query)

# 4. Guardar el GeoJSON
writeBin(content(respuesta, "raw"), "lim_recorridos.json")

# 5. Leer con sf
lim_recorridos <- st_read("lim_recorridos.json")



analisisdatos:v_ad_lim_recorridos

--*------------
  
  # 1. URL del WFS
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# 2. Parámetros para la capa V_DF_PROM_LLENADO_CONTENEDORES
query <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = "imm:V_DF_PROM_LLENADO_CONTENEDORES",
  srsname      = "EPSG:32721",
  outputFormat = "application/json"
)

# 3. Ejecutar la petición
respuesta <- GET(url, query = query)

# 4. Guardar el GeoJSON temporalmente
writeBin(content(respuesta, "raw"), "prom_llenado_contenedores.json")

# 5. Leer como objeto sf
prom_llenado <- st_read("prom_llenado_contenedores.json")



----------
  # URLs tal cual vienen en tu JS
  containersUrl <- paste0(
    "https://geoserver-ed.imm.gub.uy/geoserver/wfs?acceptversions=2.0.0",
    "&SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0",
    "&TYPENAMES=imm:spaa_posiciones_recorrido_print",
    "&STARTINDEX=0&COUNT=1000000",
    "&SRSNAME=urn:ogc:def:crs:EPSG::32721",
    "&outputFormat=application/json"
  )

circuitosUrl <- paste0(
  "https://geoserver-ed.imm.gub.uy/geoserver/imm/ows",
  "?service=WFS&version=1.0.0",
  "&request=GetFeature",
  "&typeName=imm:V_DF_ZONA_RECORRIDO_GEOM",
  "&outputFormat=application/json"
)

# Lee directamente la GeoJSON como un objeto sf
containers_sf <- st_read(containersUrl)
circuitos_sf  <- st_read(circuitosUrl)


# 1) Traer el JSON crudo
url_estado <- "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/visualizador/contenedores/estado"
resp       <- GET(url_estado)
stop_for_status(resp)
txt        <- content(resp, "text", encoding = "UTF-8")

# 2) Parsearlo como lista sin simplificar
raw_data <- fromJSON(txt, simplifyVector = FALSE)

# 3) Echar un vistazo a las keys de la primera feature
 names(raw_data$features[[1]])  # debería mostrar: "type", "geometry", "properties"

# 4) Extraer solo la parte "properties" de cada feature
props_list <- lapply(raw_data$features, `[[`, "properties")

props_clean <- lapply(props_list, function(x) {
  is_null <- vapply(x, is.null, logical(1))
  x[is_null] <- NA
  x
})

# 3) Combínalo en un solo data.frame
df <- do.call(rbind.data.frame, props_clean)

# 5) Ajusta tipos si lo necesitas
df$porcentajellenado    <- as.numeric(df$porcentajellenado)
df$UNA                  <- as.numeric(df$UNA)
df$estaProgramado       <- as.logical(df$estaProgramado)
df$contenedorGid        <- as.integer(df$contenedorGid)









library(blastula)

# 2) Componer el correo
email <- compose_email(
  body = md("
  ¡Hola!\n
  Este es un correo de prueba enviado desde R usando Gmail SMTP.
  ")
)

# 3) Guardar la contraseña en variable de entorno
Sys.setenv(SMTP_PASSWORD = "xxhw kutm sbnp yfgt")

# 4) Enviar con creds_envvar()
smtp_send(
  email       = email,
  from        = "respaldo.liotti2@gmail.com",
  to          = "nicolas.liotti@imm.gub.uy",
  subject     = "Prueba SMTP Gmail en R",
  credentials = creds_envvar(
    user     = "respaldo.liotti2@gmail.com",
    provider = "gmail",
    host     = "smtp.gmail.com",
    port     = 587,
    use_ssl  = TRUE
  )
)















#https://desa-geoserver.imm.gub.uy/geoserver/imm/ows?service=WFS&version=1.0.0&request=GetCapabilities
# https://desa-geoserver.imm.gub.uy/geoserver/analisisdatos/ows?service=WFS&version=1.0.0&request=GetCapabilities
#https://geoserver-ed.imm.gub.uy/geoserver/wfs?request=GetCapabilities
# https://desa-geoserver.imm.gub.uy/geoserver/imm/ows?




caps_url <- paste0(
  "https://desa-geoserver.imm.gub.uy/geoserver/analisisdatos/ows?",
  "service=WFS&",
  "version=1.0.0&",
  "request=GetCapabilities"
)


library(sf)

caps_url <- "https://desa-geoserver.imm.gub.uy/geoserver/analisisdatos/ows?service=WFS&version=1.0.0&request=GetCapabilities"

# Esto te mostrará en consola todos los feature types disponibles, con su nombre y CRS
st_layers(caps_url)




# 1) Escoge el layer que quieras, por ejemplo:
layer_seleccionado <- "analisisdatos:v_lim_ultlevantes4"

# 2) Construye la URL GetFeature con ese typeName
wfs_url <- paste0(
  "https://desa-geoserver.imm.gub.uy/geoserver/imm/ows?",
  "service=WFS&",
  "version=1.0.0&",
  "request=GetFeature&",
  "typeName=", layer_seleccionado, "&",
  "outputFormat=application/json"
)

# 3) Llama a st_read() para bajarlo como sf
sf_layer <- st_read(wfs_url)

# 4) Verifica la estructura (sf es a la vez un data.frame con geometría)
print(sf_layer)      # muestra primeras filas y el campo de geometría
str(sf_layer)        # muestra columnas, tipos y la columna geométrica

# 5) Si lo quieres puramente como data.frame (sin geometría), puedes hacer:
df_layer <- st_set_geometry(sf_layer, NULL)
head(df_layer)       # ves solo atributos tabulares


Driver: WFS 
Available layers:
  layer_name geometry_type features fields              crs_name
1            analisisdatos:lim_capa_ultlevantes                  11285      9                WGS 84
2               analisisdatos:ad_lim_recorridos                    142      4                WGS 84
3              analisisdatos:ad_lim_recorridos2                      0      0 WGS 84 / UTM zone 21S
4                   analisisdatos:ad_municipios                      8      2 WGS 84 / UTM zone 21S
5   analisisdatos:com_reclamos_fuera_contenedor                   3685     10                WGS 84
6  analisisdatos:com_reclamos_fuera_contenedor2                      0      0                WGS 84
7  analisisdatos:lim_capa_contenedoresinactivos                    522      8                WGS 84
8        analisisdatos:lim_capa_ultlevantes_5am                 227791     10                WGS 84
9             analisisdatos:oam_areas_liberadas                      0      0                WGS 84
10              analisisdatos:oam_asentamientos                    439     16 WGS 84 / UTM zone 21S
11     analisisdatos:oam_lim_levantes_historico         Point   300000      8 WGS 84 / UTM zone 21S
12              analisisdatos:oam_sur_problemas                  32919     16 WGS 84 / UTM zone 21S
13        analisisdatos:omo_eh2016_sec_censales       Polygon       26      2                WGS 84
14                 analisisdatos:omo_siniestros         Point    99266     24                WGS 84
15                analisisdatos:sur_mapa_calor2                   1500     20                WGS 84
16            analisisdatos:v_ad_lim_recorridos                    144      6 WGS 84 / UTM zone 21S
17               analisisdatos:v_com_anio_movil         Point   300000      3                WGS 84
18              analisisdatos:v_lim_ultlevantes                  10982     10 WGS 84 / UTM zone 21S
19             analisisdatos:v_lim_ultlevantes2                  10464     10 WGS 84 / UTM zone 21S
20             analisisdatos:v_lim_ultlevantes3                  12078     10                WGS 84
21             analisisdatos:v_lim_ultlevantes4                  12078     11                WGS 84
22     analisisdatos:v_oam_areaslib_areatrabajo         Point      508     10                WGS 84
23             analisisdatos:v_oam_sur_lim_g621                  10378     16 WGS 84 / UTM zone 21S
24             analisisdatos:v_oam_sur_lim_g622                   8142     16 WGS 84 / UTM zone 21S
25             analisisdatos:v_oam_sur_lim_g623                   4939     16 WGS 84 / UTM zone 21S
26       analisisdatos:v_sime_parcelas_exp_insp                  41188      9 WGS 84 / UTM zone 21S









library(httr)
library(jsonlite)

# 1. Definís tu usuario y contraseña (o token). 
usuario <- "im4445285"
clave   <- "Nico1919*"

# 2. Hacés la llamada:
resp <- GET(
  url = "https://desa-geoserver.imm.gub.uy/geoserver/analisisdatos/ows?service=WFS&version=1.0.0&request=GetCapabilities",
  authenticate(usuario, clave, type = "basic")
)
stop_for_status(resp)  # detiene si no respondió 200 OK

texto_json <- content(resp, as = "text", encoding = "UTF-8")
datos      <- fromJSON(texto_json, simplifyDataFrame = TRUE)

# Ahora 'datos' es un data.frame (o lista) con la información.
head(datos)


# 1) Instala y carga xml2 (si no lo tienes ya)
install.packages("xml2")
library(xml2)

# 2) Define la URL de GetCapabilities
caps_url <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs?service=WFS&version=1.0.0&request=GetCapabilities"

# 3) Lee el XML completo
xml_caps <- read_xml(caps_url)

# 4) Busca todos los nodos <FeatureType>/<Name>
name_nodes <- xml_find_all(xml_caps, ".//FeatureType/Name")

# 5) Extrae el texto de cada uno: son los typeName completos
type_names <- xml_text(name_nodes)

# 6) Muéstralos en consola
print(type_names)




library(sf)

# 1) URL correcta para GetCapabilities
caps_url <- "https://geoserver-ed.imm.gub.uy/geoserver/ows?service=WFS&version=1.0.0&request=GetCapabilities"

# 2) st_layers() ya se encarga de parsear el XML con namespaces y mostrar cada layer
st_layers(caps_url)



library(dplyr)
library(lubridate)












# 
# #---------------------------------------------------------------
# # Función: guardar_posiciones
# #
# # Parámetros:
# #   consulta      : el string con el nombre de la consulta 
# #                   (p.ej. "dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO")
# #   nombre_archivo: nombre base para el archivo RDS donde se guardará 
# #                   la versión procesada (p.ej. "posiciones_historico")
# #
# # Comportamiento:
# #   1) Si "<nombre_archivo>.rds" NO existe:
# #       - Descarga y procesa TODO.
# #       - Añade Fecha_agregado = Sys.Date() a cada fila.
# #       - Guarda el dataframe completo en "<nombre_archivo>.rds".
# #       - Devuelve ese dataframe completo.
# #
# #   2) Si "<nombre_archivo>.rds" ya existe:
# #       - Lee la versión anterior desde disco (df_anterior).
# #       - Descarga y procesa TODO de nuevo (df_actual).
# #       - Detecta filas nuevas (anti_join por GID).
# #           · A los registros nuevos les asigna Fecha_agregado = Sys.Date().
# #       - Concatena df_anterior + df_nuevos (si los hay).
# #       - **Guarda siempre** el RDS resultante (incluso si no hubo registros nuevos).
# #       - Devuelve el dataframe completo actualizado.
# #---------------------------------------------------------------
# actualizar_posiciones_historico_guardado_forzoso <- function(consulta, nombre_archivo) {
#   # 1) Ruta al archivo RDS local
#   archivo_rds <- paste0(nombre_archivo, ".rds")
#   
#   # 2) Función interna para descargar y aplicar tu pipeline original,
#   #    usando mutate(...) en lugar de rows_update()
#   procesar_df_completo <- function() {
#     # 2.1) Descargar crudo desde el servidor
#     df <- funcion_obtener_df_DFR(consulta, nombre_archivo)
#     
#     # 2.2) Corregir años "00xx" → "20xx" y convertir a Date
#     df <- df %>%
#       mutate(
#         FECHA_DESDE = if_else(
#           substr(as.character(FECHA_DESDE), 1, 2) == "00",
#           sub("^00", "20", as.character(FECHA_DESDE)),
#           as.character(FECHA_DESDE)
#         ),
#         FECHA_HASTA = if_else(
#           substr(as.character(FECHA_HASTA), 1, 2) == "00",
#           sub("^00", "20", as.character(FECHA_HASTA)),
#           as.character(FECHA_HASTA)
#         ),
#         FECHA_DESDE = as.Date(FECHA_DESDE, format = "%Y-%m-%d"),
#         FECHA_HASTA  = as.Date(FECHA_HASTA,  format = "%Y-%m-%d")
#       ) %>%
#       # 2.3) Convertir FCREA y FACT a POSIXct
#       mutate(
#         FCREA = ymd_hms(FCREA),
#         FACT  = ymd_hms(FACT)
#       )
#     
#     # 2.4) Filtrar por prefix y número extraído de COD_RECORRIDO
#     df <- df %>%
#       mutate(
#         prefix = sub("^(.*?)_.*$", "\\1", COD_RECORRIDO),
#         numero = as.numeric(sub(".*_(\\d+)$", "\\1", COD_RECORRIDO))
#       ) %>%
#       filter(
#         prefix %in% c("A","C","D","E","F","G","CH"),
#         numero >= 100
#       ) %>%
#       select(-prefix, -numero)
#     
#     # 2.5) Corregir FECHA_HASTA “mal” si es > hoy, usando mutate() + if_else()
#     hoy <- Sys.Date()
#     df <- df %>%
#       mutate(
#         FECHA_HASTA = if_else(
#           FECHA_HASTA > hoy,
#           as.Date(FACT),   # reemplazo por la fecha de FACT
#           FECHA_HASTA
#         )
#       )
#     
#     return(df)
#   }
#   
#   # 3) Si NO existe el RDS: primera descarga + guardado
#   if (!file.exists(archivo_rds)) {
#     message("No se encontró '", archivo_rds, "'. Descargando y procesando TODO por primera vez...")
#     
#     df_completo <- procesar_df_completo()
#     # 3.1) Asignar Fecha_agregado = Sys.Date() a todas las filas
#     df_completo <- df_completo %>%
#       mutate(Fecha_agregado = Sys.Date())
#     
#     # 3.2) Guardar el dataframe completo en disco
#     saveRDS(df_completo, archivo_rds)
#     
#     # 3.3) Devolver el dataframe completo
#     return(df_completo)
#   }
#   
#   # 4) Si el RDS ya existe: siempre descargar, procesar y guardar
#   message("El archivo existe. Leyendo versión previa ('", archivo_rds, "')...")
#   df_anterior <- readRDS(archivo_rds)
#   
#   message("Descargando y procesando la versión actual nuevamente...")
#   df_actual <- procesar_df_completo()
#   
#   # 4.1) Detectar filas nuevas por GID
#   df_nuevos <- df_actual %>%
#     anti_join(df_anterior, by = "GID")
#   
#   # 4.2) Si hay filas nuevas, asignar Fecha_agregado = Sys.Date()
#   if (nrow(df_nuevos) > 0) {
#     df_nuevos <- df_nuevos %>%
#       mutate(Fecha_agregado = Sys.Date())
#     message(nrow(df_nuevos), " registros nuevos encontrados.")
#   } else {
#     message("No se encontraron registros nuevos.")
#   }
#   
#   # 4.3) Concatenar df_anterior + df_nuevos (si los hay)
#   #      Para los existentes, conservamos su Fecha_agregado original (si existía).
#   #      Si df_nuevos está vacío, bind_rows() no altera nada.
#   df_completo_actualizado <- bind_rows(df_anterior, df_nuevos)
#   
#   # 4.4) **GUARDADO FORZOSO**: siempre sobrescribimos el RDS
#   saveRDS(df_completo_actualizado, archivo_rds)
#   message("Archivo '", archivo_rds, "' guardado (forzoso) con ",
#           nrow(df_completo_actualizado), " filas totales.")
#   
#   # 4.5) Devolver siempre el dataframe completo actualizado
#   return(df_completo_actualizado)
# }
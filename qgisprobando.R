library(httr)
library(sf)
library(dplyr)





# ### mapa
# 
# library(leaflet)
# 
# ##### FUNCION QUE DIBUJA UN SOLO CIRCUITO
# 
# drawZonaLeaflet <- function(zona_df, fila = 1, tile_provider = "OpenStreetMap") {
#   # zona_df: un sf con columna geometry y CRS definido
#   # fila: índice de la fila que quieres dibujar
#   # tile_provider: nombre de proveedor de tiles de leaflet
#   
#   # 1. Extraer la matriz de coordenadas
#   pts <- zona_df$geometry[[fila]][[1]]
#   
#   # 2. Cerrar el polígono si hiciera falta
#   if (!all(pts[1, ] == pts[nrow(pts), ])) {
#     pts <- rbind(pts, pts[1, ])
#   }
#   
#   # 3. Crear un objeto sf POLYGON en CRS original
#   poly_sfc <- st_sfc(st_polygon(list(pts)), crs = st_crs(zona_df))
#   poly_sf  <- st_sf(data.frame(id = zona_df$id[fila]), geometry = poly_sfc)
#   
#   # 4. Reproyectar a lon/lat (EPSG:4326) para leaflet
#   poly_ll <- st_transform(poly_sf, 4326)
#   
#   # 5. Construir y devolver el mapa leaflet
#   leaflet(poly_ll) %>%
#     {
#       if (tolower(tile_provider) == "cartodb") {
#         addProviderTiles(., "CartoDB.Positron")
#       } else if (tolower(tile_provider) == "stamen") {
#         addProviderTiles(., "Stamen.TonerLite")
#       } else {
#         addTiles(.)
#       }
#     } %>%
#     addPolygons(
#       color       = "darkgreen",
#       weight      = 2,
#       fillOpacity = 0.3,
#       popup       = ~paste0("<strong>ID:</strong> ", id)
#     ) %>%
#     addLegend(
#       position = "bottomright",
#       colors   = "darkgreen",
#       labels   = paste("Zona ID", zona_df$id[fila]),
#       title    = "Polígono"
#     )
# }
# 
# drawZonasLeaflet_global <- function(zona_df, tile_provider = "OpenStreetMap") {
#   # 1. Reproyectar todo a lon/lat
#   zonas_ll <- sf::st_transform(zona_df, 4326)
#   
#   # 2. Iniciar el mapa con el proveedor de tiles
#   mapa <- switch(
#     tolower(tile_provider),
#     cartodb = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("CartoDB.Positron"),
#     stamen  = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("Stamen.TonerLite"),
#     leaflet::leaflet(zonas_ll) %>% leaflet::addTiles()
#   )
#   
#   # 3. Añadir polígonos y una sola leyenda
#   mapa %>%
#     leaflet::addPolygons(
#       color       = "darkgreen",
#       weight      = 2,
#       fillOpacity = 0.3,
#       popup       = ~paste0("<strong>ID:</strong> ", id)
#     ) %>%
#     leaflet::addLegend(
#       position = "bottomright",
#       colors   = "darkgreen",
#       labels   = "Zonas",
#       title    = "Polígonos"
#     )
# }
# 
# mapa <- drawZonasLeaflet_global(zona, tile_provider = "cartodb")
# 
# # Y para visualizarlo en RStudio:
# mapa
# 
# 
# ### para exportarlo.
# zona_filtrada <- zona %>%
#   select(GID,COD_RECORRIDO,MUNICIPIO,geometry)
# 
# # Con esto GDAL convertirá la geometría a WKT automáticamente
# st_write(
#   obj           = zona_filtrada,
#   dsn           = "salida_gdal.csv",
#   driver        = "CSV",
#   layer_options = "GEOMETRY=AS_WKT"
# )
# 
# 
# 
# 
# drawPolygonsLeaflet <- function(sf_df,
#                                 tile_provider = c("OpenStreetMap", "CartoDB", "Stamen"),
#                                 color        = "darkgreen",
#                                 weight       = 2,
#                                 fillOpacity  = 0.3) {
#   tile_provider <- match.arg(tile_provider)
#   
#   # 1. Asegurar que es POLYGON/MULTIPOLYGON
#   if (!any(grepl("POLYGON", sf::st_geometry_type(sf_df)))) {
#     stop("El objeto sf no contiene geometrías de tipo POLYGON o MULTIPOLYGON.")
#   }
#   
#   # 2. Reproyectar a lon/lat (EPSG:4326) para leaflet
#   sf_ll <- sf::st_transform(sf_df, 4326)
#   
#   # 3. Crear base de leaflet y añadir tiles
#   mapa <- leaflet(sf_ll)
#   if (tile_provider == "CartoDB") {
#     mapa <- addProviderTiles(mapa, "CartoDB.Positron")
#   } else if (tile_provider == "Stamen") {
#     mapa <- addProviderTiles(mapa, "Stamen.TonerLite")
#   } else {
#     mapa <- addTiles(mapa)
#   }
#   
#   # 4. Añadir polígonos sin ningún popup ni leyenda
#   mapa <- mapa %>%
#     addPolygons(
#       color       = color,
#       weight      = weight,
#       fillOpacity = fillOpacity
#     )
#   
#   # 5. Devolver el mapa
#   mapa
# }
# 
# mapa_zonas <- drawPolygonsLeaflet(zona, tile_provider = "CartoDB")
# mapa_zonas  # lo despliega en RStudio o tu navegador
--------------------
#   
#   
# 
# ### Dibujar rutas
# 
# # Función para graficar una sola ruta (LINESTRING) en leaflet
# drawRutaLeaflet <- function(rutas_df, fila = 1, tile_provider = c("OpenStreetMap", "CartoDB", "Stamen")) {
#   tile_provider <- match.arg(tile_provider)
#   
#   # 1. Tomar solo la fila indicada
#   ruta_sf <- rutas_df[fila, ]
#   
#   # 2. Reproyectar a WGS84 (lon/lat) para leaflet
#   ruta_ll <- st_transform(ruta_sf, 4326)
#   
#   # 3. Crear mapa leaflet
#   mapa <- leaflet(ruta_ll)
#   
#   # 4. Añadir proveedor de tiles
#   mapa <- switch(tile_provider,
#                  CartoDB       = addProviderTiles(mapa, "CartoDB.Positron"),
#                  Stamen        = addProviderTiles(mapa, "Stamen.TonerLite"),
#                  OpenStreetMap = addTiles(mapa)
#   )
#   
#   # 5. Dibujar la línea
#   mapa <- mapa %>%
#     addPolylines(
#       color    = "blue",
#       weight   = 3,
#       opacity  = 0.7,
#       popup    = ~paste0(
#         "<strong>ID:</strong> ", id, "<br/>",
#         "<strong>Ruta:</strong> ", NOM_RUT, "<br/>",
#         "<strong>Desde:</strong> ", FECHA_DESDE
#       )
#     ) %>%
#     addLegend(
#       position = "bottomright",
#       colors   = "blue",
#       labels   = paste("Ruta ID", ruta_sf$id),
#       title    = "Linea"
#     )
#   
#   mapa
# }
# 
# 
# 
# 
# # Función para graficar todas las rutas (LINESTRING) en leaflet
# drawAllRutasLeaflet <- function(rutas_df, tile_provider = c("OpenStreetMap", "CartoDB", "Stamen")) {
#   tile_provider <- match.arg(tile_provider)
#   
#   # 1. Reproyectar todo a WGS84 (lon/lat) para leaflet
#   rutas_ll <- st_transform(rutas_df, 4326)
#   
#   # 2. Crear el mapa
#   mapa <- leaflet(rutas_ll)
#   
#   # 3. Añadir proveedor de tiles
#   mapa <- switch(tile_provider,
#                  CartoDB       = addProviderTiles(mapa, "CartoDB.Positron"),
#                  Stamen        = addProviderTiles(mapa, "Stamen.TonerLite"),
#                  OpenStreetMap = addTiles(mapa)
#   )
#   
#   # 4. Dibujar todas las líneas de una vez
#   mapa <- mapa %>%
#     addPolylines(
#       color    = "blue",
#       weight   = 3,
#       opacity  = 0.7,
#       popup    = ~paste0(
#         "<strong>ID:</strong> ", id, "<br/>",
#         "<strong>Ruta:</strong> ", NOM_RUT, "<br/>",
#         "<strong>Desde:</strong> ", FECHA_DESDE
#       )
#     ) %>%
#     addLegend(
#       position = "bottomright",
#       colors   = "blue",
#       labels   = "Todas las rutas",
#       title    = "Lineas"
#     )
#   
#   # 5. Devolver el mapa
#   mapa
# }
# 
# # Ejemplo de uso:
# mapa_todas <- drawAllRutasLeaflet(rutas, tile_provider = "CartoDB")
# mapa_todas




#   Título: Reclamos del Sistema Único de Reclamos (SUR)
# 📝 Descripción: Incluye todos los reclamos con ubicación, que están abiertos o cerrados en los últimos 3 meses.
# 🌍 CRS: EPSG:32721
# 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs

  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"


# Parámetros de la consulta WFS
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "imm:V_RE_RECLAMOS",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
)

# Ejecutar consulta
respuesta <- GET(url, query = query)

# Guardar resultado
writeBin(content(respuesta, "raw"), "reclamos_sur.json")

# Leer en R como objeto espacial
reclamos_asd <- st_read("reclamos_sur.json", quiet = TRUE)



# BUSCAR TODAS LAS CAPAS

library(httr)
library(xml2)
library(dplyr)

wfs <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

cap <- GET(wfs, query = list(service="WFS", version="2.0.0", request="GetCapabilities"))
stop_for_status(cap)
doc <- read_xml(content(cap, "text", encoding="UTF-8"))

fts <- xml_find_all(doc, ".//*[local-name()='FeatureType']")
names <- xml_text(xml_find_first(fts, ".//*[local-name()='Name']"))
titles<- xml_text(xml_find_first(fts, ".//*[local-name()='Title']"))

layers <- tibble(Name = names, Title = titles)

# Solo del workspace 'imm'
layers_imm <- filter(layers, grepl("^imm:", Name))

layers      # todas
layers_imm  # solo imm


# ---------------
# #   
# #   📍 Título: Reclamos del área Limpieza (SUR)
# # 📝 Descripción: Reclamos del Sistema Único de Reclamos (SUR) relacionados con Limpieza, tanto abiertos como cerrados en los últimos 3 meses, provenientes de la vista v_re_reclamos_limp_portal.
# # 🌍 CRS: EPSG:32721
# # 🔓 Servidor público WFS: http://geoserver.montevideo.gub.uy/geoserver/wfs
#   
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Armar consulta
# query <- list(
#   service = "WFS",
#   version = "1.0.0",
#   request = "GetFeature",
#   typeName = "imm:V_RE_RECLAMOS_LIMP_PORTAL",
#   srsname = "EPSG:32721",
#   outputFormat = "application/json"
#   # ,
#   # maxFeatures = 10000  # para prueba rápida
# )
# 
# # Ejecutar la consulta
# respuesta <- GET(url, query = query)
# 
# # Guardar contenido
# writeBin(content(respuesta, "raw"), "reclamos_limpieza.json")
# 
# # Leer con sf
# reclamos_limp <- st_read("reclamos_limpieza.json", quiet = TRUE)
# 
# reclamos_limp$FECHA_INGRESO_RECLAMO <- as.Date(
#   reclamos_limp$FECHA_INGRESO_RECLAMO,
#   format = "%d/%m/%Y"
# )
# 
# reclamos_limp_traslado <- reclamos_limp %>% 
#   filter(DESC_TIPO_PROBLEMA == "Solicitar traslado de contenedor")

----
#
#   📍 Título: V_RE_RECLAMOS_FID
# 📝 Descripción: No tiene Abstract, pero por su nombre, parece una vista especial de reclamos del SUR con un identificador único (FID → Feature ID), posiblemente usada para seguimiento, vinculación o auditoría espacial.
# 🌍 CRS: EPSG:32721
# 🔓 Servidor WFS público: http://geoserver.montevideo.gub.uy/geoserver/wfs

  # URL base
  url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

# Parámetros de consulta
query <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typeName = "imm:V_RE_RECLAMOS_FID",
  srsname = "EPSG:32721",
  outputFormat = "application/json"
  #,
  #maxFeatures = 100  # opcional para prueba
)

# Ejecutar la consulta
respuesta <- GET(url, query = query)

# Guardar contenido en archivo temporal
writeBin(content(respuesta, "raw"), "reclamos_fid.json")

# Leer con sf
reclamos_fid <- st_read("reclamos_fid.json", quiet = TRUE)
# 
# 
reclamos_fid$FECHA_INGRESO_RECLAMO <- as.Date(reclamos_fid$FECHA_INGRESO_RECLAMO, format = "%d/%m/%Y")
reclamos_fid$FECHA_DESDE_EN_ESTADO <- as.Date(reclamos_fid$FECHA_DESDE_EN_ESTADO, format = "%d/%m/%Y")

reclamos_fid_areas <- reclamos_fid %>% 
  group_by(DESC_AREA) %>%
  summarise(total = n())

reclamos_fid_limpieza <- reclamos_fid %>%
  filter(DESC_AREA == "Limpieza") %>% 
  group_by(DESC_GRUPO,DESC_TIPOPROBLEMA) %>%
  summarise(total = n())

reclamos_final <- reclamos_fid %>% 
  filter(DESC_AREA == "Limpieza") %>% 
  filter(DESC_GRUPO == "Contenedores" | DESC_GRUPO == "Estado de los contenedores" | DESC_GRUPO == "Problema de limpieza")

residuos_fuera_delcontenedor <- reclamos_final %>% 
  filter(DESC_TIPOPROBLEMA == "Residuos fuera del contenedor")

Reclamos <- read_csv("C:/Users/im4445285/Downloads/reclamos/Reclamos.csv")

Reclamos_ver <- Reclamos %>% 
  filter(DESC_AREA == "Limpieza") %>% 
  filter(DESC_GRUPO == "Contenedores" | DESC_GRUPO == "Estado de los contenedores" | DESC_GRUPO == "Problema de limpieza")


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
# 
# 



# 
# --------------------
#   
#   
#   # 1. URL base del WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # 2. Par�metros para la capa lim_capa_contenedoresinactivos
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "analisisdatos:lim_capa_contenedoresinactivos",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 3. Hacer la consulta
# respuesta <- GET(url, query = query)
# 
# # 4. Guardar GeoJSON localmente
# writeBin(content(respuesta, "raw"), "contenedores_inactivos.json")
# 
# # 5. Leer como objeto sf
# cont_inactivos <- st_read("contenedores_inactivos.json")
# 
# --------------------
#   
#   
#   # URL del WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # Parámetros para la capa lim_capa_ultlevantes
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "analisisdatos:lim_capa_ultlevantes",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 1) Hacer la petición
# respuesta <- GET(url, query = query)
# 
# # 2) Guardar como GeoJSON
# writeBin(content(respuesta, "raw"), "ultlevantes.json")
# 
# # 3) Leer con sf
# ultlevantes <- st_read("ultlevantes.json")
# 
# --------------
#   ##################################################################################  
# ######################### ULTIMO LEVANTE #########################################  
# ##################################################################################  
#   
#   # 1. Definir URL del WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # 2. Parámetros para v_lim_ultlevantes
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "analisisdatos:v_lim_ultlevantes",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 3. Hacer la petición
# respuesta <- GET(url, query = query)
# 
# # 4. Guardar GeoJSON temporalmente
# writeBin(content(respuesta, "raw"), "v_lim_ultlevantes.json")
# 
# # 5. Leer como objeto sf
# v_ultlevantes <- st_read("v_lim_ultlevantes.json")
# 
# 
# ##################################################################################  
# ##################################################################################  
# ##################################################################################  
#   
# # 1. URL del WFS
# url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # 2. Parámetros para la capa imm:v_gce_basurales
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "imm:v_gce_basurales",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 3. Ejecutar la petición
# respuesta <- GET(url, query = query)
# 
# # 4. Guardar el GeoJSON
# writeBin(content(respuesta, "raw"), "gce_basurales.json")
# 
# # 5. Leer con sf
# gce_basurales <- st_read("gce_basurales.json")
# 
# ----------------------
#   # 1. URL del WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # 2. Parámetros para la capa imm:v_gce_basurales
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "analisisdatos:v_ad_lim_recorridos",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 3. Ejecutar la petición
# respuesta <- GET(url, query = query)
# 
# # 4. Guardar el GeoJSON
# writeBin(content(respuesta, "raw"), "lim_recorridos.json")
# 
# # 5. Leer con sf
# lim_recorridos <- st_read("lim_recorridos.json")

# 
#   # 1. URL del WFS
#   url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# 
# # 2. Parámetros para la capa V_DF_PROM_LLENADO_CONTENEDORES
# query <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = "imm:V_DF_PROM_LLENADO_CONTENEDORES",
#   srsname      = "EPSG:32721",
#   outputFormat = "application/json"
# )
# 
# # 3. Ejecutar la petición
# respuesta <- GET(url, query = query)
# 
# # 4. Guardar el GeoJSON temporalmente
# writeBin(content(respuesta, "raw"), "prom_llenado_contenedores.json")
# 
# # 5. Leer como objeto sf
# prom_llenado <- st_read("prom_llenado_contenedores.json")
# 










































# LISTAR MÁS CAPAS, DISTINTAS A LAS ANTERIORES ----

# LISTAR LAS CAPAS
dsn <- "WFS:https://montevideo.gub.uy/app/geoserver/ows?service=WFS&version=1.2.0"
ver <- st_layers(dsn)
capas <- ver$name             # mirá los nombres

# SELECCIONAR CAPA
cap <- "mapstore-tematicas:v_sig_barrios"

# IMPLEMENTARLA
base <- "https://montevideo.gub.uy/app/geoserver/ows"
url <- paste0(
  base, "?service=WFS&version=2.0.0&request=GetFeature",
  "&typeNames=", utils::URLencode(cap, reserved=TRUE),
  "&srsName=EPSG:4326&outputFormat=application/json",
  "&count=10000" # opcional
)
g2 <- read_sf(url)

# FIN - LISTAR MÁS CAPAS, DISTINTAS A LAS ANTERIORES ----





library(httr)
library(xml2)

listar_capas_wms_con_leer <- function(wms_base, wfs_base) {
  # URLs de Capabilities
  url_wms_cap <- paste0(wms_base, "?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities")
  url_wfs_cap <- paste0(wfs_base, "?service=WFS&version=1.1.0&request=GetCapabilities")
  
  # 1) Descargar y parsear WMS Capabilities
  resp_wms <- GET(url_wms_cap); stop_for_status(resp_wms)
  wms_doc  <- read_xml(content(resp_wms, "text", encoding="UTF-8"))
  
  # 2) Descargar y parsear WFS Capabilities
  resp_wfs <- GET(url_wfs_cap); stop_for_status(resp_wfs)
  wfs_doc  <- read_xml(content(resp_wfs, "text", encoding="UTF-8"))
  
  # 3) Extraer sólo los typeName de WFS (los que sí puedes leer con st_read)
  wfs_names <- xml_text(xml_find_all(
    wfs_doc,
    ".//*[local-name()='FeatureTypeList']/*[local-name()='FeatureType']/*[local-name()='Name']"
  ))
  
  # 4) Encontrar todas las capas WMS que tengan un <Name>, usando local-name()
  layer_nodes <- xml_find_all(
    wms_doc,
    ".//*[local-name()='Layer'][*/*[local-name()='Name']]"
  )
  
  resultado <- lapply(layer_nodes, function(nd) {
    nm <- xml_text(xml_find_first(nd, ".//*[local-name()='Name'][1]"))
    tl <- xml_text(xml_find_first(nd, ".//*[local-name()='Title'][1]"))
    ab <- xml_text(xml_find_first(nd, ".//*[local-name()='Abstract'][1]"))
    if (ab == "") ab <- NA_character_
    
    # 5) Decidir instrucción de lectura
    if (nm %in% wfs_names) {
      instr <- sprintf(
        'sf::st_read("WFS:%s", layer="%s")',
        wfs_base, nm
      )
    } else {
      instr <- sprintf(
        '%s?service=WMS&version=1.3.0&request=GetMap&layers=%s&styles=&crs=EPSG:32721&bbox=<xmin>,<ymin>,<xmax>,<ymax>&width=800&height=600&format=image/png',
        wms_base, nm
      )
    }
    
    data.frame(
      Name     = nm,
      Title    = tl,
      Abstract = ab,
      Leer     = instr,
      stringsAsFactors = FALSE
    )
  })
  
  capas_df <- do.call(rbind, resultado)
  return(capas_df)
}

# —————————————————————————————————————————
# Ejemplo de uso
wms_base <- "http://geoserver.montevideo.gub.uy/geoserver/wms"
wfs_base <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
capas <- listar_capas_wms_con_leer(wms_base, wfs_base)
print(capas)





-------------
  # 0) Instalar geojsonsf si no lo tienes
  # install.packages("geojsonsf")
  
  # 1) Cargar librerías
  library(httr)
library(geojsonsf)
library(sf)

# 2) Parámetros del WFS
wfs_url  <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
layer    <- "analisisdatos:v_lim_ultlevantes"
params   <- list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = layer,
  outputFormat = "application/json"
)

# 3) Descargar el GeoJSON crudo
resp <- GET(wfs_url, query = params)
stop_for_status(resp)
geojson_raw <- content(resp, "raw")

# 4) Guardar a disco (opcional pero útil para cache)
file_geojson <- "leavA.json"
writeBin(geojson_raw, file_geojson)

# 5) Leer con geojsonsf para evitar el error de GDAL
redes_sf <- geojson_sf(file_geojson)



------------
  
  # install.packages(c("httr","geojsonsf","sf"))
  
  library(httr)
library(geojsonsf)
library(sf)

leer_capa_geojson <- function(wfs_url, layer, out_file = NULL) {
  # Parámetros WFS
  params <- list(
    service      = "WFS",
    version      = "1.0.0",
    request      = "GetFeature",
    typeName     = layer,
    outputFormat = "application/json"
  )
  
  # Descargar
  resp <- GET(wfs_url, query = params)
  stop_for_status(resp)
  geojson_raw <- content(resp, "raw")
  
  # Elegir dónde guardar
  if (is.null(out_file)) {
    tmp <- tempfile(fileext = ".json")
    writeBin(geojson_raw, tmp)
    file_geojson <- tmp
  } else {
    writeBin(geojson_raw, out_file)
    file_geojson <- out_file
  }
  cat("🔽 GeoJSON guardado en:", file_geojson, "\n")
  
  # Leer con geojsonsf
  cat("🔍 Intentando leer con geojsonsf...\n")
  sf_obj <- tryCatch({
    geojsonsf::geojson_sf(file_geojson)
  }, error = function(e) {
    stop("❌ Falló geojsonsf: ", conditionMessage(e))
  })
  
  # Resultado
  cat("✅ Capa '", layer, "' cargada: ", nrow(sf_obj), " filas y ", 
      ncol(sf_obj), " columnas.\n", sep = "")
  return(sf_obj)
}

# Ejemplo de uso:
wfs_url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
layer   <- "analisisdatos:v_lim_ultlevantes"

mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "ultlevantes.json")
View(mi_sf)


layer   <- "imm:V_DF_POSICIONES_MAPAWEB2_GEOM"
mias_sf <- leer_capa_geojson(wfs_url, layer, out_file = "contenedores.json")




library(httr)
library(xml2)

#################### OBTENER LAS CAPAS Y MOSTRARLAS - HOY 30/07/2025 HAY 279).

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


##############################################################################
#############################################################################


  
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


### arancamos aca las pruebas

# WFS GLOBAL.
wfs_url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"

## Capa de los limites de los circuitos en poligonos.
layer   <- "analisisdatos:ad_lim_recorridos"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "lim_recorridos.json")

## Capa de los limites de los circuitos en poligonos con el "ranking".
layer   <- "analisisdatos:v_ad_lim_recorridos"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "analisisdatos:v_ad_lim_recorridos.json")


## Capa de ultimos levantes. usar??".
layer   <- "analisisdatos:v_lim_ultlevantes"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "analisisdatos:v_lim_ultlevantes.json")
mi_sf <- mi_sf %>%
  mutate(
    fecha_pasaje_utc = ymd_hms(fecha_pasaje, tz = "UTC"),
    fecha_pasaje_uy  = with_tz(fecha_pasaje_utc, tzone = "America/Montevideo"),
    
    fecha_real_uy    = fecha_pasaje_uy + dhours(horas_reales),
    
    # convertir "ahora" (ej: "01:15:37") a difftime y sumárselo
    hora_ahora = hms(ahora),
    fecha_ahora_uy = fecha_pasaje_uy + hora_ahora
  )

## Capa de ferias.
layer   <- "geomatica:V_SF_FERIAS_mmap"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "geomatica:V_SF_FERIAS_mmap.json")


## Capa de direcciones.
layer   <- "geomatica:v_sig_accesos_montevideo"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "geomatica:v_sig_accesos_montevideo.json")

## Capa de esquinas
layer   <- "geomatica:v_sig_cruces_montevideo"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "geomatica:v_sig_cruces_montevideo.json")

## Capa de avenidas
layer   <- "ide:ide_v_avenidas"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "ide:ide_v_avenidas.json")

########### inicio obras

# obras lineas
# Obras planificadas en la vía pública, autorizadas por UCCRIU en el correr de los últimos 360 días. Tipo de geometría: LÍNEA
layer   <- "ide:v_redes_planif_lineasGeom_ultimoAnio"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "ide:v_redes_planif_lineasGeom_ultimoAnio.json")

# obras lineas
# Obras planificadas en la vía pública, autorizadas por UCCRIU en el correr de los últimos 360 días. Tipo de geometría: PUNTO
layer   <- "ide:v_redes_planif_puntosGeom_ultimoAnio"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "ide:v_redes_planif_puntosGeom_ultimoAnio.json")

########### fin obras

# Municipios
layer   <- "imm:sig_municipios"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:sig_municipios.json")

# otro municipio con menos cosas
layer   <- "geomatica:ide_v_sig_municipios"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "geomatica:ide_v_sig_municipios.json")

# basurales??
layer   <- "imm:gce_basurales"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:gce_basurales.json")

# Reclamos de limpieza
layer   <- "V_RE_RECLAMOS_LIMP_PORTAL"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "V_RE_RECLAMOS_LIMP_PORTAL.json")
mi_sf <- mi_sf %>%
  mutate(
    FECHA_INGRESO_RECLAMO = dmy(FECHA_INGRESO_RECLAMO),
    FECHA_DESDE_EN_ESTADO = dmy(FECHA_DESDE_EN_ESTADO)
  )

# CIRCUITOS CON DIAS Y POLIGONO DE ZONAS.
layer   <- "imm:V_DF_ZONAS_REC_TURNO_GEOM"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:V_DF_ZONAS_REC_TURNO_GEOML.json")


# CIRCUITOS CON poligonos, fecha de creacion, y gid
layer   <- "imm:V_DF_ZONA_RECORRIDO_VIGENTE"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:V_DF_ZONA_RECORRIDO_VIGENTE.json")

# PARECE SER LOS PUNTO DE ENTREGA VOLUNTARIO
layer   <- "imm:V_DF_SEL_DOM_VP_TES"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:V_DF_SEL_DOM_VP_TES.json")


# PARECE SER cooperativas (Municipio b y c?)
layer   <- "imm:V_DF_SEL_DOM_MBC"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:V_DF_SEL_DOM_MBC.json")

# LINEA DE LOS CIRCUITOS.
layer   <- "V_DF_RUTAS_RECORRIDO"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "V_DF_RUTAS_RECORRIDO.json")


### POSICIONES DE LOS CONTENEDORES CON. GID, FECHA DESDE POSICION, OBSERVACIONES, GEOMETRY (SIN DIRECCIÓN)
layer   <- "imm:V_DF_POSICIONES_RECORRIDO_GEOM"
mi_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:V_DF_POSICIONES_RECORRIDO_GEOM.json")

imm:V_DF_POSICIONES_MAPAWEB2_GEOM


# PARECE SER LOS PUNTO DE ENTREGA VOLUNTARIO
layer   <- "imm:DF_SELECTIVA_DOMICILIARIA"
mias_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:DF_SELECTIVA_DOMICILIARIA.json")











# -------------

### esto lo suplanta la funcion leer_capa_geojson, lo guardo por las dudas.

#   # 0) Instalar geojsonsf si no lo tienes
#   # install.packages("geojsonsf")
#   
#   # 1) Cargar librerías
#   library(httr)
# library(geojsonsf)
# library(sf)
# 
# # 2) Parámetros del WFS
# wfs_url  <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
# layer    <- "analisisdatos:v_lim_ultlevantes"
# params   <- list(
#   service      = "WFS",
#   version      = "1.0.0",
#   request      = "GetFeature",
#   typeName     = layer,
#   outputFormat = "application/json"
# )
# 
# # 3) Descargar el GeoJSON crudo
# resp <- GET(wfs_url, query = params)
# stop_for_status(resp)
# geojson_raw <- content(resp, "raw")
# 
# # 4) Guardar a disco (opcional pero útil para cache)
# file_geojson <- "leavA.json"
# writeBin(geojson_raw, file_geojson)
# 
# # 5) Leer con geojsonsf para evitar el error de GDAL
# redes_sf <- geojson_sf(file_geojson)
# 




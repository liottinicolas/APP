# ============================================
# 1) INSTALAR/CARGAR PAQUETES NECESARIOS
# ============================================
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("sf", quietly = TRUE))   install.packages("sf")

library(httr)
library(xml2)
library(sf)

# ============================================
# 2) PARÁMETROS DE CONEXIÓN Y AUTHENTICACIÓN
# ============================================
url_base  <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"
usuario   <- "im4445285"
contrasena<- "Nico1919*"

# ============================================
# 3) OBTENER GetCapabilities CON AUTENTICACIÓN
# ============================================
resp_caps <- GET(
  url_base,
  authenticate(usuario, contrasena),
  query = list(
    service = "WFS",
    version = "1.0.0",
    request = "GetCapabilities"
  )
)

stop_for_status(resp_caps)
caps_xml <- content(resp_caps, as = "text", encoding = "UTF-8")

# ============================================
# 4) PARSEAR EL XML DE GetCapabilities
# ============================================
doc   <- read_xml(caps_xml)
ns    <- xml_ns(doc)

# En WFS 1.0.0, los feature types aparecen bajo:
#   /WFS_Capabilities/FeatureTypeList/FeatureType/Name
#
# El prefijo para WFS suele ser 'wfs' o puede variar, así que lo capturamos con xml_ns().

# 4.1) Extraer nodos <Name> de cada <FeatureType>
ft_nodes <- xml_find_all(doc, ".//wfs:FeatureType/wfs:Name", ns = ns)
ft_names <- xml_text(ft_nodes)

cat("Feature types disponibles en el WFS:\n")
print(ft_names)

# ============================================
# 5) (OPCIONAL) LEER CADA CAPA COMO sf
# ============================================
# Si deseas “levantar” cada uno de esos feature types, puedes iterar:
#
# Por ejemplo, creamos una lista vacía para guardar cada sf:
lista_sf <- list()
for (full_name in ft_names) {
  cat("Leyendo feature type:", full_name, "...\n")
  # Construimos un WFS-URL de capa específica con autenticación embebida
  # (algunos entornos GDAL/OGR lo aceptan como parte del DSN).
  #
  # Notar que si el nombre no lleva esquema explícito,
  # podrías necesitar anteponer “dfr:” u otro workspace,
  # pero en la salida de GetCapabilities ya viene con el prefijo correcto (p.ej. “dfr:E_DF_POSICIONES_RECORRIDO”).
  dsn_layer <- paste0(
    "WFS:",
    "https://", URLEncode(usuario), ":", URLEncode(contrasena),
    "@geoserver-ed.imm.gub.uy/geoserver/wfs?",
    "service=WFS&version=1.0.0&request=GetFeature&typename=", full_name,
    "&srsname=EPSG:32721&outputFormat=application/json"
  )
  #
  # Intentamos leer con st_read(). Si falla, capturamos el error y seguimos.
  sf_obj <- tryCatch({
    st_read(dsn = dsn_layer, quiet = TRUE)
  }, error = function(e) {
    warning(sprintf("No se pudo leer %s: %s", full_name, e$message))
    return(NULL)
  })
  if (!is.null(sf_obj)) {
    lista_sf[[full_name]] <- sf_obj
  }
}
#
# Al final, 'lista_sf' contendrá un elemento por cada capa leída correctamente.
# ============================================




# ============================================
# SCRIPT COMPLETO: LISTAR TODAS LAS CAPAS DE UN WFS AUTENTICADO
# ============================================
# 1) CARGAR PAQUETES NECESARIOS
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("sf", quietly = TRUE))   install.packages("sf")

library(httr)
library(xml2)
library(sf)

# ============================================
# 2) PARÁMETROS DE CONEXIÓN Y AUTENTICACIÓN
# ============================================
url_base   <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"
usuario    <- "im4445285"
contrasena <- "Nico1919*"

# ============================================
# 3) OBTENER GetCapabilities CON AUTENTICACIÓN
# ============================================
resp_caps <- GET(
  url_base,
  authenticate(usuario, contrasena),
  query = list(
    service = "WFS",
    version = "1.0.0",
    request = "GetCapabilities"
  )
)
stop_for_status(resp_caps)
caps_xml <- content(resp_caps, as = "text", encoding = "UTF-8")

# ============================================
# 4) PARSEAR XML Y VER ESPACIOS DE NOMBRES
# ============================================
doc <- read_xml(caps_xml)
ns  <- xml_ns(doc)
cat("Espacios de nombres definidos en el XML:\n")
print(ns)

# ============================================
# 5) EXTRAER NOMBRES DE <FeatureType><Name> SIN PREFIJO
# ============================================
# Usamos local-name() para no depender del prefijo
ft_nodes <- xml_find_all(doc, "//*[local-name()='FeatureType']/*[local-name()='Name']")
ft_names <- xml_text(ft_nodes)
ft_names <- ft_names[str_detect(ft_names, "^dfr:")]

cat("Feature types disponibles en el WFS:\n")
print(ft_names)

# ============================================
# 6) (OPCIONAL) DESCARGAR CADA CAPA COMO sf
# ============================================
lista_sf <- list()
for (nombre_ft in ft_names) {
  cat("Leyendo feature type:", nombre_ft, "...\n")
  dsn <- paste0(
    "WFS:",
    "https://", URLencode(usuario), ":", URLencode(contrasena),
    "@geoserver-ed.imm.gub.uy/geoserver/wfs?",
    "service=WFS&version=1.0.0&request=GetFeature&typename=", nombre_ft,
    "&srsname=EPSG:32721&outputFormat=application/json"
  )
  sf_obj <- tryCatch({
    st_read(dsn = dsn, quiet = TRUE)
  }, error = function(e) {
    warning(sprintf("No se pudo leer '%s': %s", nombre_ft, e$message))
    return(NULL)
  })
  if (!is.null(sf_obj)) lista_sf[[nombre_ft]] <- sf_obj
}

# Si quisieras inspeccionar alguna capa descargada:
# Por ejemplo: head(lista_sf[["dfr:E_DF_POSICIONES_RECORRIDO"]])

# ============================================
# FIN DEL SCRIPT
# ============================================


library(dplyr)
library(stringr)  # para str_extract



asd <- lista_sf[["dfr:E_DF_POSICIONES_RECORRIDO"]]
hist <- lista_sf[["dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO"]]

hist_mayores_99 <- hist %>%
  mutate(
    parte_num = as.numeric(str_extract(COD_RECORRIDO, "\\d+$"))
  ) %>%
  filter(parte_num > 99) %>%
  select(-parte_num)  # opcional, si no quieres conservar esa columna auxiliar

posiciones_dia <- asd %>% 
  select(GID,COD_RECORRIDO,POSICION,FECHA_DESDE,DIRECCION,OBSERVACIONES,THE_GEOM,COD_MOTIVO_INACTIVA) %>% 
    mutate(
      Estado = case_when(
        COD_MOTIVO_INACTIVA == 0 ~ NA_character_,
        COD_MOTIVO_INACTIVA == 1 ~ "Mantenimiento",
        COD_MOTIVO_INACTIVA == 2 ~ "Sin instalar",
        COD_MOTIVO_INACTIVA == 4 ~ "Roto, espera sustitución",
        TRUE                      ~ NA_character_   # cualquier otro caso → NA
      )
    ) %>% 
  rename(gid = GID,
         Circuito = COD_RECORRIDO,
         Posicion = POSICION)










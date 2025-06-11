# Instala los paquetes si no los tienes:
# install.packages(c("httr", "xml2"))

library(httr)
library(xml2)

# 1. WMS GetCapabilities (versión corregida) ---------------------------
base_wms <- "https://geoserver-ed.imm.gub.uy/geoserver/imm/wms"
wms_params <- list(
  SERVICE = "WMS",
  VERSION = "1.1.1",
  REQUEST = "GetCapabilities"
)
wms_url <- modify_url(base_wms, query = wms_params)

resp_wms <- GET(wms_url)
if (http_error(resp_wms)) {
  stop("Error al solicitar WMS GetCapabilities: ", status_code(resp_wms))
}

# Pedimos el contenido como texto y luego parseamos a XML
texto_wms <- content(resp_wms, as = "text", encoding = "UTF-8")
cap_wms <- read_xml(texto_wms)

# Listamos todos los nombres de capa (Layer/Name)
capas_wms <- xml_find_all(cap_wms, ".//Layer/Name")
nombres_wms <- xml_text(capas_wms)
cat("Capas WMS encontradas:\n")
print(unique(nombres_wms))

# (Opcional) Guardamos el XML completo a disco
write_xml(cap_wms, "wms_GetCapabilities.xml")


# 2. WFS GetCapabilities (corregido) ------------------------------------
base_wfs <- "https://geoserver-ed.imm.gub.uy/geoserver/imm/wfs"
wfs_params <- list(
  SERVICE = "WFS",
  VERSION = "1.0.0",
  REQUEST = "GetCapabilities"
)
wfs_url <- modify_url(base_wfs, query = wfs_params)

resp_wfs <- GET(wfs_url)
if (http_error(resp_wfs)) {
  stop("Error al solicitar WFS GetCapabilities: ", status_code(resp_wfs))
}

texto_wfs <- content(resp_wfs, as = "text", encoding = "UTF-8")
cap_wfs <- read_xml(texto_wfs)

# 1. Extraer el conjunto de namespaces del documento
ns <- xml_ns(cap_wfs)

# 2. Buscar todos los nodos <FeatureType>/<Name> usando el namespace correspondiente
#    En WFS 1.0.0, el namespace por defecto suele quedar registrado con el prefijo "d1" (o similar).
feature_name_nodes <- xml_find_all(cap_wfs, ".//d1:FeatureType/d1:Name", ns)

# 3. Obtener los textos de cada nodo <Name>
feature_names <- xml_text(feature_name_nodes)

# 4. Mostrar resultados únicos
cat("FeatureType disponibles en el WFS:\n")
print(unique(feature_names))









######


# 5. REST API de GeoServer (listar workspaces y capas) -----------------
# Sustituye "tu_usuario" y "tu_contraseña" por credenciales válidas
user <- "im4445285"
pass <- "Nico1919*"

# 5.a) Listar workspaces
rest_ws_url <- "https://geoserver-ed.imm.gub.uy/geoserver/rest/workspaces.json"
resp_ws <- GET(rest_ws_url, authenticate(user, pass, type = "basic"))
if (http_error(resp_ws)) {
  warning("Error al listar workspaces (revisa credenciales o permisos): ", status_code(resp_ws))
} else {
  ws_json <- content(resp_ws, as = "parsed", encoding = "UTF-8")
  ws_list <- ws_json$workspaces$workspace
  cat("Workspaces encontrados:\n")
  for (w in ws_list) {
    cat(" -", w$name, "\n")
  }
}

# 5.b) Listar todas las capas registradas
rest_layers_url <- "https://geoserver-ed.imm.gub.uy/geoserver/rest/layers.json"
resp_layers <- GET(rest_layers_url, authenticate(user, pass, type = "basic"))
if (http_error(resp_layers)) {
  warning("Error al listar capas REST (revisa credenciales o permisos): ", status_code(resp_layers))
} else {
  layers_json <- content(resp_layers, as = "parsed", encoding = "UTF-8")
  layer_list <- layers_json$layers$layer
  cat("Capas GeoServer (REST) encontradas:\n")
  for (ly in layer_list) {
    cat(" -", ly$name, "\n")
  }
}




##################### ACA FUNCIONA PARA VER TIEMPO REAL #########################



library(httr)
library(jsonlite)  # para parsear JSON en R

# 1) define la URL base que viste en config.json
api_base <- "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1"

# 2) end point concreto para "contenedores/estado"
url_estado <- paste0(api_base, "/visualizador/contenedores/estado")

# 3) Si la API requiere token o autenticación, tal vez necesites headers. 
#    Para empezar, probemos sin auth (puede que esté protegido internamente).
resp <- GET(url_estado)

if (http_error(resp)) {
  stop("Error al solicitar estado de contenedores: ", status_code(resp))
}

# 4) parsear el JSON
texto_json <- content(resp, as = "text", encoding = "UTF-8")

datos_estado <- fromJSON(texto_json)

# Comprobamos la clase de 'datos_estado'
class(datos_estado)

ver <- datos_estado$features

  
####################################################################



pp/limpieza-gestion-operativa/assets/config/config.json
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/configuracion/motivoNoLevante
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/configuracion/condicionContenedor
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/permisos/mis-permisos
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/oficina
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/planificacion/1/rango?desde=2025-06-02&hasta=2025-06-08
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/oficina/1/turno
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/oficina/1/camion
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/oficina/1/tripulante
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/visualizador/contenedores/estado
https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/visualizador/contenedores/estad


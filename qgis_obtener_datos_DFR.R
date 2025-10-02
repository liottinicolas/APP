
# SCRIPT COMPLETO: LISTAR TODAS LAS CAPAS DE UN WFS AUTENTICADO -----

## 1) CARGAR PAQUETES NECESARIOS -----
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("sf", quietly = TRUE))   install.packages("sf")

library(httr)
library(xml2)
library(sf)
library(stringr)  # para str_extract


## 2) PARÁMETROS DE CONEXIÓN Y AUTENTICACIÓN ----

url_base   <- "https://geoserver-ed.imm.gub.uy/geoserver/wfs"
usuario    <- "im4445285"
contrasena <- "Nico1919*"

## 3) OBTENER GetCapabilities CON AUTENTICACIÓN ----

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

## 4) PARSEAR XML Y VER ESPACIOS DE NOMBRES ----

doc <- read_xml(caps_xml)
ns  <- xml_ns(doc)
cat("Espacios de nombres definidos en el XML:\n")
print(ns)

## 5) EXTRAER NOMBRES DE <FeatureType><Name> SIN PREFIJO ----

# Usamos local-name() para no depender del prefijo

ft_nodes <- xml_find_all(doc, "//*[local-name()='FeatureType']/*[local-name()='Name']")
ft_names <- xml_text(ft_nodes)
#ft_names <- ft_names[str_detect(ft_names, "^dfr:")]

cat("Feature types disponibles en el WFS:\n")
print(ft_names)


## 6) (OPCIONAL) DESCARGAR CADA CAPA COMO sf ----
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

# FIN DEL SCRIPT ---- 

# Leer las capas ----

# dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO
C_DF_POSICIONES_RECORRIDO_HISTORICO <- lista_sf[["dfr:C_DF_POSICIONES_RECORRIDO_HISTORICO"]]
C_DF_RUTAS_RECORRIDO_HISTORICO <- lista_sf[["dfr:C_DF_RUTAS_RECORRIDO_HISTORICO"]]
C_DF_ZONA_RECORRIDO_HISTORICO <- lista_sf[["dfr:C_DF_ZONA_RECORRIDO_HISTORICO"]]
C_DIRECCIONES <- lista_sf[["dfr:C_DIRECCIONES"]]
E_DF_CAP_CIRCUITOS <- lista_sf[["dfr:E_DF_CAP_CIRCUITOS"]]
E_DF_CAP_CONTENEDORES <- lista_sf[["dfr:E_DF_CAP_CONTENEDORES"]]
E_DF_CONTENEDORES_SOTERRADOS <- lista_sf[["dfr:E_DF_CONTENEDORES_SOTERRADOS"]]
E_DF_POSICIONES_RECORRIDO <- lista_sf[["dfr:E_DF_POSICIONES_RECORRIDO"]]
E_DF_POSICIONES_RECORRIDO_PL <- lista_sf[["dfr:E_DF_POSICIONES_RECORRIDO_PL"]]
E_DF_RUTAS_RECORRIDO <- lista_sf[["dfr:E_DF_RUTAS_RECORRIDO"]]
E_DF_RUTAS_RECORRIDO_PLAN <- lista_sf[["dfr:E_DF_RUTAS_RECORRIDO_PLAN"]]
E_DF_ZONA_RECORRIDO <- lista_sf[["dfr:E_DF_ZONA_RECORRIDO"]]
E_DF_ZONA_RECORRIDO_PLAN <- lista_sf[["dfr:E_DF_ZONA_RECORRIDO_PLAN"]]

# DF finales ----

zonas_recorrido <- E_DF_ZONA_RECORRIDO %>% 
  select(-gml_id,-IDAUDITORIA,-UCREA,-UACT,-ESTADO,-ULTIMO_ENVIO_SIMPLIFICA) %>% 
  rename(Circuito = COD_RECORRIDO,
         Fecha_desde = FECHA_DESDE,
         Fecha_hasta = FECHA_HASTA,
         Municipio = MUNICIPIO,
         the_geom = THE_GEOM) %>% 
  relocate(Municipio, .after = GID)





# REVISAR Funciones de dibujo de circuitos ----

## Solo un circuito o más que seleccione ----



#' Dibuja uno o varios circuitos por su COD_RECORRIDO
#'
#' @description
#' Filtra un `sf` por valores de `COD_RECORRIDO` y los renderiza en Leaflet.
#' Usa EPSG:4326 para el mapa. Soporta POLYGON/MULTIPOLYGON.
#'
#' @param zona_df `sf` con columnas `COD_RECORRIDO` y geometría válida.
#' @param codigos `character` o `numeric`. Uno o varios códigos a dibujar.
#' @param geom_col `character` o `NULL`. Nombre de la columna geométrica si no es la activa
#'   (por ej. `"THE_GEOM"`). Si `NULL`, usa la geom activa.
#' @param tile_provider `character`. `"cartodb"`, `"stamen"` o cualquier otro para `addTiles()`.
#' @return `leaflet` htmlwidget.
#' @examples
#' # st_geometry(E_DF_ZONA_RECORRIDO) <- "THE_GEOM"
#' # st_crs(E_DF_ZONA_RECORRIDO) <- 32721
#' # drawZonasPorCodigoLeaflet(E_DF_ZONA_RECORRIDO, c("B_DU_RM_CL_101","B_DU_RM_CL_102"))
drawZonasPorCodigoLeaflet <- function(zona_df, codigos,
                                      geom_col = NULL,      # nombre de la col geom si no es la activa
                                      tile_provider = "cartodb") {
  stopifnot(inherits(zona_df, "sf"))
  if (!is.null(geom_col)) {
    # si geom_col ya es sfc, actívala; si es WKT, conviértela
    if (inherits(zona_df[[geom_col]], "sfc")) {
      attr(zona_df, "sf_column") <- geom_col
    } else {
      sf::st_geometry(zona_df) <- sf::st_as_sfc(zona_df[[geom_col]], crs = sf::st_crs(zona_df))
    }
  }
  if (is.na(sf::st_crs(zona_df))) stop("Definí el CRS del sf antes de transformar.")
  if (!"Circuito" %in% names(zona_df)) stop("Falta columna 'Circuito'.")
  
  codigos <- unique(as.character(codigos))
  
  sel <- zona_df[zona_df$Circuito %in% codigos, , drop = FALSE]
  if (nrow(sel) == 0) stop("Ningún Circuito coincide con: ", paste(codigos, collapse = ", "))
  
  sel <- sf::st_make_valid(sel)
  sel_ll <- sf::st_transform(sel, 4326)
  
  # popup
  if ("GID" %in% names(sel_ll)) {
    sel_ll$popup <- sprintf("<strong>Circuito:</strong> %s<br><strong>GID:</strong> %s",
                            as.character(sel_ll$Circuito), as.character(sel_ll$GID))
  } else {
    sel_ll$popup <- sprintf("<strong>Circuito:</strong> %s", as.character(sel_ll$Circuito))
  }
  
  base <- switch(
    tolower(tile_provider),
    cartodb = leaflet::leaflet(sel_ll) |> leaflet::addProviderTiles("CartoDB.Positron"),
    stamen  = leaflet::leaflet(sel_ll) |> leaflet::addProviderTiles("Stamen.TonerLite"),
    leaflet::leaflet(sel_ll) |> leaflet::addTiles()
  )
  
  # decidir capa según geometría
  gtype <- unique(sf::st_geometry_type(sel_ll, by_geometry = TRUE))
  if (all(gtype %in% c("LINESTRING","MULTILINESTRING"))) {
    m <- base |> leaflet::addPolylines(color = "darkgreen", weight = 2, opacity = 1, popup = ~popup)
  } else {
    m <- base |> leaflet::addPolygons(color = "darkgreen", weight = 2, fillOpacity = 0.3, popup = ~popup)
  }
  
  m |> leaflet::addLegend(
    position = "bottomright",
    colors   = "darkgreen",
    labels   = paste("Circuitos:", paste(codigos, collapse = ", ")),
    title    = "Seleccionados"
  )
}

mapa_2 <- drawZonasPorCodigoLeaflet(zonas_recorrido, codigos)

## Todos los circuitos ----


#' Dibuja zonas en Leaflet con popup configurable
#'
#' @description
#' Reproyecta un `sf` al CRS EPSG:4326 y renderiza polígonos en Leaflet.
#' El popup muestra el valor de la columna indicada por `id_col`.
#'
#' @param zona_df `sf`. Data frame espacial con una columna geométrica válida.
#' @param id_col `character` (1). Nombre de la columna a mostrar en el popup.
#'   Ej.: `"GID"` o `"COD_RECORRIDO"`.
#' @param geom_col `character` o `NULL`. Si no es `NULL`, activa esa columna
#'   como geometría antes de reproyectar (p. ej. `"THE_GEOM"`).
#' @param tile_provider `character`. Base de mapas.
#'   Valores soportados: `"cartodb"`, `"stamen"` o cualquier otro ⇒ tiles por defecto.
#'   #' @param color_col character|NULL. Columna categórica para colorear cada polígono.
#'   Si NULL, usa color fijo.

#'
#' @return `leaflet` htmlwidget listo para imprimir o guardar.
#'
#' @details
#' - Requiere que `zona_df` tenga CRS definido (`sf::st_crs(zona_df)` no `NA`).
#' - No modifica datos de origen; solo crea una columna `popup` temporal.
#' - Usa EPSG:4326 por compatibilidad con Leaflet/GeoJSON.
#'
#' @examples
#' # Preparación típica:
#' # sf::st_geometry(E_DF_ZONA_RECORRIDO) <- "THE_GEOM"
#' # sf::st_crs(E_DF_ZONA_RECORRIDO) <- 32721
#'
#' # Mostrar GID:
#' # drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="GID", geom_col="THE_GEOM")
#'
#' # Mostrar COD_RECORRIDO con CartoDB:
#' # drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="COD_RECORRIDO",
#' #                         geom_col="THE_GEOM", tile_provider="cartodb")
#'
#' @importFrom sf st_transform st_crs st_geometry
#' @importFrom leaflet leaflet addTiles addProviderTiles addPolygons addLegend
#' @export
drawZonasLeaflet_global <- function(zona_df, id_col = "GID", geom_col = NULL,
                                    tile_provider = "cartodb", color_col = NULL) {
  stopifnot(inherits(zona_df, "sf"))
  if (!is.null(geom_col)) sf::st_geometry(zona_df) <- geom_col
  if (is.na(sf::st_crs(zona_df))) stop("Definí el CRS del sf antes de transformar.")
  
  zonas_ll <- sf::st_transform(zona_df, 4326)
  if (!id_col %in% names(zonas_ll)) stop("id_col no existe en el sf: ", id_col)
  zonas_ll$popup <- sprintf("<strong>%s:</strong> %s", id_col, as.character(zonas_ll[[id_col]]))
  
  base <- switch(tolower(tile_provider),
                 cartodb = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("CartoDB.Positron"),
                 stamen  = leaflet::leaflet(zonas_ll) %>% leaflet::addProviderTiles("Stamen.TonerLite"),
                 leaflet::leaflet(zonas_ll) %>% leaflet::addTiles()
  )
  
  if (!is.null(color_col)) {
    if (!color_col %in% names(zonas_ll)) stop("color_col no existe: ", color_col)
    pal <- leaflet::colorFactor("Set2", domain = zonas_ll[[color_col]])
    base %>%
      leaflet::addPolygons(weight=2, color=~pal(.data[[color_col]]),
                           fillColor=~pal(.data[[color_col]]), fillOpacity=0.4,
                           popup=~popup) %>%
      leaflet::addLegend(position="bottomright", pal=pal, values=~.data[[color_col]],
                         title=color_col, opacity=1)
  } else {
    base %>%
      leaflet::addPolygons(color="darkgreen", weight=2, fillOpacity=0.3, popup=~popup) %>%
      leaflet::addLegend(position="bottomright", colors="darkgreen",
                         labels="Zonas", title="Circuitos")
  }
}


# Uso todos los circuitos
mapa <- drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="GID", geom_col="THE_GEOM", tile_provider="cartodb")

# Mostrar GID
mapa <- drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="GID", geom_col="THE_GEOM")

# Mostrar COD_RECORRIDO
mapa <- drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="COD_RECORRIDO", geom_col="THE_GEOM")

# Color por COD_RECORRIDO
drawZonasLeaflet_global(E_DF_ZONA_RECORRIDO, id_col="COD_RECORRIDO", geom_col="THE_GEOM", color_col="COD_RECORRIDO")







#' Dibuja polígonos (sf) en Leaflet con popup y color genéricos
#'
#' @description
#' Reproyecta un `sf` a EPSG:4326 y lo dibuja en Leaflet.
#' El texto del popup usa `id_col`. Los colores se asignan por `color_col` si se provee.
#'
#' @param zona_df sf con geometría válida y CRS definido.
#' @param id_col character(1). Columna a mostrar en el popup. Si no existe, se crea un ID secuencial.
#' @param color_col character(1) o NULL. Columna categórica para colorear. Si NULL, color fijo.
#' @param geom_col character(1) o NULL. Nombre de la columna geométrica si no es la activa (p.ej. "THE_GEOM").
#' @param tile_provider character. "cartodb", "stamen" o cualquier otro para `addTiles()`.
#'
#' @return htmlwidget de Leaflet.
#'
#' @examples
#' # sf::st_geometry(zonas) <- "THE_GEOM"; sf::st_crs(zonas) <- 32721
#' # drawZonasLeaflet(zonas, id_col="COD_RECORRIDO", color_col="MUNICIPIO", geom_col="THE_GEOM")
drawZonasLeaflet <- function(zona_df,
                             id_col = "gid",
                             color_col = NULL,
                             geom_col = NULL,
                             tile_provider = "cartodb") {
  stopifnot(inherits(zona_df, "sf"))
  if (!is.null(geom_col)) sf::st_geometry(zona_df) <- geom_col
  if (is.na(sf::st_crs(zona_df))) stop("Definí el CRS del sf.")
  
  z <- sf::st_transform(zona_df, 4326)
  
  if (!id_col %in% names(z)) z[[id_col]] <- seq_len(nrow(z))
  z$popup <- sprintf("<strong>%s:</strong> %s", id_col, as.character(z[[id_col]]))
  
  base <- switch(tolower(tile_provider),
                 cartodb = leaflet::leaflet(z) |> leaflet::addProviderTiles("CartoDB.Positron"),
                 stamen  = leaflet::leaflet(z) |> leaflet::addProviderTiles("Stamen.TonerLite"),
                 leaflet::leaflet(z) |> leaflet::addTiles()
  )
  
  if (!is.null(color_col)) {
    stopifnot(color_col %in% names(z))
    key  <- as.factor(z[[color_col]])
    levs <- levels(key)
    pal  <- leaflet::colorFactor(grDevices::hcl.colors(length(levs), "Set 2"), levs)
    cols <- pal(key)  # vector de colores ya resuelto
    
    base |>
      leaflet::addPolygons(data = z, weight = 2,
                           color = cols, fillColor = cols, fillOpacity = 0.4,
                           popup = ~popup) |>
      leaflet::addLegend(position = "bottomright", pal = pal, values = levs,
                         title = color_col, opacity = 1)
  } else {
    base |>
      leaflet::addPolygons(data = z, color = "darkgreen", weight = 2,
                           fillOpacity = 0.3, popup = ~popup) |>
      leaflet::addLegend(position = "bottomright", colors = "darkgreen",
                         labels = "Zonas", title = "Capas")
  }
}

# Color por MUNICIPIO, popup COD_RECORRIDO
# popup por municipio y color por municipio
drawZonasLeaflet(municipios,
                 id_col    = "municipio",
                 color_col = "municipio",
                 geom_col  = "geometry",
                 tile_provider = "cartodb")


# Sin color categórico, popup por defecto
drawZonasLeaflet(municipios, geom_col="THE_GEOM")



drawZonasLeaflet(E_DF_ZONA_RECORRIDO)

drawZonasLeaflet(E_DF_ZONA_RECORRIDO,
                 id_col = "COD_RECORRIDO",
                 color_col = "COD_RECORRIDO",
                 geom_col = "THE_GEOM")












# Funciones de dibujo de rutas ----

## Solo una ruta o más que seleccione ----

#' Dibuja rutas seleccionadas por un campo (uno o varios valores)
#'
#' @param rutas_df sf con geometría LINESTRING/MULTILINESTRING y CRS definido.
#' @param valores vector con uno o varios valores a filtrar.
#' @param campo nombre de columna por la que filtrar (p.ej. "COD_RECORRIDO" o "NOM_RUT").
#' @param geom_col nombre de la columna geométrica si no es la activa (opcional).
#' @param tile_provider "OpenStreetMap", "CartoDB" o "Stamen".
#' @return htmlwidget leaflet.
drawRutasPorCodigoLeaflet <- function(rutas_df, valores,
                                      campo = "NOM_RUT",
                                      geom_col = NULL,
                                      tile_provider = c("OpenStreetMap","CartoDB","Stamen")) {
  tile_provider <- match.arg(tile_provider)
  stopifnot(inherits(rutas_df, "sf"))
  if (!is.null(geom_col)) sf::st_geometry(rutas_df) <- geom_col
  if (is.na(sf::st_crs(rutas_df))) stop("Definí el CRS del sf antes de transformar.")
  if (!campo %in% names(rutas_df)) stop("Campo inexistente: ", campo)
  
  vals <- unique(as.character(valores))
  sel  <- rutas_df[rutas_df[[campo]] %in% vals, , drop = FALSE]
  if (!nrow(sel)) stop("Sin coincidencias para: ", paste(vals, collapse=", "))
  
  sel  <- sf::st_make_valid(sel)
  rutas_ll <- sf::st_transform(sel, 4326)
  
  # popup robusto (usa columnas si existen)
  cols <- intersect(c("id","COD_RECORRIDO","NOM_RUT","FECHA_DESDE","FECHA_HASTA","MUNICIPIO"), names(rutas_ll))
  if (length(cols)) {
    parts <- lapply(cols, function(k) sprintf("<strong>%s:</strong> %s", k, as.character(rutas_ll[[k]])))
    rutas_ll$popup <- vapply(seq_len(nrow(rutas_ll)), function(i) paste(vapply(parts, `[`, "", i), collapse="<br/>"), "")
  } else {
    rutas_ll$popup <- "Ruta"
  }
  
  m <- leaflet::leaflet(rutas_ll)
  m <- switch(tile_provider,
              CartoDB       = leaflet::addProviderTiles(m, "CartoDB.Positron"),
              Stamen        = leaflet::addProviderTiles(m, "Stamen.TonerLite"),
              OpenStreetMap = leaflet::addTiles(m))
  
  m |>
    leaflet::addPolylines(color="blue", weight=3, opacity=0.7, popup=~popup) |>
    leaflet::addLegend(position="bottomright", colors="blue",
                       labels=paste(campo, "=", paste(vals, collapse=", ")),
                       title="Líneas")
}


# Por defecto filtra por "COD_RECORRIDO"
mapa1 <- drawRutasPorCodigoLeaflet(E_DF_RUTAS_RECORRIDO, valores = "B_DU_RM_CL_101")

# Varios códigos
mapa2 <- drawRutasPorCodigoLeaflet(E_DF_RUTAS_RECORRIDO, valores = c("B_DU_RM_CL_101","B_DU_RM_CL_102"))


## Todas las rutas ----


#' Dibuja todas las rutas
#'
#' @param rutas_df sf con geometría LINESTRING/MULTILINESTRING y CRS definido.
#' @param geom_col nombre de la columna geométrica si no es la activa (opcional).
#' @param tile_provider "OpenStreetMap", "CartoDB" o "Stamen".
#' @return htmlwidget leaflet.
drawTodasRutasLeaflet <- function(rutas_df,
                                  geom_col = NULL,
                                  tile_provider = c("OpenStreetMap","CartoDB","Stamen")) {
  tile_provider <- match.arg(tile_provider)
  stopifnot(inherits(rutas_df, "sf"))
  if (!is.null(geom_col)) sf::st_geometry(rutas_df) <- geom_col
  if (is.na(sf::st_crs(rutas_df))) stop("Definí el CRS del sf antes de transformar.")
  
  rutas_df <- sf::st_make_valid(rutas_df)
  rutas_ll <- sf::st_transform(rutas_df, 4326)
  
  cols <- intersect(c("id","COD_RECORRIDO","NOM_RUT","FECHA_DESDE","FECHA_HASTA","MUNICIPIO"), names(rutas_ll))
  if (length(cols)) {
    parts <- lapply(cols, function(k) sprintf("<strong>%s:</strong> %s", k, as.character(rutas_ll[[k]])))
    rutas_ll$popup <- vapply(seq_len(nrow(rutas_ll)), function(i) paste(vapply(parts, `[`, "", i), collapse="<br/>"), "")
  } else {
    rutas_ll$popup <- "Ruta"
  }
  
  m <- leaflet::leaflet(rutas_ll)
  m <- switch(tile_provider,
              CartoDB       = leaflet::addProviderTiles(m, "CartoDB.Positron"),
              Stamen        = leaflet::addProviderTiles(m, "Stamen.TonerLite"),
              OpenStreetMap = leaflet::addTiles(m))
  
  m |>
    leaflet::addPolylines(color="blue", weight=3, opacity=0.7, popup=~popup) |>
    leaflet::addLegend(position="bottomright", colors="blue",
                       labels="Todas las rutas", title="Líneas")
}


mapa1 <- drawTodasRutasLeaflet(E_DF_RUTAS_RECORRIDO)                       # OpenStreetMap por defecto








# LISTAR CAPAS PERO API GEOSERVER ----

# REST API de GeoServer (listar workspaces y capas)
# Sustituye "tu_usuario" y "tu_contraseña" por credenciales válidas
user <- "im4445285"
pass <- "ponerpass"

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

# FIN DEL SCRIPT ----





# LISTAR LEVANTE DESDE GOL----



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


# FIN LISTAR LEVANTE DESDE GOL----




# COMPLEMENTO LEVANTES GOL ----
library(jsonlite)



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

df <- fromJSON(
  toJSON(props_clean, auto_unbox = TRUE, null = "null"),
  flatten = TRUE
)
df <- as_tibble(df) 


# 3) Combínalo en un solo data.frame
df <- do.call(rbind.data.frame, props_clean)

# 5) Ajusta tipos si lo necesitas
df$porcentajellenado    <- as.numeric(df$porcentajellenado)
df$UNA                  <- as.numeric(df$UNA)
df$estaProgramado       <- as.logical(df$estaProgramado)
df$contenedorGid        <- as.integer(df$contenedorGid)

# FIN - COMPLEMENTO LEVANTES GOL -----



# CIRCUITOS Y POSICIONES DESDE GEOSERVER (PARECE IGUAL QUE EN ) ----
# Obtener la url.

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

# FIN - CIRCUITOS Y POSICIONES DESDE GEOSERVER (PARECE IGUAL QUE EN ) ----




# OBTENER LAS CAPAS Y MOSTRARLAS -----
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

# FIN - OBTENER LAS CAPAS Y MOSTRARLAS -----

# LEER LAS CAPAS ----


library(httr)
library(geojsonsf)
library(sf)
library(lubridate)


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

# FIN - LEER LAS CAPAS ----

# IMPLEMENTAR LAS CAPAS ----

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



# PARECE SER LOS PUNTO DE ENTREGA VOLUNTARIO
layer   <- "imm:v_re_reclamos_vwh"
mias_sf <- leer_capa_geojson(wfs_url, layer, out_file = "imm:v_re_reclamos_vwh.json")


# FIN - IMPLEMENTAR LAS CAPAS ----


# OTRA FORMA DE LEER LAS CAPAS ----
# esto lo suplanta la funcion leer_capa_geojson, lo guardo por las dudas.

# 0) Instalar geojsonsf si no lo tienes
# install.packages("geojsonsf")

# 1) Cargar librerías
library(httr)
library(geojsonsf)
library(sf)

# 2) Parámetros del WFS
wfs_url  <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
layer    <- "analisisdatos:v_lim_ultlevantes" ### ACa modificar
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

# FIN - OTRA FORMA DE LEER LAS CAPAS ----





# MANDAR MAIL - REVISAR ----
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

# FIN - MANDAR MAIL - REVISAR ----


library(httr)
library(xml2)
library(httr)
library(xml2)

listar_featuretypes_wfs <- function(wfs_base, version = "1.0.0") {
  cap <- paste0(wfs_base, if (!grepl("\\?", wfs_base)) "?" else "&",
                "service=WFS&version=", version, "&request=GetCapabilities")
  resp <- GET(cap); stop_for_status(resp)
  x <- read_xml(content(resp, "text", encoding = "UTF-8"))
  
  # toma cada FeatureType y saca sus hijos; evita problemas de namespace y longitudes
  fts <- xml_find_all(x, ".//*[local-name()='FeatureTypeList']/*[local-name()='FeatureType']")
  get_txt <- function(node, path) {
    v <- xml_text(xml_find_first(node, path))
    ifelse(length(v) == 0, NA_character_, v)
  }
  crs_tag <- switch(version,
                    "1.0.0" = "SRS",
                    "1.1.0" = "DefaultSRS",
                    "2.0.0" = "DefaultCRS",
                    "DefaultCRS")
  res <- lapply(fts, function(ft) {
    data.frame(
      name  = get_txt(ft, ".//*[local-name()='Name']"),
      title = get_txt(ft, ".//*[local-name()='Title']"),
      crs   = get_txt(ft, paste0(".//*[local-name()='", crs_tag, "']")),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res)
}

wfs <- "https://geoserver.montevideo.gub.uy/geoserver/wfs"
t10 <- listar_featuretypes_wfs(wfs, "1.0.0")
t11 <- listar_featuretypes_wfs(wfs, "1.1.0")

wfs_app <- "https://montevideo.gub.uy/app/geoserver/ows"
t20 <- listar_featuretypes_wfs(wfs_app, "2.0.0")

nrow(t10); nrow(t11); nrow(t20)
setdiff(t10$name, t20$name)  # solo en 1.0.0
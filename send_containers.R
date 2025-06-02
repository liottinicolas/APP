library(httr)
library(sf)
library(blastula)
library(glue)

# 1) Descargar GeoJSON crudo
wfs_url <- "http://geoserver.montevideo.gub.uy/geoserver/wfs"
layer   <- "ide:V_DF_POSICIONES_MAPAWEB_GEOM"
tmp_geojson <- tempfile(fileext = ".geojson")

resp <- GET(wfs_url, query = list(
  service      = "WFS",
  version      = "1.0.0",
  request      = "GetFeature",
  typeName     = layer,
  outputFormat = "application/json"
))
stop_for_status(resp)
writeBin(content(resp, "raw"), tmp_geojson)

# 2) Leerlo en sf y añadir columna con la fecha de ejecución
df <- st_read(tmp_geojson, quiet = TRUE)
df$fecha_envio <- Sys.Date()    # aquí agregamos la fecha

# 3) Volver a escribir un GeoJSON con la nueva columna
out_file <- "contenedores_con_fecha.geojson"
st_write(df, out_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)

# 4) Componer el correo
n    <- nrow(df)
cols <- paste(names(df), collapse = ", ")

email <- compose_email(
  body = md(glue("
    **Capa:** {layer}  
    **Registros:** {n}  
    **Columnas:** {cols}  
    **Fecha de envío:** {Sys.Date()}
  "))
) %>%
  add_attachment(file = out_file, content_type = "application/json")

# 5) Enviar por SMTP (lee credenciales de GitHub Secrets)
smtp <- smtp_credentials(
  host     = Sys.getenv("SMTP_HOST"),
  port     = as.integer(Sys.getenv("SMTP_PORT")),
  username = Sys.getenv("SMTP_USER"),
  password = Sys.getenv("SMTP_PASS"),
  use_ssl  = TRUE
)

email %>%
  smtp_send(
    from        = Sys.getenv("SMTP_FROM"),
    to          = Sys.getenv("SMTP_TO"),
    subject     = glue("Contenedores con fecha — {Sys.Date()}"),
    credentials = smtp
  )
#* Veo la información de grafana para ver que puedo obtener
#*
#*


# 0) Instalar/​cargar paquetes -----------------------------------------------
if (!requireNamespace("httr",    quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite",quietly = TRUE)) install.packages("jsonlite")
library(httr)
library(jsonlite)

# 1) Configuración ----------------------------------------------------------
grafana_host <- "https://graf.montevideo.gub.uy/graf"     # sin '/d' ni '/graf'
usuario      <- "im4445285"
contraseña   <- "Nico1919*"
dash_uid     <- "aaXRzFvns"                          # extraído de la URL /d/<UID>/...

auth_hdr <- authenticate(usuario, contraseña, type = "basic")
# 2) Listar datasources y filtrar WMS ---------------------------------------
resp_ds <- GET(
  url = paste0(grafana_host, "/api/datasources"),
  auth_hdr
)
stop_for_status(resp_ds)

# Parsear el JSON
ds_text <- content(resp_ds, "text", encoding = "UTF-8")
ds_list <- jsonlite::fromJSON(ds_text, flatten = TRUE)

# Inspecciona rápidamente la estructura
cat("Clase de ds_list:", class(ds_list), "\n")
# str(ds_list)

# Filtrar las WMS de forma robusta
if (is.data.frame(ds_list)) {
  # Caso data.frame
  wms_ds <- subset(ds_list, type == "wms", select = c("id","name","type","url"))
} else if (is.list(ds_list)) {
  # Caso lista de listas
  types <- sapply(ds_list, `[[`, "type")
  idx   <- types == "wms"
  if (any(idx)) {
    wms_ds <- data.frame(
      id    = sapply(ds_list[idx], `[[`, "id"),
      name  = sapply(ds_list[idx], `[[`, "name"),
      type  = types[idx],
      url   = sapply(ds_list[idx], `[[`, "url"),
      stringsAsFactors = FALSE
    )
  } else {
    wms_ds <- data.frame()  # sin WMS
  }
} else {
  stop("Estructura de ds_list inesperada: ", class(ds_list))
}

# Mostrar resultados
if (nrow(wms_ds) == 0) {
  message("No se encontraron datasources de tipo WMS.")
} else {
  cat("Datasources WMS encontradas:\n")
  print(wms_ds)
}


### El objeto ds_list es simplemente la representación en R de la respuesta JSON que te devuelve el endpoint /api/datasources. Es decir:

resp_ds <- GET(paste0(grafana_host, "/api/datasources"), auth_hdr)
ds_text <- content(resp_ds, "text", encoding = "UTF-8")
ds_list <- jsonlite::fromJSON(ds_text, flatten = TRUE)

# Te dice si es data.frame, lista, etc.
cat("Clase de ds_list:", class(ds_list), "\n")

# Te muestra nombres de columnas o elementos
if (is.data.frame(ds_list)) {
  print(names(ds_list))
  print(head(ds_list))
} else {
  str(ds_list)
}



# 1) Compruebo que mi R vea el host y puerto:
ping_res <- try(system("ping -c 1 sthprodv.imm.gub.uy", intern = TRUE))
print(ping_res)

# 2) Si pasa el ping, pruebo un GET:
res <- GET("http://sthprodv.imm.gub.uy:4200/health")  # cambia /health por lo que tengas
if (status_code(res) == 200) {
  cat("Respuesta:\n", content(res, "text", encoding="UTF-8"))
} else {
  cat("HTTP", status_code(res), "– no está accesible desde aquí\n")
}
library(httr)
library(xml2)

# 1) Creamos un handle para el dominio de APEX
h <- handle("https://apex.imm.gub.uy")

# 2) Primer GET al login (sin session) usando el mismo handle
url_login <- "https://apex.imm.gub.uy/apex/r/prod/cons-exp/login_desktop"
resp0     <- GET(url_login, handle = h)
stop_for_status(resp0)

# 3) Parseamos el HTML para encontrar el action y extraer el session
html_doc <- read_html(content(resp0, "text", encoding = "UTF-8"))
form_node <- xml_find_first(html_doc, ".//form")
action_rel <- xml_attr(form_node, "action", default = "")
if (action_rel == "") stop("No se encontró el atributo 'action' en el formulario.")
base_host <- "https://apex.imm.gub.uy/apex/"
url_action_full <- paste0(base_host, action_rel)

# Extraemos el número de sesión que está después de "login_desktop/"
session_val <- sub(".*login_desktop/([0-9]+).*", "\\1", url_action_full)
if (!grepl("^[0-9]+$", session_val)) {
  stop("No se pudo extraer un número válido de sesión.")
}
cat("Sesión detectada:", session_val, "\n")

# 4) Extraemos los campos ocultos necesarios para el POST
get_hidden <- function(name) {
  nodo <- xml_find_first(html_doc, paste0(".//input[@type='hidden'][@name='", name, "']"))
  if (is.na(nodo)) return("") else return(xml_attr(nodo, "value"))
}

p_flow_id      <- get_hidden("p_flow_id")
p_flow_step_id <- get_hidden("p_flow_step_id")
p_instance     <- get_hidden("p_instance")
p_flow_fcs     <- get_hidden("p_flow_fcs")
p_request      <- get_hidden("p_request")  # normalmente "LOGIN"

# 5) Tus credenciales: ajusta los nombres de campo si en tu HTML son p_t01 / p_t02, etc.
usuario <- "im4445285"
clave   <- "Nico1919*"

payload_login <- list(
  p_flow_id      = p_flow_id,
  p_flow_step_id = p_flow_step_id,
  p_instance     = p_instance,
  p_request      = p_request,
  p_flow_fcs     = p_flow_fcs,
  p_app_user     = usuario,
  p_app_pwd      = clave
)

# 6) Enviamos el POST con el mismo handle (que preserva cookies automáticamente)
resp_auth <- POST(
  url    = url_action_full,
  body   = payload_login,
  encode = "form",
  handle = h
)
stop_for_status(resp_auth)
cat("Login enviado. HTTP status:", status_code(resp_auth), "\n")


# Justo después del POST de login, revisa la sesión con la URL correcta:
url_test <- paste0(
  "https://apex.imm.gub.uy/apex/r/prod/cons-exp/consultascsv?session=",
  session_val
)
resp_test <- GET(url_test, handle = h)
cat("Status code de prueba:", status_code(resp_test), "\n")













library(httr)
library(xml2)

# ------------------------------------------------------------------------------
# 0) Ubicación del “cookie-jar” en disco
# ------------------------------------------------------------------------------
cookie_path <- "apex_cookies.txt"

# ------------------------------------------------------------------------------
# 1) LOGIN APEX (guardamos la cookie, pero NO forzamos session= en la URL)
# ------------------------------------------------------------------------------
h <- handle("https://apex.imm.gub.uy")

# GET al login
resp0 <- GET(
  "https://apex.imm.gub.uy/apex/r/prod/cons-exp/login_desktop",
  handle = h,
  config(cookiefile = cookie_path, cookiejar = cookie_path)
)
stop_for_status(resp0)

# Parsear formulario de login para extraer el action
html_doc   <- read_html(content(resp0, "text", encoding = "UTF-8"))
form_login <- xml_find_first(html_doc, ".//form")
action_rel <- xml_attr(form_login, "action", default = "")
if (action_rel == "") stop("No se encontró 'action' en el form de login.")
url_action_full <- paste0("https://apex.imm.gub.uy/apex/", action_rel)

# Sacar hidden fields necesarios
get_hidden <- function(html, name) {
  nodo <- xml_find_first(html, paste0(".//input[@type='hidden' and @name='", name, "']"))
  if (is.na(nodo)) return("") else return(xml_attr(nodo, "value"))
}
p_flow_id            <- get_hidden(html_doc, "p_flow_id")
p_flow_step_id       <- get_hidden(html_doc, "p_flow_step_id")
p_instance           <- get_hidden(html_doc, "p_instance")
p_page_submission_id <- get_hidden(html_doc, "p_page_submission_id")
p_request            <- get_hidden(html_doc, "p_request")
p_reload_on_submit   <- get_hidden(html_doc, "p_reload_on_submit")
p_flow_fcs           <- get_hidden(html_doc, "p_flow_fcs")

# Tus credenciales (nombres exactos de los inputs: P101_USERNAME / P101_PASSWORD)
usuario <- "im4445285"
clave   <- "Nico1919*"

payload_login <- list(
  p_flow_id            = p_flow_id,
  p_flow_step_id       = p_flow_step_id,
  p_instance           = p_instance,
  p_page_submission_id = p_page_submission_id,
  p_request            = p_request,
  p_reload_on_submit   = p_reload_on_submit,
  p_flow_fcs           = p_flow_fcs,
  P101_USERNAME        = usuario,
  P101_PASSWORD        = clave
)

# POST de login (guardando cookie)
resp_auth <- POST(
  url    = url_action_full,
  body   = payload_login,
  encode = "form",
  handle = h,
  config(cookiefile = cookie_path, cookiejar = cookie_path)
)
stop_for_status(resp_auth)
cat("✅ Login exitoso. HTTP status:", status_code(resp_auth), "\n\n")

# ------------------------------------------------------------------------------
# 2) VERIFICAR QUE LA SESIÓN SIGUE ACTIVA
# ------------------------------------------------------------------------------
resp_test <- GET(
  "https://apex.imm.gub.uy/apex/r/prod/cons-exp/consultascsv",
  handle = h,
  config(cookiefile = cookie_path, cookiejar = cookie_path)
)
cat("Status code de verificación:", status_code(resp_test), "\n")
if (status_code(resp_test) != 200) {
  stop("La sesión APEX no está activa o la URL es incorrecta.")
}
cat("→ La sesión sigue activa. Vamos con csql_id = 10334.\n\n")

# ------------------------------------------------------------------------------
# 3) EJECUTAR Y DESCARGAR CSV DE csql_id = 10334 (sin ‘session=’ en la URL)
# ------------------------------------------------------------------------------
# 3.1) Valores a enviar
fecha_desde  <- "05/06/2025"
fecha_hasta  <- "10/06/2025"
lugar_salida <- "0"

# 3.2) Hacer GET a la página de parámetros, pasando p4_csql_id y clear por query
resp_param <- GET(
  "https://apex.imm.gub.uy/apex/r/prod/cons-exp/paramentros-consulta",
  handle = h,
  config(cookiefile = cookie_path, cookiejar = cookie_path),
  query = list(
    p4_csql_id = 10334,
    clear      = "RP,4,4"
    # omitimos session=… aquí
  )
)
stop_for_status(resp_param)
html_param <- read_html(content(resp_param, "text", encoding = "UTF-8"))

# 3.3) Verificar que no volvió al login
if (!is.na(xml_find_first(html_param, ".//input[@name='P101_USERNAME']"))) {
  stop("ERROR: Seguimos en el login. Quizá la cookie caducó.")
}
if (is.na(xml_find_first(html_param, ".//input[@name='f02']"))) {
  stop("ERROR: No se encontró <input name='f02'>; revisa la URL o la sesión.")
}
cat("→ Formulario de parámetros cargado correctamente para csql_id = 10334\n\n")

# 3.4) Extraer <form> y su action (POST “Ejecutar”)
form_node  <- xml_find_first(html_param, ".//form[@method='post']")
action_rel <- xml_attr(form_node, "action", default = "")
if (action_rel == "") stop("El <form> de parámetros no tiene atributo 'action'.")
url_action <- paste0("https://apex.imm.gub.uy/apex/", action_rel)
cat("→ URL de acción para POST 'Ejecutar':", url_action, "\n\n")

# 3.5) Localizar las filas de la tabla
rows <- xml_find_all(form_node, ".//tr[input[@type='hidden' and @name='f01']]")
cat("→ Filas encontradas en la tabla de parámetros:", length(rows), "\n")
if (length(rows) != 3) {
  warning("Se esperaban 3 filas, pero se encontraron ", length(rows), ".")
} else {
  cat("  - Fila 1 → Fecha_desde\n")
  cat("  - Fila 2 → Fecha_hasta\n")
  cat("  - Fila 3 → lugar_salida\n\n")
}

# 3.6) Construir vectores f01, f02, f05, f06, f07
f01_vec <- sapply(rows, function(tr) {
  xml_attr(xml_find_first(tr, ".//input[@name='f01']"), "value")
})
f02_vec <- sapply(rows, function(tr) {
  xml_attr(xml_find_first(tr, ".//input[@name='f02']"), "value")
})
f05_vec <- sapply(rows, function(tr) {
  xml_attr(xml_find_first(tr, ".//input[@name='f05']"), "value")
})
f06_vec <- sapply(rows, function(tr) {
  xml_attr(xml_find_first(tr, ".//input[@name='f06']"), "value")
})
f07_vec <- sapply(rows, function(tr) {
  xml_attr(xml_find_first(tr, ".//input[@name='f07']"), "value")
})
cat("Vectores extraídos:\n")
cat(" f01:", paste(f01_vec, collapse = ", "), "\n")
cat(" f02:", paste(f02_vec, collapse = ", "), "\n")
cat(" f05:", paste(f05_vec, collapse = ", "), "\n")
cat(" f06:", paste(f06_vec, collapse = ", "), "\n")
cat(" f07:", paste(f07_vec, collapse = ", "), "\n\n")

# 3.7) Construir vector f04 a partir de f02_vec
f04_vec <- sapply(f02_vec, function(param) {
  if (param == "Fecha_desde") {
    fecha_desde
  } else if (param == "Fecha_hasta") {
    fecha_hasta
  } else if (param == "lugar_salida") {
    lugar_salida
  } else {
    stop("Parámetro inesperado en f02_vec: ", param)
  }
})
cat(" f04:", paste(f04_vec, collapse = ", "), "\n\n")

# ------------------------------------------------------------------------------
# 4) ARMAR EL PAYLOAD PARA POST “Ejecutar”
# ------------------------------------------------------------------------------
payload <- list()
# 4.1) Añadir todos los hidden genéricos del <form>
for (n in xml_find_all(form_node, ".//input[@type='hidden']")) {
  nm  <- xml_attr(n, "name")
  val <- xml_attr(n, "value")
  if (!is.na(nm) && nzchar(nm)) {
    payload[[nm]] <- val
  }
}
# 4.2) Forzar p_request = "Ejecutar"
payload[["p_request"]] <- "Ejecutar"
# 4.3) Añadir los vectores f01…f07 y f04
payload[["f01"]] <- f01_vec
payload[["f02"]] <- f02_vec
payload[["f04"]] <- f04_vec
payload[["f05"]] <- f05_vec
payload[["f06"]] <- f06_vec
payload[["f07"]] <- f07_vec
cat("Payload armado:\n")
cat("  Campos: ", paste(names(payload), collapse = ", "), "\n")
cat("  Longitud f01…f07, f04:", length(payload[["f01"]]), "elementos\n\n")

# ------------------------------------------------------------------------------
# 5) ENVIAR POST “Ejecutar” (usando el cookie-jar)
# ------------------------------------------------------------------------------
resp_ejec <- POST(
  url    = url_action,
  body   = payload,
  encode = "form",
  handle = h,
  timeout = 60,
  config(cookiefile = cookie_path, cookiejar = cookie_path)
)
stop_for_status(resp_ejec)
cat("✅ POST 'Ejecutar' enviado. HTTP status:", status_code(resp_ejec), "\n\n")

# ------------------------------------------------------------------------------
# 6) CAPTURAR EL BOTÓN “DESCARGAR” Y OBTENER LA URL DEL CSV
# ------------------------------------------------------------------------------
html_res <- read_html(content(resp_ejec, "text", encoding = "UTF-8"))

nodo_btn <- xml_find_first(
  html_res,
  ".//button[contains(normalize-space(.),'Descargar') 
           and contains(@onclick,'apex.navigation.redirect')]"
)
if (is.na(nodo_btn)) {
  stop("No se encontró el botón “Descargar” tras ejecutar la consulta 10334.")
}
onclick_raw <- xml_attr(nodo_btn, "onclick", default = "")

# Extraer la URL escapada dentro de apex.navigation.redirect('…')
pattern    <- "apex\\.navigation\\.redirect\\('([^']+)'\\)"
url_between<- sub(pattern, "\\1", onclick_raw)
# Des-escapar \u002F → "/" y \u0026 → "&"
url_unesc1  <- gsub("\\\\u002F", "/", url_between, perl = TRUE)
url_unesc   <- gsub("\\\\u0026", "&", url_unesc1, perl = TRUE)

# Construir la URL absoluta final para el CSV
url_csv <- if (grepl("^/apex/", url_unesc)) {
  paste0("https://apex.imm.gub.uy", url_unesc)
} else if (grepl("^https?://", url_unesc)) {
  url_unesc
} else {
  paste0("https://apex.imm.gub.uy/", url_unesc)
}
cat("→ URL para descargar el CSV:\n   ", url_csv, "\n\n")

# ------------------------------------------------------------------------------
# 7) BAJAR EL CSV FINAL Y GUARDARLO COMO 'consulta_10334.csv'
# ------------------------------------------------------------------------------
resp_csv <- GET(
  url_csv,
  handle = h,
  config(cookiefile = cookie_path, cookiejar = cookie_path)
)
stop_for_status(resp_csv)
nombre_csv <- "consulta_10334.csv"
writeBin(content(resp_csv, "raw"), nombre_csv)
cat("📥 CSV guardado como '", nombre_csv, "'\n", sep = "")

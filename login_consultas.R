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



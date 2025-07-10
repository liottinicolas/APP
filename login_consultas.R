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





############################## #####################################
############## LOGIN APP GOL #####################################
############################## #####################################


# Librerías
library(httr)
library(xml2)

# 1) Nuevo handle para Montevideo SSO
h2 <- handle("https://is.montevideo.gub.uy")

# 2) URL completa desde tu navegador (con todos los parámetros)
url_login <- "https://is.montevideo.gub.uy/authenticationendpoint/login.do?RelayState=a8726b81-34f4-45ba-b3a4-e5216eceed69&SigAlg=http%3A%2F%2Fwww.w3.org%2F2001%2F04%2Fxmldsig-more%23rsa-sha256&Signature=NqK%2F6N6hF8wio4gk%2FIXcUBV4Ps7VALev%2Fu5D7ICFJnJXRcod72WNWEj3%2BUgG%2FhvEwW3Ol1RJoU4hZyvKI5K%2F9oY5e9PNdgVE2peD6j8HBa0oPEkCjiSBPNzyvcdFNhx4eQ1itBZ%2Fg%2FL4vzxZAQbCg2caWF02KU9153Dx08kK%2F7uf3cQX28eP0HPVleqxpEtkOodL1pCMr83gO16y7tle8QMZCUUqLgyl7wBRc47R2KWXh2%2FN7dYCIpDUYgSzohEQf724PvxipZ0z4ZIC93HoXVEN8SeA78ZA84%2BGPkF%2FCvTtrOkjUUbq8SPd0VbGDrd8%2BhhXAh6o30J20I%2BF3Gqm6Q%3D%3D&commonAuthCallerPath=%2Fsamlsso&forceAuth=false&passiveAuth=false&tenantDomain=carbon.super&sessionDataKey=65f19eed-fae9-4384-8324-774e53c09d38&relyingParty=ih.montevideo.gub.uy&type=samlsso&sp=ih.montevideo.gub.uy&isSaaSApp=false&authenticators=BasicAuthenticator%3ALOCAL"

# 3) GET inicial (ya lo tienes funcionando)
resp0 <- GET(url_login, handle = h2, 
             user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/115.0"), 
             verbose())
stop_for_status(resp0)

# 4) Extraer el <form> y la URL de POST
doc        <- read_html(content(resp0, "text", encoding = "UTF-8"))
form       <- xml_find_first(doc, ".//form")
action_rel <- xml_attr(form, "action")
url_post   <- url_absolute(action_rel, url_login)
cat("POST a:", url_post, "\n")

# 5) Valores que ya conoces
usuario <- "im4445285"
clave   <- "Nico1919*"

# 6) Construir el payload con los 4 campos
payload <- list(
  tocommonauth   = xml_attr(xml_find_first(form, ".//input[@name='tocommonauth']"), "value"),
  username       = usuario,
  password       = clave,
  sessionDataKey = xml_attr(xml_find_first(form, ".//input[@name='sessionDataKey']"), "value")
)
print(payload)

# 7) Enviar el POST de login
resp1 <- POST(
  url      = url_post,
  handle   = h2,
  body     = payload,
  encode   = "form",
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/115.0"),
  verbose()
)
stop_for_status(resp1)
cat("Login enviado. Status:", status_code(resp1), "\n")

# 8) (Opcional) Verificación tras login
#   Por ejemplo, consulta un recurso protegido con el mismo handle h2
# resp2 <- GET("https://is.montevideo.gub.uy/algún_endpoint_protegido", handle = h2)
# cat("Status recurso protegido:", status_code(resp2), "\n")







############################## #####################################
############## FIN LOGIN APP GOL #####################################
############################## #####################################




### ACA FUNCIONO PARA OBTENER 1 CIRCUITO """


library(httr)

# 1. Define la URL tal cual
url <- "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/planificacion/2/2025-07-02/7/viaje/22544/levantes"

# 2. Pon aquí tu Bearer token (sin las comillas finales “…”)
token <- "eyJ4NXQiOiJOakEwTVRZeU1qZGtPV1kyT1dFek56QmxObVV4TkRVeVpESm1OMk0zT0dSaVpqSXhOV1ExWmciLCJraWQiOiJOakEwTVRZeU1qZGtPV1kyT1dFek56QmxObVV4TkRVeVpESm1OMk0zT0dSaVpqSXhOV1ExWmciLCJhbGciOiJSUzI1NiJ9.eyJhdF9oYXNoIjoiRk8zeWdHZnpBRFg5X2s1OXdPMk91USIsInN1YiI6ImltNDQ0NTI4NSIsImFtciI6WyJyZWZyZXNoX3Rva2VuIl0sImlzcyI6Imh0dHBzOlwvXC9paC5tb250ZXZpZGVvLmd1Yi51eTo0NDNcL29hdXRoMlwvdG9rZW4iLCJncm91cHMiOlsiU09fR09MX0FDQ0VTTyIsIkxJTVBJRVpBX0dFU1RJT05fT1BFUkFUSVZBIiwiSW50ZXJuYWxcL2V2ZXJ5b25lIl0sInByZWZlcnJlZF91c2VybmFtZSI6Ik5pY29sYXMgTGlvdHRpIiwiZ2l2ZW5fbmFtZSI6Ik5pY29sYXMiLCJhdWQiOiJvUGFsb3JZcTAxN3pNamhwOE9EZU1fYkVmb29hIiwibmJmIjoxNzUxNDc5ODIxLCJhenAiOiJvUGFsb3JZcTAxN3pNamhwOE9EZU1fYkVmb29hIiwiZXhwIjoxNzUxNDgzNDIxLCJpYXQiOjE3NTE0Nzk4MjEsIk11bHRpQXR0cmlidXRlU2VwYXJhdG9yIjpbXSwiZmFtaWx5X25hbWUiOiJMaW90dGkifQ.L6Q-Y5UbCQ3KweNwcRx3s7sHK70Y76ZIt5zuunQbCqPp9RnaSc-pC5dlqmDhkYf8WoGeiYHfgLiEt-ZZYlcrOuIecXJ3GR4wR_2wx_eH-L2NvKm2deViBy5vRRmskfmAgray-ViurYHhnUWco8hi4XiEoCSpobI-zbizbBZ731Xf7sQbyJ2qb_djKTF8iLxQqeSBaIMUHbaWyqdRpBKTuLP29OOcvY0q1lN1d4yc0rANGaurOpYd-OEWVCsMykmoh-dkj3qj38WbIZf68VZGBgez57WzBLoDz6_TDPEKiEJIuEicmONOaHJ79rtOaV7cH_gcdm5zxBRjU8FGxRBjjw"

# 3. Pega aquí la cadena completa de cookies que viste en el -b
cookie_str <- paste(
  "f5avraaaaaaaaaaaaaaaa_session_=FNJHELJJIHCNDLCDGAIHOCPCDNFEEKEONGKBBOGLJMPHFDKAHNNAGJDHCHFHOJFPONCDBHGJBGDKMBAJAKMAMDHPLHCDFPFJPPCBNIMHELOKOIJPFAPMKCDNKJEICKMI;",
  "f5_cspm=1234;",
  "f5avraaaaaaaaaaaaaaaa_session_=EKAAKAMECKECJPOMDHNKMIGNDMOGBNCLEOKIDEMNNOOPMLIMLBMGADFAFGIGJNEAPCADKBLMFDLNDIELKPKAKCKNHHFGIBKELLLOPIMMGKMDFECHIIIILOJBFCIPANIE;",
  "_ga_RG2DNF9893=GS1.3.1745426514.6.0.1745426514.0.0.0;",
  "_ga=GA1.1.1626996302.1742634998;",
  "_ga_SZDK0HF1BL=GS2.1.s1750870077^$o13^$g0^$t1750870077^$j60^$l0^$h0;",
  "BIGipServer~ocp4-prod~Shared~pool_intranet-drupal=1998188298.36895.0000;",
  "BIGipServer~ocp4-prod~Shared~pool_gestion-operativa-frontend-limpieza=4195020554.36895.0000;",
  "BIGipServer~ocp4-prod~Shared~pool_gestion-operativa-backend-limpieza=2015096586.36895.0000;",
  "LtpaToken=AAECAzY4NjU0QTVGNjg2NUJBREZDTj1OaWNvbGFzIExpb3R0aSA0NDQ1Mjg1L089SU1NXX7DTrzHFg6lpZKli84a0U7K6vw=;",
  "f5avraaaaaaaaaaaaaaaa_session_=GKAELMPDAJLODIBPPBBMKDAAOPBIHDBDGLGCCFCPKNKAJOBLAIKEMEENNALMMHKADFIDJBIJJCCGLKIKADPADOJBDGMCFNOJEDEGIGGCDACMOEHCEDDAHHOFIKBEBLCP"
)

# 4. Ejecuta el GET con todos los headers
resp <- GET(
  url,
  add_headers(
    Accept             = "application/json, text/plain, */*",
    `Accept-Language`  = "es-ES,es;q=0.9,en;q=0.8",
    Authorization      = paste("Bearer", token),
    Connection         = "keep-alive",
    Referer            = "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/levantes/2/2025-07-02/7/22544",
    `Sec-Fetch-Dest`   = "empty",
    `Sec-Fetch-Mode`   = "cors",
    `Sec-Fetch-Site`   = "same-origin",
    `User-Agent`       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36",
    `sec-ch-ua`        = "\"Google Chrome\";v=\"137\", \"Chromium\";v=\"137\", \"Not/A)Brand\";v=\"24\"",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "\"Windows\"",
    Cookie             = cookie_str
  ),
  verbose()
)

stop_for_status(resp)

# 5. Y parseas el JSON:
datos <- content(resp, "parsed", simplifyVector = TRUE)
str(datos)

### fin### ### ###  """
### ### ### ### 
### ### ### 


library(httr)

# 1) Define la URL del endpoint
url <- "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/api/frontend/v1/configuracion/motivoNoLevante"

# 2) Tu Bearer token obtenido tras el login
token <- "eyJ4NXQiOiJOakEwTVRZeU1qZGtPV1kyT1dFek56QmxObVV4TkRVeVpESm1OMk0zT0dSaVpqSXhOV1ExWmciLCJraWQiOiJOakEwTVRZeU1qZGtPV1kyT1dFek56QmxObVV4TkRVeVpESm1OMk0zT0dSaVpqSXhOV1ExWmciLCJhbGciOiJSUzI1NiJ9.eyJhdF9oYXNoIjoiLUo5UE9fUVc5RC04M215dWtoZmxRUSIsInN1YiI6ImltNDQ0NTI4NSIsImFtciI6WyJTQU1MU1NPQXV0aGVudGljYXRvciJdLCJpc3MiOiJodHRwczpcL1wvaWgubW9udGV2aWRlby5ndWIudXk6NDQzXC9vYXV0aDJcL3Rva2VuIiwiZ3JvdXBzIjpbIlNPX0dPTF9BQ0NFU08iLCJMSU1QSUVaQV9HRVNUSU9OX09QRVJBVElWQSIsIkludGVybmFsXC9ldmVyeW9uZSJdLCJwcmVmZXJyZWRfdXNlcm5hbWUiOiJOaWNvbGFzIExpb3R0aSIsImdpdmVuX25hbWUiOiJMaW90dGkifQ.kNY-es95InWl7ZrIgA3HkZD5VbmYus7-xMpk_SKzBx4jAddbpl05l--pvzL9F46UrpQ61RJhW7wV0etmoKFSc21xfNU9_0giD8VaCJrMZVrQprnJ9j_9GRjdnmZqo1kce7OBWL2bJpVnijk9znBAfcMwXZ2QkXqt2Ip3fSIIhuEDliJ7hDDd_0VcG4W3FnL433SkoUIjxKBz6Cy-W0F0klyeQwWDFOdpsvJxNbOiblkrDHjgXznjCxURATLbPRzUK_Ui_bNytJ8uRET_L84kIka1_uQICYJOqyNLuM4pOUxdUIMB5_bbROzHR4N5t7_vI8ShAV8QhTp6YxaZipmTEw"

# 3) Cadena de cookies de tu sesión (cópialas de Postman o de cookies(resp_login))
cookie_str <- paste(
  "f5_cspm=1234",
  "f5avraaaaaaaaaaaaaaaa_session_=JONFGLICLBLODPEHJIGDLNDLLNCDBLPLAONFGNGJNPAJLNJOMHAPBEBONAJNAFHNEIEDMMCHPKDJLBMFGBBAPFELGHJCMJJIIGDODJIBIIKDFDJCGAOHFPGDAPABFIHC",
  "_ga_RG2DNF9893=GS1.3.1745426514.6.0.1745426514.0.0.0",
  "_ga=GA1.1.1626996302.1742634998",
  "_ga_SZDK0HF1BL=GS2.1.s1750870077^$o13^$g0^$t1750870077^$j60^$l0^$h0",
  "BIGipServer~ocp4-prod~Shared~pool_intranet-drupal=1998188298.36895.0000",
  "BIGipServer~ocp4-prod~Shared~pool_gestion-operativa-frontend-limpieza=4195020554.36895.0000",
  "BIGipServer~ocp4-prod~Shared~pool_gestion-operativa-backend-limpieza=2015096586.36895.0000",
  "LtpaToken=AAECAzY4NjU0QTVGNjg2NUJBREZDTj1OaWNvbGFzIExpb3R0aSA0NDQ1Mjg1L089SU1NXX7DTrzHFg6lpZKli84a0U7K6vw=",
  "f5avraaaaaaaaaaaaaaaa_session_=GKAELMPDAJLODIBPPBBMKDAAOPBIHDBDGLGCCFCPKNKAJOBLAIKEMEENNALMMHKADFIDJBIJJCCGLKIKADPADOJBDGMCFNOJEDEGIGGCDACMOEHCEDDAHHOFIKBEBLCP",
  sep = "; "
)

# 4) Ejecuta la petición
resp <- GET(
  url,
  add_headers(
    Accept             = "application/json, text/plain, */*",
    `Accept-Language`  = "es-ES,es;q=0.9,en;q=0.8",
    Authorization      = paste("Bearer", token),
    Connection         = "keep-alive",
    Referer            = "https://intranet.imm.gub.uy/app/limpieza-gestion-operativa/",
    `Sec-Fetch-Dest`   = "empty",
    `Sec-Fetch-Mode`   = "cors",
    `Sec-Fetch-Site`   = "same-origin",
    `User-Agent`       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36",
    `sec-ch-ua`        = "\"Google Chrome\";v=\"137\", \"Chromium\";v=\"137\", \"Not/A)Brand\";v=\"24\"",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "\"Windows\"",
    Cookie             = cookie_str
  ),
  verbose()
)

stop_for_status(resp)

# 5) Parsear el JSON de respuesta
data <- content(resp, "parsed", simplifyVector = TRUE)
str(data)









library(httr)

# 1) Datos que debes completar:
client_id     <- "oPalorYq017zMjhp8ODeM_bEfooa"
client_secret <- "TU_CLIENT_SECRET"
user          <- "TU_USUARIO"       # p.ej. "im4445285"
pass          <- "TU_CONTRASEÑA"    # p.ej. "Nico1919*"

# 2) Endpoint de token (ajusta si es otro)
token_url <- "https://is.montevideo.gub.uy/oauth2/token"

# 3) Petición Password Grant para obtener el JWT
resp_tok <- POST(
  url = token_url,
  authenticate(client_id, client_secret),
  body = list(
    grant_type = "password",
    username   = user,
    password   = pass,
    scope      = "openid"
  ),
  encode = "form"
)
stop_for_status(resp_tok)

# 4) Extraes el access_token
tok_data      <- content(resp_tok, "parsed", simplifyVector=TRUE)
access_token  <- tok_data$access_token
cat("Token válido hasta:", Sys.time() + tok_data$expires_in, "\n")

# 5) Ya tienes el Bearer token listo para usar en tu GET:
resp_api <- GET(
  "https://intranet.imm.gub.uy/app/…/motivoNoLevante",
  add_headers(
    Authorization = paste("Bearer", access_token),
    Accept        = "application/json"
  )
)
stop_for_status(resp_api)
datos <- content(resp_api, "parsed", simplifyVector=TRUE)
str(datos)





















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





# 1) Instala curlconverter si no lo tienes
install.packages("curlconverter")

# 2) Carga librerías
library(httr)
library(xml2)
library(curlconverter)

# ——————————————————————————————
#   Tu login (igual que antes)
# ——————————————————————————————
h2 <- handle("https://is.montevideo.gub.uy")
url_login <- "https://is.montevideo.gub.uy/authenticationendpoint/login.do?RelayState=…&sessionDataKey=…"
resp0 <- GET(url_login, handle = h2, user_agent("Mozilla/5.0"), verbose()); stop_for_status(resp0)
doc    <- read_html(content(resp0, "text", encoding="UTF-8"))
form   <- xml_find_first(doc, ".//form")
url_post <- url_absolute(xml_attr(form, "action"), url_login)
payload <- list(
  tocommonauth   = xml_attr(xml_find_first(form, ".//input[@name='tocommonauth']"), "value"),
  username       = "im4445285",
  password       = "Nico1919*",
  sessionDataKey = xml_attr(xml_find_first(form, ".//input[@name='sessionDataKey']"), "value")
)
# 3) Haz el POST pero guardando la petición en un objeto de petición
req <- VERB(
  verb   = "POST",
  url    = url_post,
  body   = payload,
  encode = "form",
  handle = h2,
  user_agent("Mozilla/5.0"),
  config(verbose())
)
resp1 <- req  # ejecuta la petición
stop_for_status(resp1)

# ——————————————————————————————
# 4) Extrae el curl de esa petición
# ——————————————————————————————
# curlconverter puede tomar el objeto `req` y devolverte la cadena:
curl_cmd <- to_curl(req)
cat(curl_cmd)




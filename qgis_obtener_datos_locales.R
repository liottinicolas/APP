# ==================================================================================
#                     SCRIPT COMPLETO: LEER TABLAS ESPACIALES
# ==================================================================================
# Este script muestra cómo:
#  1) Conectarse a Postgres/PostGIS
#  2) Extraer automáticamente las tablas espaciales de `geometry_columns`
#  3) Definir una función robusta para leer cualquiera de esas tablas como `sf`
#  4) Probar la función con ejemplos, incluyendo diagnóstico si no aparece `geom_wkb`
# ==================================================================================

# 1) Instalar / cargar paquetes necesarios
if (!requireNamespace("DBI", quietly = TRUE))    install.packages("DBI")
if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
if (!requireNamespace("sf", quietly = TRUE))     install.packages("sf")

library(DBI)
library(RPostgres)
library(sf)

# ==================================================================================
# 2) PARÁMETROS DE CONEXIÓN A POSTGRES/POSTGIS
# ==================================================================================
dbname   <- "qgis"
host     <- "pdbqgistest.imm.gub.uy"
port     <- 5411
user     <- "qgis"
password <- "mapa22"
sslmode  <- "disable"

# 3) ABRIR CONEXIÓN
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname   = dbname,
  host     = host,
  port     = port,
  user     = user,
  password = password,
  sslmode  = sslmode
)

# ==================================================================================
# 4) OBTENER LAS TABLAS ESPACIALES DESDE geometry_columns
# ==================================================================================
#    Consultamos `geometry_columns` para listar esquema, nombre de tabla, 
#    nombre de columna geométrica, tipo y srid. Luego armamos `tablas_geom`.
sql_geom <- "
  SELECT 
    f_table_schema      AS esquema,
    f_table_name        AS nombre_tabla,
    f_geometry_column   AS columna_geom,
    type                AS tipo_geom,
    srid                AS srid
  FROM geometry_columns
  WHERE f_table_schema = 'public';
"

geom_info <- dbGetQuery(conn, sql_geom)

# Construir data.frame con la forma deseada:
tablas_geom <- data.frame(
  tabla         = paste0(geom_info$esquema, ".", geom_info$nombre_tabla),
  columna_geom  = geom_info$columna_geom,
  tipo_geom     = geom_info$tipo_geom,
  srid          = geom_info$srid,
  stringsAsFactors = FALSE
)

# Mostrar rápidamente qué tablas espaciales se encontraron
cat("Tablas espaciales encontradas en public.geometry_columns:\n")
print(tablas_geom)
# Ejemplo de salida:
#                     tabla         columna_geom     tipo_geom srid
# 1   public.RBB_puntos             geom         MULTIPOLYGON 4326
# 2   public.sg_23_mdeo             geom         MULTIPOLYGON 4326
# 3 public.Relevamiento PEV         geom         MULTIPOLYGON 4326
# 4 public.otratabla_geom           the_geom      POINT        4326
#    ...

# ==================================================================================
# 5) FUNCIÓN get_spatial_table(): LEER UNA TABLA ESPACIAL COMO sf
# ==================================================================================
# Parámetros:
#   - nombre_completo: cadena "esquema.tabla" (tal cual figura en tablas_geom$tabla)
#   - tablas_geom     : data.frame con las columnas obtenidas de `geometry_columns`
#   - dbname, host, port, user, password, sslmode: parámetros de conexión
#
# Comportamiento:
#   1) Verifica que nombre_completo exista en tablas_geom$tabla.
#   2) Divide en esquema y tabla (primer punto).
#   3) Cita correctamente esquema, tabla y columna geométrica.
#   4) Ejecuta SELECT + ST_AsBinary(...) AS geom_wkb.
#   5) Verifica que el df contenga 'geom_wkb'; si no, imprime nombres de columnas.
#   6) Convierte a sf con `st_as_sf(df, wkb="geom_wkb", crs=srid)`.
#
# Errores:
#   - Si nombre_completo no está en tablas_geom$tabla          → stop(...)
#   - Si la consulta no produce 'geom_wkb'                     → imprime names(df) y stop(...)
#   - Si st_as_sf falla                                        → stop(...)
# ==================================================================================
get_spatial_table <- function(nombre_completo,
                              tablas_geom,
                              dbname   = "qgis",
                              host     = "pdbqgistest.imm.gub.uy",
                              port     = 5411,
                              user     = "qgis",
                              password = "mapa22",
                              sslmode  = "disable") {
  # --- 5.1) Verificar que exista en tablas_geom$tabla ---
  idx <- which(tablas_geom$tabla == nombre_completo)
  if (length(idx) == 0) {
    stop(sprintf("Error: '%s' no figura en tablas_geom$tabla.", nombre_completo))
  }
  fila         <- tablas_geom[idx, , drop = TRUE]
  columna_geom <- fila$columna_geom   # p.ej. "geom" o "the_geom"
  srid_actual  <- fila$srid           # p.ej. 4326
  
  # --- 5.2) Abrir conexión ---
  conn2 <- dbConnect(
    RPostgres::Postgres(),
    dbname   = dbname,
    host     = host,
    port     = port,
    user     = user,
    password = password,
    sslmode  = sslmode
  )
  on.exit(dbDisconnect(conn2), add = TRUE)
  
  # --- 5.3) Separar esquema y tabla usando el primer punto ---
  pos_punto <- regexpr("\\.", nombre_completo)
  if (pos_punto[1] == -1) {
    esquema <- "public"
    tabla   <- nombre_completo
  } else {
    esquema <- substring(nombre_completo, 1, pos_punto[1] - 1)
    tabla   <- substring(nombre_completo, pos_punto[1] + 1, nchar(nombre_completo))
  }
  
  # --- 5.4) Construir identificadores citados ---
  esquema_q      <- dbQuoteIdentifier(conn2, esquema)
  tabla_q        <- dbQuoteIdentifier(conn2, tabla)
  columna_geom_q <- dbQuoteIdentifier(conn2, columna_geom)
  full_name      <- DBI::SQL(paste0(esquema_q, ".", tabla_q))
  # Ejemplo: "\"public\".\"Relevamiento PEV\""
  
  # --- 5.5) Armar la consulta SQL con ST_AsBinary(...) AS geom_wkb ---
  sql_select <- DBI::SQL(paste0(
    "SELECT *,\n",
    "       ST_AsBinary(", columna_geom_q, ") AS geom_wkb\n",
    "  FROM ", full_name, ";"
  ))
  
  # --- 5.6) Ejecutar la consulta y capturar errores ---
  df_wkb <- tryCatch({
    dbGetQuery(conn2, sql_select)
  }, error = function(e) {
    stop("Error al ejecutar SELECT de geom_wkb:\n", e$message)
  })
  
  # --- 5.7) Verificar que exista la columna 'geom_wkb' ---
  if (!"geom_wkb" %in% names(df_wkb)) {
    cat("Advertencia: la consulta NO devolvió columna 'geom_wkb'.\n")
    cat("Columnas obtenidas:\n")
    print(names(df_wkb))
    stop("Abortando: la consulta no produjo 'geom_wkb'.")
  }
  
  # --- 5.8) Convertir a sf usando st_as_sf() con el CRS correcto ---
  sf_obj <- tryCatch({
    st_as_sf(df_wkb, wkb = "geom_wkb", crs = srid_actual)
  }, error = function(e) {
    stop("Error al convertir a sf (st_as_sf):\n", e$message)
  })
  
  return(sf_obj)
}

# ==================================================================================
# 6) CERRAR LA CONEXIÓN INICIAL (ya usamos `conn` para levantar `tablas_geom`)
# ==================================================================================
dbDisconnect(conn)

# ==================================================================================
# 7) EJEMPLOS DE USO DE `get_spatial_table`
# ==================================================================================

# 7.1) Listar las tablas espaciales disponibles:
cat("Tablas espaciales disponibles:\n")
print(tablas_geom$tabla)




##########################################
# HASTA ACÁ FUNCIONA #
# LEE LAS TABLAS QUE ESTÁN EN POSGRES.
##########################################


##### Se cambia el nombre en 3 lugares ######

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "qgis",
  host     = "pdbqgistest.imm.gub.uy",
  port     = 5411,
  user     = "qgis",
  password = "mapa22",
  sslmode  = "disable"
)

# 1) Ver todas las columnas de public."Relevamiento PEV"
cols_relevamiento <- dbGetQuery(conn, '
  SELECT column_name 
  FROM information_schema.columns 
  WHERE table_schema = \'public\' 
    AND table_name   = \'mobile_v_mdg_vias_multilinestring\';
')
print(cols_relevamiento)

# 2) Verificar en geometry_columns si figura y con qué columna geométrica
cols_relevamiento_geom <- dbGetQuery(conn, "
  SELECT f_geometry_column, srid 
  FROM geometry_columns 
  WHERE f_table_schema = 'public' 
    AND f_table_name   = 'imm:mobile_v_mdg_vias_multilinestring';
")
print(cols_relevamiento_geom)


# 4.1) Traer atributos y WKB en un data.frame
df_wkb <- dbGetQuery(conn, '
  SELECT *, ST_AsBinary("geom") AS geom_wkb
    FROM public."imm:mobile_v_mdg_vias_multilinestring";
')

# 4.2) Verificar que exista la columna geom_wkb
if (!"geom_wkb" %in% names(df_wkb)) {
  stop("No se encontró la columna 'geom_wkb' en el resultado.")
}

geom_sfc <- st_as_sfc(df_wkb$geom, EWKB = TRUE, crs = 32721)

# 3) Construir el objeto sf: 
#    - Conservamos todas las columnas de atributos, excepto la que usaremos para geometría.
#    - Llamamos a st_sf() indicando geom_sfc como la columna geométrica.

sf_circuitos <- st_sf(
  # todas las columnas NO geom, en un data.frame:
  df_wkb[ , setdiff(names(df_wkb), "geom"), drop = FALSE ],
  geometry = geom_sfc
)

# 4) Verificar que ahora tenemos un objeto sf
print(sf_circuitos)
plot(st_geometry(sf_circuitos), main = "Circuitos con turnos y frecuencias")



# ============================================
# 6) CERRAR CONEXIÓN
# ============================================
dbDisconnect(conn)









library(DBI); library(RPostgres)

conn2 <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "qgis",
  host     = "pdbqgistest.imm.gub.uy",
  port     = 5411,
  user     = "qgis",
  password = "mapa22",
  sslmode  = "disable"
)

# Ver columnas de ide.mi_tabla
cols_ide <- dbGetQuery(conn2, "
  SELECT column_name
  FROM information_schema.columns
  WHERE table_schema = 'ide'
    AND table_name   = 'mi_tabla';
")
print(cols_ide)

dbDisconnect(conn)





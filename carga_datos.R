# nolint start: line_length_linter, object_name_linter
#

# Cargar paquetes necesarios
library(dplyr)

# Definir la ruta del proyecto
ruta_proyecto <- getwd()

# Función auxiliar para establecer rutas y cargar datos
cargar_datos <- function(
  modulo,
  nombre_archivo_funcion = NULL, 
 nombre_archivo_historico = NULL) {
  # Determinar nombres de archivos basados en el módulo
  if (is.null(nombre_archivo_funcion)) {
    # Usar el nombre original exacto de cada módulo
    if (modulo == "10484_llenado") {
      nombre_archivo_funcion <- "llenado"
    } else if (modulo == "10393_ubicaciones") {
      nombre_archivo_funcion <- "ubicaciones"
    } else if (modulo == "10334_viajesEnUnPeriodo") {
      nombre_archivo_funcion <- "viajesEnUnPeriodo"
    } else if (modulo == "10338_incidencias") {
      nombre_archivo_funcion <- "incidencias"
    } else {
      nombre_archivo_funcion <- modulo
    }
  }
  
  if (is.null(nombre_archivo_historico)) {
    # Usar el nombre original exacto para historicos
    if (modulo == "10334_viajesEnUnPeriodo") {
      nombre_archivo_historico <- "viajes"
    } else if (modulo == "10393_ubicaciones") {
      nombre_archivo_historico <- "ubicaciones"
    } else if (modulo == "10484_llenado") {
      nombre_archivo_historico <- "llenado"
    } else if (modulo == "10338_incidencias") {
      nombre_archivo_historico <- "incidencias"
    } else {  
      nombre_archivo_historico <- modulo
    }
  }
  
  # Construir rutas
  ruta_base <- file.path("scripts/db", modulo)
  ruta_funciones <- file.path(ruta_proyecto, ruta_base, paste0("funciones_db_", nombre_archivo_funcion, ".R"))
  ruta_carpeta_archivos <- file.path(ruta_proyecto, "archivos", modulo)
  
  # Construir rutas para archivos RDS
  if (modulo == "10334_viajesEnUnPeriodo") {
    # Caso especial para viajes
    ruta_RDS_datos <- file.path(ruta_proyecto, ruta_base, paste0("historico_", nombre_archivo_historico, ".rds"))
    ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, ruta_base, paste0("archivos_procesados_", nombre_archivo_historico, ".rds"))
  } else {
    # Caso general
    ruta_RDS_datos <- file.path(ruta_proyecto, ruta_base, paste0("historico_", nombre_archivo_historico, ".rds"))
    ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, ruta_base, paste0("archivos_aplicados_historico_", nombre_archivo_historico, ".rds"))
  }
  
  return(list(
    ruta_funciones = ruta_funciones,
    ruta_carpeta_archivos = ruta_carpeta_archivos,
    ruta_RDS_datos = ruta_RDS_datos,
    ruta_RDS_planillas_procesadas = ruta_RDS_planillas_procesadas
  ))
}

## CARGAS PRINCIPALES DE DATOS

## 1. Datos de llenado
rutas_llenado <- cargar_datos("10484_llenado")
# Definir globalmente la variable que necesita actualizar_planillas_RDS
ruta_RDS_planillas_procesadas <- rutas_llenado$ruta_RDS_planillas_procesadas
historico_llenado <- actualizar_planillas_RDS(
  ruta_proyecto, 
  rutas_llenado$ruta_funciones, 
  rutas_llenado$ruta_carpeta_archivos, 
  rutas_llenado$ruta_RDS_datos
)

## 2. Datos de ubicaciones
rutas_ubicaciones <- cargar_datos("10393_ubicaciones")
# Definir globalmente la variable para esta sección
ruta_RDS_planillas_procesadas <- rutas_ubicaciones$ruta_RDS_planillas_procesadas
historico_ubicaciones <- actualizar_planillas_RDS(
  ruta_proyecto, 
  rutas_ubicaciones$ruta_funciones, 
  rutas_ubicaciones$ruta_carpeta_archivos, 
  rutas_ubicaciones$ruta_RDS_datos
)

# prueba <- read_rds(rutas_ubicaciones$ruta_RDS_datos)
# prueba <- historico_ubicaciones_cambio_de_estado
# prueba <- prueba %>% 
#   filter(Fecha < "2025-05-05")
# 
# saveRDS(prueba, rutas_ubicaciones$ruta_RDS_datos)

# Actualizo las modificaciones de ubicaciones
ruta_RDS_modificaciones_historicas <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/historico_modificaciones.rds")
historico_ubicaciones_cambio_de_estado <- funcion_guardar_historico_modificaciones(
  ruta_RDS_modificaciones_historicas,
  historico_ubicaciones,
  historico_llenado
)

## 3. Datos de viajes
rutas_viajes <- cargar_datos("10334_viajesEnUnPeriodo")
# Definir globalmente la variable para esta sección
ruta_RDS_planillas_procesadas <- rutas_viajes$ruta_RDS_planillas_procesadas
historico_viajes <- actualizar_planillas_RDS(
  ruta_proyecto, 
  rutas_viajes$ruta_funciones, 
  rutas_viajes$ruta_carpeta_archivos,
  rutas_viajes$ruta_RDS_datos
)

## 4. Actualización de estado diario
ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/estado_diario/historico_estado_diario.rds")

# Usar siempre la versión optimizada
ruta_funciones_estadodiario <- file.path(ruta_proyecto, "scripts/estado_diario/funciones_cargadatos_estado_diario_optimizado.R")
escribir_log("INFO", "Usando funciones optimizadas para estado diario")

source(ruta_funciones_estadodiario)
historico_estado_diario <- actualizar_planillas_RDS_estado_diario(ruta_RDS_datos)

## 5. Datos de incidencias
rutas_incidencias <- cargar_datos("10338_incidencias")
# Definir globalmente la variable para esta sección
ruta_RDS_planillas_procesadas <- rutas_incidencias$ruta_RDS_planillas_procesadas
historico_incidencias <- actualizar_planillas_RDS(
  ruta_proyecto, 
  rutas_incidencias$ruta_funciones, 
  rutas_incidencias$ruta_carpeta_archivos, 
  rutas_incidencias$ruta_RDS_datos
)
historico_incidencias_completas <- actualizar_planillas_RDS_llenado_completas(
  rutas_incidencias$ruta_RDS_datos
)

## 6. Actualización de incidencias por GID
ruta_RDS_incidencias <- file.path(ruta_proyecto, "scripts/incidencias_por_gid/historico_incidencias_por_gid.rds")
historico_incidencias_por_gid <- actualizar_planillas_RDS_incidencias_por_gid(ruta_RDS_incidencias)

## 7. Llenado con incidencias
ruta_RDS_llenado_completo <- file.path(ruta_proyecto, "scripts/llenado_completo/historico_llenado_completo.rds")
historico_completo_llenado_incidencias <- actualizar_planillas_RDS_llenado_completas(ruta_RDS_llenado_completo)


# Actualizo las modificaciones de ubicaciones
ruta_RDS_ubicaciones_conthegeom <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/ubicaciones_con_thegheom.rds")
ubicaciones_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
saveRDS(ubicaciones_existentes, ruta_RDS_ubicaciones_conthegeom)

# nolint end


## llenado
## Cargo el db de llenado sin modificaciones
ruta_funciones <- file.path(ruta_proyecto, "scripts/db/10484_llenado/funciones_db_llenado.R")
ruta_carpeta_archivos <- file.path(ruta_proyecto, "archivos/10484_llenado")
ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/db/10484_llenado/historico_llenado.rds")
ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, "scripts/db/10484_llenado/archivos_aplicados_historico_llenado.rds")

historico_llenado <- actualizar_planillas_RDS(ruta_proyecto,ruta_funciones,ruta_carpeta_archivos,ruta_RDS_datos)
rm(ruta_carpeta_archivos,ruta_funciones,ruta_RDS_datos,ruta_RDS_planillas_procesadas)



## ubicaciones
## Cargo el db de ubcaciones sin modificaciones

ruta_funciones <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/funciones_db_ubicaciones.R")
ruta_carpeta_archivos <- file.path(ruta_proyecto, "archivos/10393_ubicaciones")
ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/historico_ubicaciones.rds")
ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/archivos_aplicados_historico_ubicaciones.rds")

## NO TIENE THE_GEOM
historico_ubicaciones <- actualizar_planillas_RDS(ruta_proyecto,ruta_funciones,ruta_carpeta_archivos,ruta_RDS_datos)

# Actualizo las modificaciones
ruta_RDS_modificaciones_historicas <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/historico_modificaciones.rds")
historico_ubicaciones_cambio_de_estado <- funcion_guardar_historico_modificaciones(ruta_RDS_modificaciones_historicas,historico_ubicaciones,historico_llenado)







########## viajesenunperiodo

ruta_funciones <- file.path(ruta_proyecto, "scripts/db/10334_viajesEnUnPeriodo/funciones_db_viajesEnUnPeriodo.R")
ruta_carpeta_archivos <- file.path(ruta_proyecto, "archivos/10334_viajesEnUnPeriodo")
ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/db/10334_viajesEnUnPeriodo/historico_viajes.rds")
ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, "scripts/db/10334_viajesEnUnPeriodo/archivos_procesados_viajes.rds")

historico_viajes <- actualizar_planillas_RDS(ruta_proyecto,ruta_funciones,ruta_carpeta_archivos,ruta_RDS_datos)


##### informe diario actualización.

ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/estado_diario/historico_estado_diario.rds")
ruta_funciones_estadodiario <- file.path(ruta_proyecto, "scripts/estado_diario/funciones_cargadatos_estado_diario.R")
source(ruta_funciones_estadodiario)
historico_estado_diario <- actualizar_planillas_RDS_estado_diario(ruta_RDS_datos)

### Cargo todas las direcciones con los gids.


########## incidencias

ruta_funciones <- file.path(ruta_proyecto, "scripts/db/10338_incidencias/funciones_db_incidencias.R")
ruta_carpeta_archivos <- file.path(ruta_proyecto, "archivos/10338_incidencias")
ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/db/10338_incidencias/historico_incidencias.rds")
ruta_RDS_planillas_procesadas <- file.path(ruta_proyecto, "scripts/db/10338_incidencias/archivos_aplicados_historico_incidencias.rds")

historico_incidencias <- actualizar_planillas_RDS(ruta_proyecto,ruta_funciones,ruta_carpeta_archivos,ruta_RDS_datos)

##### incidencias actualización.

ruta_RDS_incidencias <- file.path(ruta_proyecto, "scripts/incidencias_por_gid/historico_incidencias_por_gid.rds")
historico_incidencias_por_gid <- actualizar_planillas_RDS_incidencias_por_gid(ruta_RDS_incidencias)
# 
# ##### llenado con incidencias
ruta_RDS_llenado_completo <- file.path(ruta_proyecto, "scripts/llenado_completo/historico_llenado_completo.rds")
historico_completo_llenado_incidencias <- actualizar_planillas_RDS_llenado_completas(ruta_RDS_llenado_completo)


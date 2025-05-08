# nolint start: line_length_linter, object_name_linter

  
  ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/estado_diario/historico_estado_diario.rds")
  web_historico_estado_diario <- read_rds(ruta_RDS_datos)
  
  ruta_RDS_llenado_completo <- file.path(ruta_proyecto, "scripts/llenado_completo/historico_llenado_completo.rds")
  web_historico_completo_llenado_incidencias <- read_rds(ruta_RDS_llenado_completo)
  
  # Cálculo de fechas relevantes
  ultima_fecha_registro <- max(web_historico_estado_diario$Fecha, na.rm = TRUE)
  fecha_informe_diario <- ultima_fecha_registro + 1
  escribir_log("INFO", paste("Última fecha de registro:", ultima_fecha_registro, 
                             "- Fecha informe diario:", fecha_informe_diario))

  inicio <- as.Date(CONFIGURACION$FECHA_INICIO_ANALISIS)
  
  ruta_RDS_ubicaciones_conthegeom <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/ubicaciones_con_thegheom.rds")
  ubicaciones_existentes <- readRDS(ruta_RDS_ubicaciones_conthegeom)

  # Carga de circuitos planificados
  ruta_RDS_circuitos_planificados <- file.path(ruta_proyecto, "scripts/para_mapear/circuitos_planificados.rds")
  web_circuitos_planificados <- readRDS(ruta_RDS_circuitos_planificados)

# nolint end
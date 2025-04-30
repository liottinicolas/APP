# nolint start: line_length_linter, object_name_linter


######################## CARGA DIRECTA DE LAS CONSULTAS EXPORTABLES ################################

########################
# SECCIÓN 0: CARGA DE CONFIGURACIÓN
########################

# Cargar archivo de configuración
tryCatch({
  source("config.R")
}, error = function(e) {
  stop("Error al cargar archivo de configuración: ", e$message)
})

########################
# SECCIÓN 1: SISTEMA DE LOGGING
########################

# Cargar archivo de logging
tryCatch({
  source("logging.R")
}, error = function(e) {
  stop("Error al cargar archivo de logging: ", e$message)
})

########################
# SECCIÓN 2: CARGA DE DEPENDENCIAS
########################

# Función para cargar archivos con manejo de errores
cargar_archivo <- function(ruta_archivo) {
  tryCatch({
    source(ruta_archivo)
    escribir_log("INFO", paste("Archivo cargado con éxito:", ruta_archivo))
  }, error = function(e) {
    manejar_error(e, paste("al cargar", ruta_archivo))
  })
}

# Cargo paquetes y funciones básicas
cargar_archivo("global.R")

# Funciones para actualizar RDS
cargar_archivo("funciones_carga_datos.R")

# Funciones utilitarias generales
cargar_archivo("funciones_utiles.R")

# Funciones para la interfaz web
cargar_archivo("funciones_para_web.R")

# Limpieza de duplicados en historico_estado_diario
tryCatch({
  escribir_log("INFO", "Verificando y limpiando duplicados en historico_estado_diario")
  ruta_RDS_estado_diario <- file.path(ruta_proyecto, "scripts/estado_diario/historico_estado_diario.rds")
  
  if(file.exists(ruta_RDS_estado_diario)) {
    # Leer el archivo actual
    historico_estado_diario_actual <- readRDS(ruta_RDS_estado_diario)
    
    # Limpiar duplicados
    historico_estado_diario_limpio <- historico_estado_diario_actual %>%
      distinct(gid, Fecha, .keep_all = TRUE) %>%
      arrange(Fecha, gid)
    
    # Verificar si se eliminaron duplicados
    num_registros_antes <- nrow(historico_estado_diario_actual)
    num_registros_despues <- nrow(historico_estado_diario_limpio)
    
    if(num_registros_antes > num_registros_despues) {
      # Guardar la versión limpia
      saveRDS(historico_estado_diario_limpio, file = ruta_RDS_estado_diario)
      escribir_log("INFO", paste("Se eliminaron", num_registros_antes - num_registros_despues, 
                                "registros duplicados en historico_estado_diario"))
    } else {
      escribir_log("INFO", "No se encontraron duplicados en historico_estado_diario")
    }
  }
}, error = function(e) {
  manejar_error(e, "al limpiar duplicados en historico_estado_diario")
})

# Carga de datos principal
cargar_archivo("carga_datos.R")


########################
# SECCIÓN 4: LIMPIEZA DE VARIABLES TEMPORALES
########################

tryCatch({
  # Eliminar variables de ruta que ya no se necesitan
  escribir_log("DEBUG", "Limpiando variables temporales")
  rm(ruta_carpeta_archivos, ruta_funciones, ruta_RDS_datos, 
     ruta_RDS_planillas_procesadas)
  
  # Eliminar funciones auxiliares que ya cumplieron su propósito
  rm(actualizar_planillas_RDS, eliminar_ultimo_dia_llenado,
     funcion_actualizar_incidencias_10334, funcion_actualizar_llenado_10484,
     funcion_actualizar_ubicaciones_10393, 
     funcion_actualizar_viajesEnUnPeriodo_10334,
     funcion_agregar_gid_incidencias)
  
  # Código comentado para verificación de registros duplicados
  # repes <- imprimir_repetidos(historico_estado_diario)
}, error = function(e) {
  manejar_error(e, "limpiar variables temporales")
})

########################
# SECCIÓN 5: ANÁLISIS DE DATOS
########################

tryCatch({
  # Configuración de período de análisis
  escribir_log("INFO", "Iniciando análisis de datos")
  inicio <- as.Date(CONFIGURACION$FECHA_INICIO_ANALISIS)
  fin <- max(historico_estado_diario$Fecha)

  
  # Análisis de responsabilidades por tipo de equipo
  for (tipo_equipo in CONFIGURACION$TIPOS_EQUIPO) {
    escribir_log("INFO", paste("Procesando responsabilidades para", tipo_equipo))
    
    # Solo calculamos los resultados pero no exportamos automáticamente
    ubicaciones_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
    
    resultado <- funcion_mostrar_responsables_por_incidencias(
      historico_completo_llenado_incidencias,
      historico_estado_diario,
      fin,
      tipo_equipo
    )
    
    # Mostrar la fecha más antigua
    escribir_log("INFO", paste("Fecha más antigua para", tipo_equipo, ":", resultado$fecha_antigua))
    
    escribir_log("INFO", paste("Procesamiento completado para", tipo_equipo))
  }
}, error = function(e) {
  manejar_error(e, "analizar datos")
})



# Mensaje de confirmación de carga completa
escribir_log("INFO", paste("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO))
cat("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO, "\n")

# rm(historico_ubicaciones,historico_viajes,historico_estado_diario,historico_incidencias,historico_llenado)

# ASD <- estado_diario_global %>% 
#   filter(Fecha == "2025-04-03") %>% 
#   filter(Acumulacion == 1) %>% 
#   group_by(Circuito_corto) %>% 
#   summarise(total = n())


# nolint end

# funcion_imprimir_datosporgid(162392)

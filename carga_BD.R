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

# Configuración del sistema de logging
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(CONFIGURACION$DIRECTORIO_LOGS, paste0("log_", timestamp, ".txt"))

# Función para escribir logs
escribir_log <- function(nivel, mensaje, detalles = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Niveles de log y sus valores numéricos
  niveles <- list(DEBUG = 1, INFO = 2, WARN = 3, ERROR = 4)
  
  # Solo escribir si el nivel del mensaje es mayor o igual al nivel configurado
  if (niveles[[nivel]] >= niveles[[CONFIGURACION$NIVEL_LOG]]) {
    # Formatear mensaje
    log_mensaje <- sprintf("[%s] [%s] %s", timestamp, nivel, mensaje)
    
    # Añadir detalles si existen
    if (!is.null(detalles)) {
      log_mensaje <- paste0(log_mensaje, "\nDetalles: ", paste(detalles, collapse = " "))
    }
    
    # Escribir en archivo
    write(log_mensaje, file = log_file, append = TRUE)
    
    # Mostrar en consola si estamos en modo desarrollo
    if (CONFIGURACION$MODO == "desarrollo" || nivel == "ERROR") {
      cat(log_mensaje, "\n")
    }
  }
}

# Función para manejo de errores global
manejar_error <- function(e, contexto) {
  mensaje_error <- paste("Error en", contexto, ":", e$message)
  escribir_log("ERROR", mensaje_error, detalles = as.character(e$call))
  if (CONFIGURACION$MODO == "desarrollo") {
    stop(mensaje_error)
  }
}

# Iniciar log
escribir_log("INFO", paste("Iniciando script carga_BD.R en modo", CONFIGURACION$MODO))

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

# Carga de datos principal
cargar_archivo("carga_datos.R")

########################
# SECCIÓN 3: PROCESAMIENTO DE DATOS
########################

tryCatch({
  # Obtener ubicaciones y procesar estado diario
  escribir_log("INFO", "Procesando ubicaciones y estado diario")
  ubicaciones_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
  estado_diario_global <- funcion_agregar_the_geom_a_faltantes(
    historico_estado_diario,
    ubicaciones_existentes$ubicaciones_con_thegeom
  )
  
  # Cálculo de fechas relevantes
  ultima_fecha_registro <- max(historico_estado_diario$Fecha, na.rm = TRUE)
  fecha_informe_diario <- ultima_fecha_registro + 1
  escribir_log("INFO", paste("Última fecha de registro:", ultima_fecha_registro, 
                            "- Fecha informe diario:", fecha_informe_diario))
}, error = function(e) {
  manejar_error(e, "procesar datos iniciales")
})

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
  fin <- max(estado_diario_global$Fecha)

  
  # Análisis de responsabilidades por tipo de equipo
  for (tipo_equipo in CONFIGURACION$TIPOS_EQUIPO) {
    escribir_log("INFO", paste("Procesando responsabilidades para", tipo_equipo))
    
    # Solo calculamos los resultados pero no exportamos automáticamente
    resultado <- funcion_mostrar_responsables_por_incidencias(
      historico_completo_llenado_incidencias,
      estado_diario_global,
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

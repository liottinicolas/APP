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

# Carga de datos principal
cargar_archivo("carga_datos.R")

# Mensaje de confirmación de carga completa
escribir_log("INFO", paste("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO))
cat("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO, "\n")

# nolint end

# funcion_imprimir_datosporgid(162392)

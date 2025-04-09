
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
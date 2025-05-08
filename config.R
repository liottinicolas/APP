########################
# ARCHIVO DE CONFIGURACIÓN DEL SISTEMA
########################

# Configuración de fechas y parámetros generales
CONFIGURACION <- list(
  # Fechas de análisis (formato YYYY-MM-DD)
  FECHA_INICIO_ANALISIS = "2025-02-20",
  FECHA_INICIO_HISTORICO_LLENADO = "2024-10-10",
  
  # Tipos de equipos para análisis
  TIPOS_EQUIPO = c("Grua", "Pluma"),
  
  # Directorios
  DIRECTORIO_SALIDA = "./salida/",
  DIRECTORIO_LOGS = "./logs/",
  DIRECTORIO_DATOS = "./datos/",
  
  # Archivos de datos
  ARCHIVO_LLENADO = "llenado.rds",
  ARCHIVO_ESTADO_DIARIO = "estado_diario.rds",
  ARCHIVO_UBICACIONES = "ubicaciones.rds",
  
  # Modo de ejecución: "produccion" o "desarrollo"
  MODO = "produccion",
  
  # Nivel de log: "DEBUG", "INFO", "WARN", "ERROR"
  NIVEL_LOG = "INFO",
  
  # Opciones de exportación
  FORMATO_FECHA_EXCEL = "dd/mm/yyyy",
  ESTILO_TABLA_EXCEL = "TableStyleLight9",
  
  # Configuración de manejo de errores
  DETENER_EN_ERROR = TRUE,
  MAX_INTENTOS_CONEXION = 3
)

# Crear directorios necesarios
crear_directorios <- function() {
  for (dir in c(CONFIGURACION$DIRECTORIO_SALIDA, 
                CONFIGURACION$DIRECTORIO_LOGS,
                CONFIGURACION$DIRECTORIO_DATOS)) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

# Validar configuración
validar_configuracion <- function() {
  # Validar fechas
  tryCatch({
    as.Date(CONFIGURACION$FECHA_INICIO_ANALISIS)
  }, error = function(e) {
    stop("Error en formato de fechas en configuración: ", e$message)
  })
  
  # Validar modo
  if (!CONFIGURACION$MODO %in% c("produccion", "desarrollo")) {
    stop("Modo debe ser 'produccion' o 'desarrollo'")
  }
  
  # Validar nivel de log
  if (!CONFIGURACION$NIVEL_LOG %in% c("DEBUG", "INFO", "WARN", "ERROR")) {
    stop("Nivel de log debe ser 'DEBUG', 'INFO', 'WARN' o 'ERROR'")
  }
  
  # Otras validaciones pueden agregarse aquí
  
  return(TRUE)
}

# Ejecutar validación
validar_configuracion()

# Crear directorios
crear_directorios()

# Mensaje informativo
cat("Configuración cargada correctamente\n") 
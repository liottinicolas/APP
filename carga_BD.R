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
  fin <- max(historico_estado_diario$Fecha)
  fecha_consulta <- fin
  
  # Análisis de responsabilidades por tipo de equipo
  for (tipo_equipo in CONFIGURACION$TIPOS_EQUIPO) {
    escribir_log("INFO", paste("Procesando responsabilidades para", tipo_equipo))
    
    # Variable para almacenar resultados
    nombre_var <- paste0("prueba_", tolower(tipo_equipo))
    
    # Realizar análisis
    assign(nombre_var, funcion_mostrar_responsables_por_incidencias(
      historico_completo_llenado_incidencias,
      historico_estado_diario,
      inicio,
      fin,
      tipo_equipo
    ))
    
    # Exportar resultados
    funcion_exportar_incidencias_grua_o_pluma(get(nombre_var), tipo_equipo)
    escribir_log("INFO", paste("Exportación completada para", tipo_equipo))
  }
}, error = function(e) {
  manejar_error(e, "analizar datos")
})

########################
# SECCIÓN 6: ANÁLISIS ADICIONALES (TESTING)
########################

tryCatch({
  # Análisis para período específico
  escribir_log("INFO", "Realizando análisis adicionales")
  ini <- as.Date(CONFIGURACION$FECHA_INICIO_ANALISIS_DETALLADO)
  fin <- as.Date(CONFIGURACION$FECHA_FIN_ANALISIS_DETALLADO)
  prueba_grua_dia <- funcion_mostrar_responsables_por_incidencias(
    historico_completo_llenado_incidencias,
    historico_estado_diario,
    ini,
    fin,
    "Grua"
  )
}, error = function(e) {
  manejar_error(e, "realizar análisis adicionales")
})

########################
# SECCIÓN 7: FUNCIONES AUXILIARES PARA BÚSQUEDA POR GID
########################

#' Exporta datos de un GID específico a Excel
#'
#' @param gid_buscado El identificador único a buscar
#' @return No devuelve valor, genera archivo Excel
funcion_imprimir_datosporgid <- function(gid_buscado) {
  tryCatch({
    escribir_log("INFO", paste("Exportando datos para GID:", gid_buscado))
    
    imprimir <- historico_completo_llenado_incidencias %>% 
      filter(gid == gid_buscado) %>% 
      select(-Id_Motivo_no_levante, -Accion_requerida, -Responsable, 
             -Circuito, -DB, -Numero_caja)
    
    wb <- createWorkbook()
    
    # Añadir una hoja
    addWorksheet(wb, "Datos")
    
    # Escribir el data frame como tabla con formato
    writeDataTable(wb, sheet = "Datos", x = imprimir, 
                  tableStyle = "TableStyleLight9")
    
    # Ajustar automáticamente el ancho de todas las columnas
    setColWidths(wb, sheet = "Datos", cols = 1:ncol(imprimir), 
                 widths = "auto")
    
    # Crear un estilo para fechas
    dateStyle <- createStyle(numFmt = "dd/mm/yyyy")
    
    # Aplicar estilo a la columna de fechas
    addStyle(wb, sheet = "Datos", style = dateStyle, 
             cols = 2, rows = 2:(nrow(imprimir) + 1), 
             gridExpand = TRUE)
    
    nombre_archivo <- file.path(CONFIGURACION$DIRECTORIO_SALIDA, 
                               paste0("datos_gid_", gid_buscado, ".xlsx"))
    saveWorkbook(wb, file = nombre_archivo, overwrite = TRUE)
    
    escribir_log("INFO", paste("Archivo generado:", nombre_archivo))
  }, error = function(e) {
    manejar_error(e, paste("exportar datos para GID", gid_buscado))
  })
}

#' Obtiene datos de un GID específico
#'
#' @param gid_buscado El identificador único a buscar
#' @return Data frame con los datos del GID
funcion_obtener_datosporgid <- function(gid_buscado) {
  tryCatch({
    escribir_log("DEBUG", paste("Obteniendo datos para GID:", gid_buscado))
    
    imprimir <- historico_completo_llenado_incidencias %>% 
      filter(gid == gid_buscado) %>% 
      select(-Id_Motivo_no_levante, -Accion_requerida, -Responsable, 
             -Circuito, -DB, -Numero_caja)
    
    return(imprimir)
  }, error = function(e) {
    manejar_error(e, paste("obtener datos para GID", gid_buscado))
    return(NULL)
  })
}

########################
# SECCIÓN 8: EJECUCIÓN DE EJEMPLOS (PARA DESARROLLO)
########################

# Solo ejecutar ejemplos en modo desarrollo
if (CONFIGURACION$MODO == "desarrollo") {
  tryCatch({
    escribir_log("INFO", "Ejecutando ejemplos de desarrollo")
    
    # Procesar GIDs de ejemplo
    for (gid in CONFIGURACION$GIDS_EJEMPLO) {
      if (gid == CONFIGURACION$GIDS_EJEMPLO[1]) {
        funcion_imprimir_datosporgid(gid)
      } else {
        var_name <- paste0("asd_", gid)
        assign(var_name, funcion_obtener_datosporgid(gid))
      }
    }
  }, error = function(e) {
    manejar_error(e, "ejecutar ejemplos de desarrollo")
  })
}

# Mensaje de confirmación de carga completa
escribir_log("INFO", paste("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO))
cat("Script carga_BD.R ejecutado correctamente en modo:", CONFIGURACION$MODO, "\n")

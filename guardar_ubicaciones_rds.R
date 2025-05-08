## Función para guardar ubicaciones en formato RDS, actualizarlo y evitar duplicados
## Solo guarda el archivo, no devuelve la variable
guardar_ubicaciones_en_rds <- function(ubicaciones_existentes, directorio = "reporte_diario") {
  # Crear directorio si no existe
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Ruta completa del archivo
  ruta_archivo <- file.path(directorio, "ubicaciones_existentes.rds")
  
  # Verificar si ya existe el archivo
  if (file.exists(ruta_archivo)) {
    # Cargar ubicaciones anteriores
    ubicaciones_previas <- readRDS(ruta_archivo)
    
    # Combinar ubicaciones previas con nuevas
    ubicaciones_combinadas <- bind_rows(
      ubicaciones_previas$ubicaciones_con_thegeom,
      ubicaciones_existentes$ubicaciones_con_thegeom
    )
    
    # Eliminar duplicados basados en gid y Direccion
    ubicaciones_sin_duplicados <- ubicaciones_combinadas %>%
      distinct(gid, Direccion, .keep_all = TRUE)
    
    # Actualizar para guardar
    ubicaciones_a_guardar <- ubicaciones_existentes
    ubicaciones_a_guardar$ubicaciones_con_thegeom <- ubicaciones_sin_duplicados
    
    # Guardar el resultado actualizado
    saveRDS(ubicaciones_a_guardar, file = ruta_archivo)
    
    # Imprimir información sobre la actualización
    mensaje <- paste0(
      "Archivo actualizado: ",
      "Registros anteriores = ", nrow(ubicaciones_previas$ubicaciones_con_thegeom),
      ", Registros nuevos = ", nrow(ubicaciones_existentes$ubicaciones_con_thegeom) - nrow(ubicaciones_sin_duplicados),
      ", Total después de eliminar duplicados = ", nrow(ubicaciones_sin_duplicados)
    )
    cat(mensaje, "\n")
  } else {
    # Guardar el archivo por primera vez
    saveRDS(ubicaciones_existentes, file = ruta_archivo)
    cat("Archivo RDS creado por primera vez en:", ruta_archivo, "\n")
  }
  
  # No retorna ningún valor, simplemente guarda el archivo
  invisible(NULL)
} 
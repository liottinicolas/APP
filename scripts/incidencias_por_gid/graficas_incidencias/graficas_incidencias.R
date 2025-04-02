# nolint start: line_length_linter, object_name_linter

#############################################
# FUNCIONES #
#############################################

# Dada una fecha de fin e inicio, me devuelve los pendientes del dia, con la diferencia de días de atraso
funcion_Calcular_pendientes_por_dia <- function(db_historico_incidencias,
                                               db_informe_diario,
                                               responsable_incidencia,
                                               fecha_inicio_general,
                                               fecha_fin_general,
                                               lista_incidencias){
  # [Mantener el código existente de esta función sin cambios]
}

funcion_calcular_historico_y_diario_de_incidencias <- function(db_historico_incidencias,
                                                              db_informe_diario,
                                                              responsable_incidencia,
                                                              fecha_inicio_general,
                                                              fecha_fin_general,                                                               
                                                              lista_incidencias){
  # [Mantener el código existente de esta función sin cambios]
}

# Nueva función unificada para procesar cualquier tipo de responsable
funcion_actualizar_historico_incidencias <- function(responsable, lista_incidencias, nombre_archivo) {
  # Construir ruta del archivo RDS
  archivords_historico <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", 
                                   "graficas_incidencias", paste0("historicos_pendientes_generados_", nombre_archivo, ".rds"))
  
  # Cargar el histórico anterior si existe
  historico_pendientes_generados <- if (file.exists(archivords_historico)) {
    readRDS(archivords_historico)
  } else {
    character(0)
  }
  
  fecha_inicio_general <- as.Date("2024-10-10")
  fecha_fin_general <- max(historico_incidencias_completas$Dia_incidencia)
  
  ## Si existe el historial
  if(length(historico_pendientes_generados) > 0) {
    # Busco la última fecha con datos
    ultima_fecha_ingresada <- as.Date(max(historico_pendientes_generados$Fecha)) + 1
    
    # Actualizar solo los días nuevos
    while (ultima_fecha_ingresada <= fecha_fin_general) {
      aux_fecha <- ultima_fecha_ingresada
      
      # Obtener pendientes
      ultimo_dia_pendientes <- funcion_Calcular_pendientes_por_dia(
        historico_incidencias_completas,
        historico_estado_diario_con_direccion,
        responsable,
        fecha_inicio_general,
        aux_fecha,
        lista_incidencias
      )
      
      pendientes <- nrow(ultimo_dia_pendientes)
      
      # Calcular incidencias generadas hoy
      incidencias_generadashoy <- ultimo_dia_pendientes %>% 
        filter(diferencia_dias == 0)
      incidencias_generadashoy <- nrow(incidencias_generadashoy)
      
      # Añadir al histórico
      historico_pendientes_generados <- rbind(
        historico_pendientes_generados, 
        data.frame(
          Fecha = ultima_fecha_ingresada, 
          Generados = incidencias_generadashoy,
          Pendientes = pendientes
        )
      )
      
      ultima_fecha_ingresada <- ultima_fecha_ingresada + 1
    }
  } else {
    # Primera vez
    historico_pendientes_generados <- funcion_calcular_historico_y_diario_de_incidencias(
      historico_incidencias_completas,
      historico_estado_diario_con_direccion,
      responsable,
      fecha_inicio_general,
      fecha_fin_general,
      lista_incidencias
    )
  }
  
  # Guardar resultados
  saveRDS(historico_pendientes_generados, file = archivords_historico)
  return(historico_pendientes_generados)
}

#############################################
# PROCESAMIENTO DE DATOS
#############################################

# Procesar datos de Grua
historico_grua <- funcion_actualizar_historico_incidencias(
  responsable = "Grua",
  lista_incidencias = c("Sobrepeso"),
  nombre_archivo = "GRUA"
)

# Procesar datos de Pluma
historico_pluma <- funcion_actualizar_historico_incidencias(
  responsable = "Pluma",
  lista_incidencias = c("Buzonera Girada", "Contenedor Cruzado", "Contenedor Fuera de Alcance"),
  nombre_archivo = "PLUMA"
)

# Limpiar variables temporales
rm(historico_grua, historico_pluma)

# nolint end
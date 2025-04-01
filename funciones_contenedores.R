# Funciones para manejo de contenedores

#' Obtiene las ubicaciones para un día determinado
#' @param dia Día del informe
#' @return DataFrame con las ubicaciones del día
funcion_obtener_ubicaciones_por_dia <- function(dia){
  dia <- dia-1
  ubicaciones_por_dia <- historico_ubicaciones %>% 
    filter(Fecha == dia)
  return(ubicaciones_por_dia)
}

#' Obtiene los contenedores nuevos agregados con respecto al día anterior
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores nuevos
funcion_obtener_contenedores_agregados <- function(dia_informe){
  dia_informe_anterior <- dia_informe -1
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_nuevos_agregados <- ubicaciones_dia_informe %>%
    anti_join(
      ubicaciones_dia_anterior_informe,
      by = "gid"
    )
  
  return(contenedores_nuevos_agregados)
}

#' Obtiene los contenedores eliminados con respecto al día anterior
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores eliminados
funcion_obtener_contenedores_eliminados <- function(dia_informe){
  dia_informe_anterior <- dia_informe -1
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_nuevos_eliminados <- ubicaciones_dia_anterior_informe %>%
    anti_join(
      ubicaciones_dia_informe,
      by = "gid"
    )
  
  return(contenedores_nuevos_eliminados)
}

#' Obtiene los contenedores que cambiaron su estado
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores que cambiaron de estado
funcion_obtener_contenedores_cambio_de_estado_del_dia <- function(dia_informe){
  dia_informe_anterior <- dia_informe -1
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_con_cambio_de_estado <- ubicaciones_dia_informe %>%
    anti_join(
      ubicaciones_dia_anterior_informe,
      by = c("gid","Estado")
    )
  
  return(contenedores_con_cambio_de_estado)
}

#' Obtiene los contenedores que cambiaron de activos a mantenimiento
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores que cambiaron a mantenimiento
funcion_obtener_contenedores_activos_a_mantenimiento_del_dia <- function(dia_informe){
  df <- funcion_obtener_contenedores_cambio_de_estado_del_dia(dia_informe) %>% 
    filter(Estado == "Mantenimiento")
  return(df)
}

#' Obtiene los contenedores que cambiaron de mantenimiento a activos
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores que cambiaron a activos
funcion_obtener_contenedores_mantenimientos_a_activos_del_dia <- function(dia_informe){
  hoy <- dia_informe
  ayer <- dia_informe -1
  cambios_hoy <- funcion_obtener_contenedores_cambio_de_estado_del_dia(hoy)
  cambios_ayer <- funcion_obtener_contenedores_cambio_de_estado_del_dia(ayer)
  
  agregados_hoy <- funcion_obtener_contenedores_agregados_historico(inicio,hoy)
  agregados_ayer <- funcion_obtener_contenedores_agregados_historico(inicio,ayer)
  
  return(df)
}

#' Obtiene los contenedores levantados en un día específico
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores levantados
funcion_contenedores_levantados_del_dia <- function(dia_informe){
  levantados <- historico_llenado %>% 
    filter(Levantado == "S") %>% 
    filter(Fecha == dia_informe)
  return(levantados)
}

#' Obtiene los contenedores inactivos levantados en un día específico
#' @param dia_informe Día del informe
#' @return DataFrame con los contenedores inactivos levantados
funcion_contenedores_levantados_del_dia_inactivos <- function(dia_informe){
  levantados <- funcion_contenedores_levantados_del_dia(dia_informe)
  levantados <- levantados %>% 
    filter(Id_motivo_inactiva == 1)
  return(levantados)
}

#' Elimina GIDs inactivos de un conjunto de ubicaciones
#' @param ubicaciones DataFrame con ubicaciones
#' @param ubicacion_deldia DataFrame con ubicaciones del día
#' @return DataFrame con GIDs inactivos eliminados
eliminar_gids_inactivos <- function(ubicaciones, ubicacion_deldia){
  ubicaciones_repetidas <- ubicaciones %>%
    group_by(Circuito, Posicion, Fecha) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(gid, Circuito, Posicion, Fecha)
  
  ubicaciones_repetidas_sin_duplicados <- ubicaciones_repetidas %>% 
    distinct(gid, Circuito, Posicion, .keep_all = TRUE) %>% 
    select(-Fecha)
  
  ubicaciones_actualizado <- ubicaciones_repetidas_sin_duplicados %>%
    left_join(ubicacion_deldia %>% select(gid, Acumulacion), by = "gid")
  ubicaciones_actualizado <- ubicaciones_actualizado %>% 
    filter(!is.na(Acumulacion))
  
  ubicaciones_filtrado <- ubicaciones_actualizado %>%
    group_by(Circuito, Posicion) %>%
    slice_min(Acumulacion, with_ties = FALSE) %>%
    ungroup()
  
  ubicaciones_repetidas_filtrado <- ubicaciones_repetidas_sin_duplicados %>%
    anti_join(ubicaciones_filtrado, by = "gid")
  
  return(ubicaciones_repetidas_filtrado)
} 
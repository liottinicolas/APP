


# ruta_datos <- ruta_RDS_datos
# ubicaciones <- historico_ubicaciones
actualizar_planillas_RDS_estado_diario <- function(ruta_datos){
  
  ubicaciones <- historico_ubicaciones
  
  estado_diario_global <- if (file.exists(ruta_datos)) {
    readRDS(ruta_datos)
  } else {
    character(0)
  }
  
  ## Tiene datos?
  ## Si
  if (length(estado_diario_global) > 0) {
    
    ## Busco el último valor que tiene
    ultimo_dia_con_modificacion <- max(estado_diario_global$Fecha)
    inicio_dia_con_modificacion <- ultimo_dia_con_modificacion + 1
    
    fecha_fin <- max(historico_llenado$Fecha) + 1
    
    ## si no hay modificaciones?
    fechas <- sort(unique(ubicaciones$Fecha[ubicaciones$Fecha > ultimo_dia_con_modificacion]))
    
    if(length(fechas)>0){
      
      # Lista para almacenar los cambios de cada comparación
      lista_cambios <- list()
      
      
      for(i in seq.Date(inicio_dia_con_modificacion, fecha_fin, by = "day")) {
        
        fecha <- as.Date(i, origin = "1970-01-01")
        
        # fecha <- as.Date("2024-02-24")
        informe_del_dia <- funcion_calcular_estado_diario(fecha)
        
        # Almacenar los resultados si existen cambios
        if(nrow(informe_del_dia) > 0) {
          lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
        }
      }
      
      # Combinar todos los cambios en un único dataframe
      datos_nuevos <- bind_rows(lista_cambios)
      
      estado_diario_datos_nuevos <- agregar_municipio_y_circuitocorto_df(datos_nuevos)
      estado_diario_datos_nuevos <- estado_diario_datos_nuevos %>% 
        select(gid,Circuito,Municipio,Circuito_corto,Posicion,Estado,Calle,Numero,Observaciones,Fecha,Direccion,Id_viaje,the_geom,Id_motivo_inactiva,Acumulacion)
      
      
      
      estado_diario_global <- bind_rows(estado_diario_global, estado_diario_datos_nuevos)
      
    }
    
  } else {
    
    ## Si es la primera vez
    
    fecha_inicio <- as.Date("2025-02-16")
    fecha_fin <- max(historico_llenado$Fecha) + 1
    
    # Lista para almacenar los cambios de cada comparación
    lista_cambios <- list()
    
    for(i in seq.Date(fecha_inicio, fecha_fin, by = "day")) {
      
      fecha <- as.Date(i, origin = "1970-01-01")
      
      # fecha <- as.Date("2024-02-24")
      informe_del_dia <- funcion_calcular_estado_diario(fecha)
      
      # Almacenar los resultados si existen cambios
      if(nrow(informe_del_dia) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
      }
    }
    
    # Combinar todos los cambios en un único dataframe
    estado_diario <- bind_rows(lista_cambios)
    
    estado_diario_global <- agregar_municipio_y_circuitocorto_df(estado_diario)
    estado_diario_global <- estado_diario_global %>% 
      select(gid,Circuito,Municipio,Circuito_corto,Posicion,Estado,Calle,Numero,Observaciones,Fecha,Direccion,Id_viaje,the_geom,Id_motivo_inactiva,Acumulacion)
    
    
  }
  
  
  
  # Guardar el resultado
  saveRDS(estado_diario_global , file = ruta_datos)
  
  return(estado_diario_global)
  
}




# dia <- as.Date("2025-02-24")
# dia <- dia_informe
## Funcion que calcula para el DÍA, el estado diario de cada contenedor
# Este o no en mantenimiento, inactivo, etc.
funcion_calcular_estado_diario <- function(dia){
  # Busco los que hayan sido levantados
  llenado_levantado <- historico_llenado %>% 
    filter(Fecha < dia)
  
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia)
  # ubicaciones_dia_informe <- ubicaciones_dia_informe %>% 
  #   filter(is.na(Estado))
  
  
  # Filtro los que se levantaron más de una vez en dia.
  ## Busco el ultimo levante que tiene.
  df_llenadodiario <- llenado_levantado %>%
    mutate(
      Acumulacion = if_else(
        Levantado == "S", 
        # Calculamos la diferencia en días. 
        # Ojo: `dia_prueba - Fecha` también puede usarse directamente
        as.numeric(difftime(dia, Fecha, units = "days")), 
        # Si Levantado != "S", ponemos NA
        NA_real_
      )
    )
  
  #Filtro los que fueron levantados
  df_llenadodiario_levantados <- df_llenadodiario %>% 
    filter(Levantado == "S")
  
  # ordeno por GID y obtengo el último GID que fue levantado
  # obteniendo así el último dia de ataso más reciente por cada uno
  df_llenadodiario_ultimodia <- df_llenadodiario_levantados %>%
    group_by(gid) %>%
    slice_max(order_by = Fecha, n = 1) %>%
    ungroup() %>% 
    distinct(gid, .keep_all = TRUE)
  
  historico_ubicaciones_dia_informe <- ubicaciones_dia_informe %>%
    left_join(
      df_llenadodiario_ultimodia %>%
        select(Direccion,Turno_levantado,Fecha_hora_pasaje,Incidencia,Porcentaje_llenado,Numero_caja,Id_viaje,the_geom,Condicion,Id_motivo_inactiva,Acumulacion,gid),
      by = "gid"
    )
  
  
  informe_diario <- historico_ubicaciones_dia_informe
  ## Elimino los inactivos en llenado
  # informe_diario <- historico_ubicaciones_dia_informe %>% 
  #   filter(Id_motivo_inactiva == 0)
  
  gid_repetidos <- eliminar_gids_inactivos(historico_ubicaciones,historico_ubicaciones_dia_informe)
  
  # Eliminar filas en ubicaciones_repetidas si el gid existe en ubicaciones_filtrado
  informe_diario_filtrado <- informe_diario %>%
    anti_join(gid_repetidos, by = "gid")
  
  contenedores_agregados <- historico_ubicaciones_cambio_de_estado %>% 
    filter(Motivo == "Agregado") %>% 
    filter(Fecha < dia)
  
  
  ## agregados
  contenedores_agregados <- contenedores_agregados %>%
    group_by(gid) %>%
    slice_max(Fecha, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  contenedores_agregados <- contenedores_agregados %>% 
    mutate(Acumulacion =  as.numeric(difftime(dia, Fecha, units = "days"))) 
  
  informe_diario_filtrado <- informe_diario_filtrado %>%
    left_join(contenedores_agregados %>% select(gid, Acumulacion),
              by = "gid", suffix = c("", ".agg")) %>%
    mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
                                Acumulacion.agg,
                                Acumulacion)) %>%
    select(-Acumulacion.agg)
  
  ###mantenimientos y eso
  
  contenedores_activos <- historico_ubicaciones_cambio_de_estado %>% 
    filter(Motivo == "Activo") %>% 
    filter(Fecha < dia)   
  
  ## agregados
  contenedores_activos <- contenedores_activos %>%
    group_by(gid) %>%
    slice_max(Fecha, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  contenedores_activos <- contenedores_activos %>% 
    mutate(Acumulacion =  as.numeric(difftime(dia, Fecha, units = "days")))
  
  informe_diario_filtrado <- informe_diario_filtrado %>%
    left_join(contenedores_activos %>% select(gid, Acumulacion),
              by = "gid", suffix = c("", ".agg")) %>%
    mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
                                Acumulacion.agg,
                                Acumulacion)) %>%
    select(-Acumulacion.agg)
  
  
  
  
  return(informe_diario_filtrado)
  
  
}

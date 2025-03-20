
#########################################################################################
# Funcion para actualizar los df de las descargas
actualizar_planillas_RDS <- function(
    ruta_proyecto,                                                     
    ruta_funciones,
    ruta_carpeta_archivos,
    ruta_RDS_datos) {
  
  
  # Cargar la función auxiliar necesaria
  if (nzchar(ruta_funciones)) {
    source(ruta_funciones)
  }
  
  nombre_consulta <- basename(ruta_carpeta_archivos)
  
  # Cargar la lista de archivos procesados previamente (si existe)
  archivos_procesados <- if (file.exists(ruta_RDS_planillas_procesadas)) {
    readRDS(ruta_RDS_planillas_procesadas)
  } else {
    character(0)
  }
  
  # Obtener la lista de archivos CSV en la carpeta
  lista_archivos <- dir(ruta_carpeta_archivos, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Filtrar los archivos que aún no han sido procesados
  archivos_nuevos <- setdiff(lista_archivos, archivos_procesados)
  
  if (length(archivos_nuevos) > 0) {
    
    ##### Si es llenado, hago algo
    if(nombre_consulta == "10484_llenado"){
      datos_nuevos <- funcion_actualizar_llenado_10484(archivos_nuevos,ruta_carpeta_archivos)
      
    } 
    
    ##### Si es ubiccion, hago algo
    if(nombre_consulta == "10393_ubicaciones"){
      datos_nuevos <- funcion_actualizar_ubicaciones_10393(archivos_nuevos,ruta_carpeta_archivos)
      
      
    }
    
    #### Si es viajesEnUnPeriodo hago algo
    if(nombre_consulta == "10334_viajesEnUnPeriodo"){
      datos_nuevos <- funcion_actualizar_viajesEnUnPeriodo_10334(archivos_nuevos,ruta_carpeta_archivos)
    }
    
    #### Si es incidencias hago algo
    if(nombre_consulta == "10338_incidencias"){
      
      ## Cargo los datos de los df.
      datos_nuevos <- funcion_actualizar_incidencias_10334(archivos_nuevos,ruta_carpeta_archivos)
      datos_nuevos <- funcion_agregar_gid_incidencias(datos_nuevos)
      
      if(length(archivos_procesados) == 0){
        
        fecha_fin_viejo_sistema <- as.Date("2025-02-09")
        
        ruta_datos_sistema_anterior <- file.path(ruta_proyecto, "scripts/db/10338_incidencias/historico_incidencias_sistemaanterior.rds")
        datos_sistema_anterior <- readRDS(ruta_datos_sistema_anterior)
        datos_sistema_anterior <- datos_sistema_anterior %>% 
          rename(Fecha = Dia,
                 Id_viaje = id_viaje,
                 Posicion = posicion,
                 Id_incidencia = cod_inci) %>% 
          select(-nombre_contenedor) %>% 
          filter(!is.na(gid)) %>% 
          filter(Fecha <= fecha_fin_viejo_sistema)
        
        datos_sistema_anterior <- funcion_agregar_responsables(datos_sistema_anterior)
        
        datos_nuevos <- bind_rows(datos_nuevos,datos_sistema_anterior)
        datos_nuevos <- datos_nuevos %>% 
          arrange(Fecha)
      }
      
    
      
  
      

      
      
    } 
    
    # Cargar el histórico previo, si existe; en caso contrario, utilizar los datos nuevos
    historico <- if (file.exists(ruta_RDS_datos)) {
      readRDS(ruta_RDS_datos)
    } else {
      agregar_municipio_y_circuitocorto_df(datos_nuevos)
    }
    
    

    datos_nuevos <- agregar_municipio_y_circuitocorto_df(datos_nuevos)

    
    # Actualizar el histórico con los datos nuevos
    historico <- bind_rows(historico, datos_nuevos)
    historico <- historico %>% 
      distinct()
    
    # Guardar el histórico actualizado en el archivo RDS
    saveRDS(historico, file = ruta_RDS_datos)
    
    # Actualizar la lista de archivos procesados, añadiendo los nuevos
    archivos_procesados  <- c(archivos_procesados , archivos_nuevos)
    
    # Guardar la lista actualizada de archivos procesados
    saveRDS(archivos_procesados , file = ruta_RDS_planillas_procesadas)
    
  } else {
    # Si no hay archivos nuevos, se carga el histórico existente
    historico <- readRDS(ruta_RDS_datos)
  }
  
  # # Retornar el histórico actualizado
  return(historico)
}

#########################################################################################
#########################################################################################
#########################################################################################

# ruta_datos <- ruta_RDS_datos
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
    
    fecha_fin <- max(historico_llenado$Fecha)
    
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
        select(gid,Circuito,Municipio,Circuito_corto,Posicion,Estado,Calle,Numero,Observaciones,Fecha,Direccion,Id_viaje,the_geom,Id_motivo_inactiva,Fecha_informe,Acumulacion)
      
      
      
      estado_diario_global <- bind_rows(estado_diario_global, estado_diario_datos_nuevos)
      
    }
    
  } else {
    
    ## Si es la primera vez
    
    fecha_inicio <- as.Date("2025-02-15")
  
    fecha_fin <- max(historico_llenado$Fecha)
    
    # Lista para almacenar los cambios de cada comparación
    lista_cambios <- list()
    
    # i <- fecha_inicio
    for(i in seq.Date(fecha_inicio, fecha_fin, by = "day")) {
  
      fecha <- as.Date(i, origin = "1970-01-01")
      
      informe_del_dia <- funcion_calcular_estado_diario(fecha)
      informe_del_dia_arreglado <- funcion_modificar_informe_diario(informe_del_dia)
        
      # Almacenar los resultados si existen cambios
      if(nrow(informe_del_dia) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
      }
    }
    
    # Combinar todos los cambios en un único dataframe
    estado_diario <- bind_rows(lista_cambios)
    # repes <- imprimir_repetidos(estado_diario)
    
    
    
    estado_diario_global <- agregar_municipio_y_circuitocorto_df(estado_diario)
    estado_diario_global <- estado_diario_global %>% 
      select(gid,Circuito,Municipio,Circuito_corto,Posicion,Estado,Calle,Numero,Observaciones,Fecha,Direccion,Id_viaje,the_geom,Id_motivo_inactiva,Fecha_informe,Acumulacion)

  
    
    
  }
  

  
  # Guardar el resultado
  saveRDS(estado_diario_global , file = ruta_datos)
  
  return(estado_diario_global)
  
}

# dia <- i
# dia <- as.Date("2025-02-15")
# dia <- dia_informe
# dia <- fecha
## Funcion que calcula para el DÍA, el estado diario de cada contenedor
# Este o no en mantenimiento, inactivo, etc.
funcion_calcular_estado_diario_nuevo <- function(dia){
  
  #### Es el primer dia?
  if(dia == "2025-02-15"){
    
    dia_informe <- dia +1
    
    # Busco los que hayan sido levantados
    llenado_levantado <- historico_llenado %>% 
      filter(Fecha < dia_informe)
    
    
    ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
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
          as.numeric(difftime(dia_informe, Fecha, units = "days")), 
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
    
    ## estaba esto 
    
    # informe_diario_filtrado <- informe_diario_filtrado %>%
    #   left_join(contenedores_agregados %>% select(gid, Acumulacion),
    #             by = "gid", suffix = c("", ".agg")) %>%
    #   mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
    #                               Acumulacion.agg,
    #                               Acumulacion)) %>%
    #   select(-Acumulacion.agg)
    
    informe_diario_filtrado <- informe_diario_filtrado %>%
      left_join(contenedores_agregados %>% 
                  select(gid, Acumulacion),
                by = "gid", suffix = c("", ".agg")) %>%
      mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & (is.na(Acumulacion) | Acumulacion.agg < Acumulacion),
                                  Acumulacion.agg,
                                  Acumulacion)) %>%
      select(-Acumulacion.agg)
    
    # asd <- imprimir_repetidos(informe_diario_filtrado)
    
    
    
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
    
    # ESTABA ESTO
    
    # informe_diario_filtrado <- informe_diario_filtrado %>%
    #   left_join(contenedores_activos %>% select(gid, Acumulacion),
    #             by = "gid", suffix = c("", ".agg")) %>%
    #   mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
    #                               Acumulacion.agg,
    #                               Acumulacion)) %>%
    #   select(-Acumulacion.agg)
    
    informe_diario_filtrado <- informe_diario_filtrado %>%
      left_join(contenedores_activos %>% 
                  select(gid, Acumulacion),
                by = "gid", suffix = c("", ".agg")) %>%
      mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & (is.na(Acumulacion) | Acumulacion.agg < Acumulacion),
                                  Acumulacion.agg,
                                  Acumulacion)) %>%
      select(-Acumulacion.agg)
    
    
    
    
    informe_diario_filtrado <- informe_diario_filtrado %>% 
      filter(!grepl("^B_0[1-7]$", Circuito_corto)) %>% 
      mutate(DB = "Llenado")
    
 
    
    
    
    
    informe_diario_filtrado <- informe_diario_corregido %>% 
      mutate(Fecha_informe = Fecha +1)
    
  } else {
  
  dia_informe <- dia +1
  
  # Busco los que hayan sido levantados
  llenado_levantado <- historico_llenado %>% 
    filter(Fecha < dia_informe)
  
  
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
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
        as.numeric(difftime(dia_informe, Fecha, units = "days")), 
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
  
  ## estaba esto 
  
  # informe_diario_filtrado <- informe_diario_filtrado %>%
  #   left_join(contenedores_agregados %>% select(gid, Acumulacion),
  #             by = "gid", suffix = c("", ".agg")) %>%
  #   mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
  #                               Acumulacion.agg,
  #                               Acumulacion)) %>%
  #   select(-Acumulacion.agg)
  
  informe_diario_filtrado <- informe_diario_filtrado %>%
    left_join(contenedores_agregados %>% 
                select(gid, Acumulacion),
              by = "gid", suffix = c("", ".agg")) %>%
    mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & (is.na(Acumulacion) | Acumulacion.agg < Acumulacion),
                                Acumulacion.agg,
                                Acumulacion)) %>%
    select(-Acumulacion.agg)
  
  # asd <- imprimir_repetidos(informe_diario_filtrado)
  
  
  
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
  
  # ESTABA ESTO
  
  # informe_diario_filtrado <- informe_diario_filtrado %>%
  #   left_join(contenedores_activos %>% select(gid, Acumulacion),
  #             by = "gid", suffix = c("", ".agg")) %>%
  #   mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
  #                               Acumulacion.agg,
  #                               Acumulacion)) %>%
  #   select(-Acumulacion.agg)
  
  informe_diario_filtrado <- informe_diario_filtrado %>%
    left_join(contenedores_activos %>% 
                select(gid, Acumulacion),
              by = "gid", suffix = c("", ".agg")) %>%
    mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & (is.na(Acumulacion) | Acumulacion.agg < Acumulacion),
                                Acumulacion.agg,
                                Acumulacion)) %>%
    select(-Acumulacion.agg)
  
  
  informe_diario_filtrado <- informe_diario_filtrado %>% 
    mutate(Fecha_informe = Fecha +1)
  
  
  
  }
  
  return(informe_diario_filtrado)
  
  
}


funcion_cacular_estado_diario_sin_mantenimiento <- function(dia){
  
  estado_diario_completo <- funcion_calcular_estado_diario(dia)
  
  estado_diario_completo <- estado_diario_completo %>% 
    filter(is.na(Estado))
  
  return(estado_diario_completo)
  
}


## Agrego municipio, circuito corto.
agregar_municipio_y_circuitocorto_df <- function(df){
  
  estado_diario_filtrado <- df %>% 
    mutate(Municipio = case_when(
      substring(Circuito,1,2) == "CH" ~ "CH",
      substring(Circuito,1,1) == "A" ~ "A",
      substring(Circuito,1,1) == "B" ~ "B",
      substring(Circuito,1,1) == "C" ~ "C",
      substring(Circuito,1,1) == "D" ~ "D",
      substring(Circuito,1,1) == "E" ~ "E",
      substring(Circuito,1,1) == "F" ~ "F",
      substring(Circuito,1,1) == "G" ~ "G")
    )
  
  estado_diario_filtrado <- estado_diario_filtrado %>%
    mutate(Circuito_corto = ifelse(
      # Si tiene 3 digitos
      substring(Circuito,nchar(Circuito)-3,nchar(Circuito)-3) == "_",
      #valor verdadero
      substring(Circuito,nchar(Circuito)-2,nchar(Circuito)),
      substring(Circuito,nchar(Circuito)-1,nchar(Circuito))
    )) %>%
    mutate(Circuito_corto = paste0(Municipio,"_",Circuito_corto))
  
  
  return(estado_diario_filtrado)
}

#########################################################################################
#########################################################################################
#########################################################################################




# ruta_datos <- ruta_RDS_incidencias
actualizar_planillas_RDS_incidencias_por_gid <- function(ruta_datos){
  
  ubicaciones <- historico_ubicaciones
  
  incidencias_totales_conubi_yacumulacion <- if (file.exists(ruta_datos)) {
    readRDS(ruta_datos)
  } else {
    character(0)
  }
  
  ## Tiene datos?
  ## Si
  if (length(incidencias_totales_conubi_yacumulacion) > 0) {
    
    fecha_final_incidencias_registradas <- max(incidencias_totales_conubi_yacumulacion$Fecha_incidencia)
    fecha_inicio <- fecha_final_incidencias_registradas + 1
    fecha_maxima_incidencias_bd <- max(historico_incidencias$Fecha)
    
    
    if(fecha_final_incidencias_registradas < fecha_maxima_incidencias_bd) {
    
    turno_idviajes <- historico_viajes %>% 
      filter(Lugar_salida == 50) %>% 
      filter(Fecha >= fecha_inicio) %>% 
      select(Fecha,Id_viaje,Id_turno,Turno, Matricula) %>% 
      distinct()
    
    incidenciasdellenado <- historico_llenado %>% 
      filter(!is.na(Incidencia)) %>%
      filter(Fecha >= fecha_inicio) %>% 
      select(Fecha,gid,Id_viaje,Incidencia,Fecha_hora_pasaje,Direccion,Condicion) %>% 
      mutate(DB = "Llenado") %>% 
      distinct()
    
    incidenciasdeincidencias <- historico_incidencias %>%
      filter(Fecha >= fecha_inicio) %>%
      select(Fecha,gid,Id_viaje,Descripcion) %>% 
      rename(Incidencia = Descripcion) %>% 
      mutate(DB = "Incidencia") %>% 
      distinct()
    
    incidencias_totales <- bind_rows(incidenciasdeincidencias,incidenciasdellenado)
    
    
    ### me quedo con el llenado y no incidencias en caso de repetir
    incidencias_totales_sinrepetidos <- incidencias_totales %>%
      group_by(Fecha, gid, Id_viaje, Incidencia) %>%
      arrange(desc(DB == "Llenado"), .by_group = TRUE) %>%  # Ordena: TRUE (1) primero
      slice(1) %>%  # Selecciona la primera fila de cada grupo
      ungroup()
    
    
    incidencias_totales_conturno <- incidencias_totales_sinrepetidos %>% 
      left_join(turno_idviajes, by = c("Fecha", "Id_viaje")) %>% 
      arrange(desc(Fecha), desc(Id_turno))
    
    
    
    ## Le anexo la dirección para ese día.
    ## Esto sirve para lo nuevo.
    incidencias_totales_conubi <- incidencias_totales_conturno %>%
      left_join(historico_ubicaciones, by = c("gid", "Fecha")) %>% 
      filter(Fecha >= fecha_inicio)
    
    
    
    ## Agrego acumulación.
    
    acumulacion_select <- historico_estado_diario %>%
      select(gid, Fecha, Acumulacion)
    
    # repes <- imprimir_repetidos(acumulacion_select)
    
    
    datos_nuevos_incidencias_totales_conubi_yacumulacion <- incidencias_totales_conubi %>% 
      left_join(acumulacion_select, by = c("gid", "Fecha"))
    
    datos_nuevos_incidencias_totales_conubi_yacumulacion <- datos_nuevos_incidencias_totales_conubi_yacumulacion %>% 
      mutate(Fecha_informe = Fecha + 1) %>% 
      rename(Fecha_incidencia = Fecha) %>% 
      select(gid,Fecha_incidencia,Incidencia,Id_viaje,Id_turno,Turno,Matricula,Municipio,Circuito,Circuito_corto,Posicion,
             Estado,Direccion,Calle,Numero,Observaciones, Fecha_informe, Acumulacion, DB)
    
    datos_nuevos_incidencias_totales_conubi_yacumulacion <- datos_nuevos_incidencias_totales_conubi_yacumulacion %>%
      mutate(Direccion = if_else(is.na(Direccion) | Direccion == "",
                                 paste(Calle, Numero),
                                 Direccion))
    
    responsables_incidencias <- dataframe_responsable_incidencias()
    
    df_unido <- datos_nuevos_incidencias_totales_conubi_yacumulacion %>%
      left_join(responsables_incidencias, by = c("Incidencia" = "descripcion"))
    
    datos_nuevos_incidencias_totales_conubi_yacumulacion <- df_unido
    
    
    incidencias_totales_conubi_yacumulacion <- bind_rows(incidencias_totales_conubi_yacumulacion,datos_nuevos_incidencias_totales_conubi_yacumulacion)
    

    
    }
    
  } else {
    
    ### CARGO SISTEMA ANTERIOR
    # 
    # ruta_sistema_anterior <- file.path(ruta_proyecto, "scripts/incidencias_por_gid/historico_incidencias_sistemaanterior.rds")
    # # Cargar la lista de archivos procesados previamente (si existe)
    # 
    # incidencias_sistema_anterior <- if (file.exists(ruta_sistema_anterior)) {
    #   readRDS(ruta_sistema_anterior)
    # } else {
    #   character(0)
    # }
    # 
    # incidencias_sistema_anterior <- incidencias_sistema_anterior %>% 
    #   select(gid,Dia_incidencia,Descripcion,id_viaje,Turno_viaje,Municipio,Nomenclatura,Circuito_Corto,Posicion,
    #          Direccion)
      
    ########################
    
    
    ## primera vez
    
    ## obtener con el idviaje, los turnos.
    turno_idviajes <- historico_viajes %>% 
      filter(Lugar_salida == 50) %>% 
      select(Fecha,Id_viaje,Id_turno,Turno, Matricula) %>% 
      distinct()
    
    ## Separar las incidencias
    
    ## llenado
    incidenciasdellenado <- historico_llenado %>% 
      filter(!is.na(Incidencia)) %>% 
      select(Fecha,gid,Id_viaje,Incidencia,Fecha_hora_pasaje,Direccion,Condicion) %>% 
      mutate(DB = "Llenado") %>% 
      distinct()
    
    ## de incidencia
    incidenciasdeincidencias <- historico_incidencias %>%
      select(Fecha,gid,Id_viaje,Descripcion) %>% 
      rename(Incidencia = Descripcion) %>% 
      mutate(DB = "Incidencia") %>% 
      distinct()
    
    
    
    ## las uno en un solo df
    incidencias_totales <- bind_rows(incidenciasdeincidencias,incidenciasdellenado)
    
    
    incidencias_totales_sinrepetidos <- incidencias_totales %>%
      group_by(Fecha, gid, Id_viaje, Incidencia) %>%
      arrange(desc(DB == "Llenado"), .by_group = TRUE) %>%  # Ordena: TRUE (1) primero
      slice(1) %>%  # Selecciona la primera fila de cada grupo
      ungroup()
    
    
    
    
    incidencias_totales_conturno <- incidencias_totales_sinrepetidos %>% 
      left_join(turno_idviajes, by = c("Fecha", "Id_viaje")) %>% 
      arrange(desc(Fecha), desc(Id_turno))
    
    
    
    
    fecha_inicio_nuevo <- as.Date("2025-02-15")
    ## Le anexo la dirección para ese día.
    ## Esto sirve para lo nuevo.
    incidencias_totales_conubi <- incidencias_totales_conturno %>%
      left_join(historico_ubicaciones, by = c("gid", "Fecha")) %>% 
      filter(Fecha >= fecha_inicio_nuevo)
    
    
    
    ## Agrego acumulación.
    
    acumulacion_select <- historico_estado_diario %>%
      select(gid, Fecha, Acumulacion)
    
    
    incidencias_totales_conubi_yacumulacion <- incidencias_totales_conubi %>% 
      left_join(acumulacion_select, by = c("gid", "Fecha"))
    
    incidencias_totales_conubi_yacumulacion <- incidencias_totales_conubi_yacumulacion %>% 
      mutate(Fecha_informe = Fecha + 1) %>% 
      rename(Fecha_incidencia = Fecha) %>% 
      select(gid,Fecha_incidencia,Incidencia,Id_viaje,Id_turno,Turno,Matricula,Municipio,Circuito,Circuito_corto,Posicion,
             Estado,Direccion,Calle,Numero,Observaciones,Condicion, Fecha_informe, Acumulacion, DB)
    
    incidencias_totales_conubi_yacumulacion <- incidencias_totales_conubi_yacumulacion %>%
      mutate(Direccion = if_else(is.na(Direccion) | Direccion == "",
                                 paste(Calle, Numero),
                                 Direccion))
    
    responsables_incidencias <- dataframe_responsable_incidencias()
    
    df_unido <- incidencias_totales_conubi_yacumulacion %>%
      left_join(responsables_incidencias, by = c("Incidencia" = "descripcion"))

    incidencias_totales_conubi_yacumulacion <- df_unido
    
  }
  
  
  
  # Guardar el resultado
  saveRDS(incidencias_totales_conubi_yacumulacion , file = ruta_datos)
  
  return(incidencias_totales_conubi_yacumulacion)
  
}



# ruta_datos <- ruta_RDS_llenado_completo
actualizar_planillas_RDS_llenado_completas <- function(ruta_datos){
  
  incidencias_y_llenado_completo_global <- if (file.exists(ruta_datos)) {
    readRDS(ruta_datos)
  } else {
    character(0)
  }
  
  ## Tiene datos?
  ## Si
  if (length(incidencias_y_llenado_completo_global) > 0) {
    
    ## Busco el último valor que tiene
    ultimo_dia_con_modificacion <- max(incidencias_y_llenado_completo_global$Fecha)
    inicio_dia_con_modificacion <- ultimo_dia_con_modificacion + 1
    
    if(ultimo_dia_con_modificacion < max(historico_llenado$Fecha)){
      
      ## Filtro los que fueron levantado segun "Llenado".
      # Para obtener y armar el historico completo.
      solo_levantado <- historico_llenado %>% 
        filter(Levantado == "S") %>% 
        select(gid, Fecha, Municipio, Circuito, Circuito_corto, Posicion, Direccion, Turno_levantado,Fecha_hora_pasaje, Id_viaje, Levantado, Porcentaje_llenado, Numero_caja, Condicion) %>% 
        rename(Turno = Turno_levantado) %>% 
        mutate(DB = "Llenado") %>% 
        filter(Fecha > ultimo_dia_con_modificacion)
      
      solo_incidencias <- historico_incidencias_por_gid %>% 
        select(gid, Fecha_incidencia, Municipio, Circuito, Circuito_corto, 
               Posicion, Direccion, Observaciones, Turno, Id_viaje, Incidencia, Condicion, DB) %>% 
        rename(Fecha = Fecha_incidencia) %>% 
        mutate(
          Turno = case_when(
            Turno == "Nocturno (22 a 06 hrs.)" ~ "NOCTURNO",
            Turno == "Vespertino (14 a 22 hrs.)" ~ "VESPERTINO",
            Turno == "Matutino (06 a 14 hrs.)" ~ "MATUTINO",
            TRUE ~ Turno  # Mantiene el valor original en otros casos
          ),
          Condicion = ifelse(Condicion == "", NA, Condicion)  # Reemplaza cadenas vacías por NA
        ) %>% 
        filter(Fecha > ultimo_dia_con_modificacion)
      
      incidencias_y_llenado_completo_deldia <- bind_rows(solo_incidencias,solo_levantado) %>% 
        mutate(Condicion = ifelse(Condicion == "", NA, Condicion))  # Reemplaza cadenas vacías por NA)
      
      responsables_incidencias <- dataframe_responsable_incidencias()
      
      incidencias_y_llenado_completo_deldia <- incidencias_y_llenado_completo_deldia %>%
        left_join(responsables_incidencias, by = c("Incidencia" = "descripcion"))
      
      incidencias_y_llenado_completo_global <- bind_rows(incidencias_y_llenado_completo_global,incidencias_y_llenado_completo_deldia) %>% 
        arrange(desc(Fecha),Circuito_corto,Posicion)
      

      
      # acumulacion_select <- historico_estado_diario %>%
      #   select(gid, Fecha, Acumulacion)
      # 
      # incidencias_y_llenado_completo_global <- incidencias_y_llenado_completo_global %>% 
      #   left_join(acumulacion_select, by = c("gid", "Fecha"))
      
    }
    

    
  } else {
    
    ## Si es la primera vez
    
    
    ## Filtro los que fueron levantado segun "Llenado".
    # Para obtener y armar el historico completo.
    solo_levantado <- historico_llenado %>%
      filter(Levantado == "S") %>% 
      select(gid, Fecha, Municipio, Circuito, Circuito_corto, Posicion, Direccion, Turno_levantado,Fecha_hora_pasaje, Id_viaje, Levantado, Porcentaje_llenado, Numero_caja, Condicion) %>% 
      rename(Turno = Turno_levantado) %>% 
      mutate(DB = "Llenado")
    
    solo_incidencias <- historico_incidencias_por_gid %>% 
      select(gid, Fecha_incidencia, Municipio, Circuito, Circuito_corto, 
             Posicion, Direccion, Observaciones, Turno, Id_viaje, Incidencia, Condicion, DB) %>% 
      rename(Fecha = Fecha_incidencia) %>% 
      mutate(
        Turno = case_when(
          Turno == "Nocturno (22 a 06 hrs.)" ~ "NOCTURNO",
          Turno == "Vespertino (14 a 22 hrs.)" ~ "VESPERTINO",
          Turno == "Matutino (06 a 14 hrs.)" ~ "MATUTINO",
          TRUE ~ Turno  # Mantiene el valor original en otros casos
        ),
        Condicion = ifelse(Condicion == "", NA, Condicion)  # Reemplaza cadenas vacías por NA
      )
    
    
    incidencias_y_llenado_completo_global <- bind_rows(solo_incidencias,solo_levantado) %>% 
      arrange(desc(Fecha),Circuito_corto,Posicion) %>% 
      mutate(Condicion = ifelse(Condicion == "", NA, Condicion))  # Reemplaza cadenas vacías por NA)
    
    responsables_incidencias <- dataframe_responsable_incidencias()
    
    incidencias_y_llenado_completo_global <- incidencias_y_llenado_completo_global %>%
      left_join(responsables_incidencias, by = c("Incidencia" = "descripcion"))
    
    # acumulacion_select <- historico_estado_diario %>%
    #   select(gid, Fecha, Acumulacion)
    # 
    # incidencias_y_llenado_completo_global <- incidencias_y_llenado_completo_global %>% 
    #   left_join(acumulacion_select, by = c("gid", "Fecha"))
  }
  
  
  
  # Guardar el resultado
  saveRDS(incidencias_y_llenado_completo_global , file = ruta_datos)
  
  return(incidencias_y_llenado_completo_global)
  
}







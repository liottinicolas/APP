


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
    
    fecha_inicio <- as.Date("2025-02-15")
    fecha_fin <- max(historico_llenado$Fecha) + 1
    
    # Lista para almacenar los cambios de cada comparación
    lista_cambios <- list()
    
    for(i in seq.Date(fecha_inicio, fecha_fin, by = "day")) {
      
      fecha <- as.Date(i, origin = "1970-01-01")
      
      # fecha <- as.Date("2025-02-16")
      informe_del_dia <- funcion_calcular_estado_diario(fecha)
      ## Calcular
      informe_modificado <- funcion_modificar_informe_diario(informe_del_dia)
      
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




# dia <- as.Date("2025-02-15")
# dia <- dia_informe
## Funcion que calcula para el DÍA, el estado diario de cada contenedor
# Este o no en mantenimiento, inactivo, etc.
# dia = dia informe.
funcion_calcular_estado_diario <- function(dia){
  

  # Busco los que hayan sido levantados
  llenado_levantado <- historico_llenado %>% 
    filter(Fecha < dia)
  
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia)
  
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
    select(-Acumulacion.agg) %>% 
    mutate(DB = "Llenado") %>% 
    filter(!grepl("^B_0[1-7]$", Circuito_corto))
  
  
  
  
  return(informe_diario_filtrado)
  
  
}




# df_informediario <- informe_del_dia
# Me modifica los df cuando los viajes de grua y pluma no están registrados en llenado. (tablet)
# Se saca la info de viajes en un período.
funcion_modificar_informe_diario <- function(df_informediario){
  
  ### Hay info guardada??
  ruta_datos_sinreg <- file.path(ruta_proyecto, "scripts/estado_diario/cambios_estado_diario_gruapluma_sin_registrarllenado.rds")
  
  cambios_estado_diario_gruapluma_sin_registrarllenado <- if (file.exists(ruta_datos_sinreg)) {
    readRDS(ruta_datos_sinreg)
  } else {
    character(0)
  }
  
  if (length(cambios_estado_diario_gruapluma_sin_registrarllenado) > 0){
    
    
### OBTENGO LOS NUEVOS CONTENEDORES QUE FUERON LEVANTADOS POR PLUMA-GRUA
        
   ### Hago todo lo mismo que lo anterior
    # df_informediario <- informe_del_dia
    dia <- max(df_informediario$Fecha)
    
    ## Obtengo los contenedores del día, que se deben modificar en el informe diario
    # Que fueron levantados por pluma y grua.
    contenedores_a_modificar_acumulacion <- funcion_obtener_df_de_contenedores_a_modificar_acumulacion(dia,df_informediario)
    
    # Arreglo el informe diario con esos contenedores.
    informe_diario_corregido <- df_informediario %>%
      rows_update(contenedores_a_modificar_acumulacion, by = c("gid", "Fecha"))
    
#########################################################################
    
    ## Datos a guardar del dia de hoy
    # Contenedores que se modifico la acumulacion del dia
    # Falta ver que sucedio con los que ya fueron modificados días anteriores.
    datos_a_guardar <- informe_diario_corregido %>%
      filter(DB == "ViajesEnUnPeriodo")
    
    ## Busco todos los cambios anteriores al día de hoy
    ## Es decir, con los datos historicos de los contenedores que se modificaron en dias anteriores,
    # Busco cuantos días de acumulacion tiene el dia de hoy.
    cambios_estado_diario_gruapluma_sin_registrarllenado_para_hoy <- cambios_estado_diario_gruapluma_sin_registrarllenado %>% 
      filter(Fecha < dia) %>% 
      select(gid,Fecha,Id_viaje,Circuito_corto,Posicion,DB,Turno_levantado,Porcentaje_llenado,Fecha_hora_pasaje,Acumulacion) %>% 
      mutate(Acumulacion_actual = as.integer(dia - Fecha)+1)
    
    # Este df, son los contenedores, sin contar el dia de hoy, que fueron modificados por pluma/grua
    # Obtengo la acumulación, si ya el camión paso y lo registro en la app, lo debería dejar así y borramos en el historico.
    cambios_historicos_sinhoy <- informe_diario_corregido %>% 
      semi_join(cambios_estado_diario_gruapluma_sin_registrarllenado_para_hoy, by = "gid")
    
########################################################## DF para no borrar.
    
    ## Aquellos que la acumulacion está en 1, porque lo levantó el camión lateral.
    cambios_historicos_sinhoy_acumulacion_mayora1 <- cambios_historicos_sinhoy %>% 
      filter(Acumulacion != 1)
    
    ## no hay que guardarlo, porque sigue estando en el db.
    
    ## Corrigo el día en el informe diario. Actualizo la acumulación.
    ## Este no lo borro porque sigue mal.
    informe_diario_corregido <- informe_diario_corregido %>%
      rows_update(cambios_historicos_sinhoy_acumulacion_mayora1, by = "gid")
    
    
    ## Aquellos que la acumulacion está en 1, porque lo levantó el camión lateral.
    cambios_historicos_sinhoy_acumulacion_iguala1 <- cambios_historicos_sinhoy %>% 
      filter(Acumulacion == 1)
    
    if(nrow(cambios_historicos_sinhoy_acumulacion_iguala1) > 0){
      
      ## Elimino del historico, el df que tiene acumulacion 1 porque ya fue levantado.
      datos_sin_contenedores_levantadosenlateral <- anti_join(cambios_estado_diario_gruapluma_sin_registrarllenado, 
                                      cambios_historicos_sinhoy_acumulacion_iguala1, 
                                      by = "gid")
      
    datos_a_guardar <- bind_rows(datos_a_guardar,datos_sin_contenedores_levantadosenlateral)
      
    } else {
      
      resultado <- cambios_estado_diario_gruapluma_sin_registrarllenado %>% 
        filter(gid %in% cambios_historicos_sinhoy_acumulacion_mayora1$gid)
      
      datos_a_guardar <- bind_rows(datos_a_guardar,resultado)
    }

    
    
    

    
  } else {
    # df_informediario <- informe_del_dia
    dia <- max(df_informediario$Fecha)
    
    contenedores_a_modificar_acumulacion <- funcion_obtener_df_de_contenedores_a_modificar_acumulacion(dia,df_informediario)
    
    informe_diario_corregido <- df_informediario %>%
      rows_update(contenedores_a_modificar_acumulacion, by = c("gid", "Fecha"))
    
    datos_a_guardar <- informe_diario_corregido %>%
      filter(DB == "ViajesEnUnPeriodo")
    
  }
  
  saveRDS(datos_a_guardar , file = ruta_datos_sinreg)
  
  return(informe_diario_corregido)
}



funcion_obtener_df_de_contenedores_a_modificar_acumulacion <- function(fecha_consulta,df_inf_diario){
  
  dia <- fecha_consulta
  df_informediario <- df_inf_diario
  
  contenedores_levantados_por_gruapluma <- funcion_obtener_contenedores_levantados_por_grua_pluma_sin_llenado(dia)
  
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>%
    mutate(Turno = case_when(
      Id_turno == 1 ~ "MATUTINO",
      Id_turno == 2 ~ "VESPERTINO",
      Id_turno == 3 ~ "NOCTURNO",
      TRUE ~ NA_character_
    )) %>%
    mutate(Porcentaje_llenado = 100)
  
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>%
    mutate(Fecha_hora_pasaje = as.POSIXct(
      paste(Fecha, case_when(
        Id_turno == 1 ~ "06:00:00",
        Id_turno == 2 ~ "14:00:00",
        Id_turno == 3 ~ "22:00:00"
      )),
      format = "%Y-%m-%d %H:%M:%S"
    )) 
  
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>% 
    select(Fecha,Id_viaje,Circuito_corto,Posicion,DB,Turno,Porcentaje_llenado,Fecha_hora_pasaje)
  
  
  contenedores_a_modificar_acumulacion <- inner_join(df_informediario, 
                                                     contenedores_levantados_por_gruapluma, 
                                                     by = c("Fecha", "Circuito_corto", "Posicion")) %>% 
    filter(Acumulacion > 1)
  
  contenedores_a_modificar_acumulacion <- contenedores_a_modificar_acumulacion %>% 
    select(gid,Fecha,Acumulacion,Id_viaje.y,DB.y,Porcentaje_llenado.y,Turno,Fecha_hora_pasaje.y) %>% 
    rename(Id_viaje = Id_viaje.y,
           Porcentaje_llenado = Porcentaje_llenado.y,
           Turno_levantado = Turno,
           Fecha_hora_pasaje = Fecha_hora_pasaje.y,
           DB = DB.y) %>% 
    mutate(Acumulacion = 1) %>% 
    mutate(Numero_caja = NA) %>% 
    mutate(Incidencia = NA_character_) %>% 
    mutate(Condicion = "") 
  
  return(contenedores_a_modificar_acumulacion)
  
}  




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
  
  
  ubis_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
  estado_diario_global <- funcion_agregar_the_geom_a_faltantes(estado_diario_global,ubis_existentes$ubicaciones_con_thegeom)
  
  
  
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
    contenedores_a_modificar_acumulacion <- funcion_obtener_df_de_contenedores_a_modificar_acumulacion(dia,df_informediario) %>% 
      distinct(gid, Fecha, .keep_all = TRUE)
    
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



funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom <- function(){
  
  ubicaciones_totales <- historico_ubicaciones %>% 
    mutate(Direccion = ifelse(
      is.na(historico_ubicaciones$Numero),
      historico_ubicaciones$Calle,
      paste(historico_ubicaciones$Calle, historico_ubicaciones$Numero)))
  
  # ubicaciones_totales <- funcion_arreglar_ubicaciones_paragid()
  
  ubicaciones_cambios_direccion_unica <- ubicaciones_totales %>%
    group_by(gid, Direccion) %>%              # Agrupamos por gid y dirección
    slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%  # Nos quedamos con la fila con la fecha mínima
    ungroup() %>% 
    select(gid,Fecha,Estado,Municipio,Circuito_corto,Posicion,Calle,Numero,Direccion,Observaciones) %>% 
    group_by(gid) %>%
    mutate(n_dir = n_distinct(Direccion)) %>%  # Cuenta las direcciones únicas por gid
    ungroup() %>% 
    filter(!(Circuito_corto %in% paste0("B_0", 1:7)))
  
  
  
  total_gid_ubicaciones_cambios_direccion_unica <- ubicaciones_cambios_direccion_unica %>% 
    group_by(gid) %>% 
    summarise(total = n())
  
  ## Total gid con direcciones distintas = 10996
  ## Total gids = 11019
  
  #### LLENADO.
  
  unique_gids <- historico_llenado %>% 
    distinct(gid)
  
  ## TOTAL 40
  gid_que_no_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
    anti_join(unique_gids, by = "gid")
  
  ## TOTAL 11020
  gid_que_si_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
    anti_join(gid_que_no_estan_ubicaciones, by ="gid")
  
  
  direcciones_de_llenado <- historico_llenado %>%
    group_by(gid, Direccion) %>%              # Agrupamos por gid y dirección
    slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%  # Nos quedamos con la fila con la fecha mínima
    ungroup() %>% 
    select(gid,Fecha,Municipio,Circuito_corto,Posicion,Direccion,the_geom)
  
  # df_filtrado_llenado <- historico_llenado %>% 
  #   distinct(gid, Direccion, .keep_all = TRUE)
  
  
  ### Bloque 1 #####
  
  ### Aquellos que coincida el gid, direccion, le agrego the_geom.
  ubicaciones_por_direccion_gid_iguales <- gid_que_si_estan_ubicaciones %>%
    left_join(
      direcciones_de_llenado %>% select(gid, Direccion, the_geom),
      by = c("gid", "Direccion")
    ) 
  
  ## Filtro los que no tienen direccion
  NA_de_ubicaciones_por_direccion_gid_iguales <- ubicaciones_por_direccion_gid_iguales %>% 
    filter(is.na(the_geom))
  
  bloque_1_direcciones <- ubicaciones_por_direccion_gid_iguales %>% 
    anti_join(NA_de_ubicaciones_por_direccion_gid_iguales, by="gid")
  
  ## los que faltan????
  
  
  
  ## BLOQUE 2  ############################
  
  ### Aquellos que coincida el gid y la calle, le agrego the_geom.
  
  ubicaciones_por_calle_y_gid_iguales <- NA_de_ubicaciones_por_direccion_gid_iguales %>%
    left_join(
      direcciones_de_llenado %>% select(gid, Direccion, the_geom),
      by = c("gid", "Calle" = "Direccion")
    ) %>% 
    select(-the_geom.x) %>% 
    rename(the_geom = the_geom.y)
  
  NA_ubicaciones_por_calle_y_gid_iguales <- ubicaciones_por_calle_y_gid_iguales %>% 
    filter(is.na(the_geom))
  
  bloque_2_calles <- ubicaciones_por_calle_y_gid_iguales %>% 
    anti_join(NA_ubicaciones_por_calle_y_gid_iguales, by="gid")
  
  ### BLOQUE 3
  
  the_geom_ubicaciones <- bind_rows(bloque_1_direcciones,bloque_2_calles)
  ###################################
  
  ##3 Ubicaciones que por gid y direccion, gid y calle no se ubicaron
  ubicaciones_diferente_direccion_y_calle <- NA_ubicaciones_por_calle_y_gid_iguales 
  
  # direcciones que solo tuvieron 1 direccion en la historia.
  ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle %>% 
    filter(n_dir == 1) %>% 
    select(-the_geom)
  
  # Filtro solo los gids
  unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
    distinct(gid)
  
  ### Filtro los que no tienen cambio de direccion, es decir para un solo gid no cambiaron
  # No los encontré antes por que el gid y la dirección no coincidian (era dificil el nombre)
  
  # Filtro esas direcciones sin repetir
  the_geom_faltantes <- historico_llenado %>% 
    inner_join(unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion , by="gid") %>% 
    distinct(gid,Direccion,the_geom)
  
  ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final <- ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
    left_join(
      the_geom_faltantes %>% select(gid,the_geom), by="gid"
    )
  
  ### 10946
  the_geom_ubicaciones <- bind_rows(the_geom_ubicaciones,ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final)
  ###################################################################################################
  
  ubicaciones_diferente_direccion_y_calle_con_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle %>% 
    filter(n_dir > 1) %>% 
    select(-the_geom)
  
  
  return(list(ubicaciones_con_thegeom = the_geom_ubicaciones, ubicaciones_sin_thegeom = ubicaciones_diferente_direccion_y_calle_con_cambios_de_direccion))
  
}





## Le paso el estado_diario.
# df_singeom <- historico_estado_diario
# ubi_completa <- ubicaciones_unicas_completas
funcion_agregar_the_geom_a_faltantes <- function(df_singeom,ubi_completa){
  
  # Forzar la conversión de las columnas a character en ambos data frames
  df_singeom <- df_singeom %>% 
    mutate(
      the_geom = as.character(the_geom),
      Direccion = as.character(Direccion)
    )
  
  ubi_completa <- ubi_completa %>% 
    mutate(
      the_geom = as.character(the_geom),
      Direccion = as.character(Direccion)
    )
  
  retorno <- df_singeom
  
  estado_diario_con_thegeom_historico <- df_singeom %>% 
    filter(!is.na(the_geom))
  
  estado_diario_sin_thegeom <- df_singeom %>% 
    filter(is.na(the_geom))
  
  # Usar nrow() para verificar si hay filas sin the_geom
  if(nrow(estado_diario_sin_thegeom) > 0){
    
    ## Le anexo calle y número para tener la dirección, forzando el tipo character
    estado_diario_sin_thegeom <- estado_diario_sin_thegeom %>%
      mutate(Direccion = if_else(
        is.na(Numero),
        as.character(Calle),
        paste0(as.character(Calle), " ", as.character(Numero)),
        missing = NA_character_
      ))
    
    # Realizamos el left_join y actualizamos the_geom
    estado_diario_con_thegeom <- estado_diario_sin_thegeom %>%
      left_join(
        ubi_completa %>% 
          select(gid, Direccion, the_geom) %>% 
          rename(the_geom_asd = the_geom),
        by = c("gid", "Direccion")
      ) %>%
      mutate(the_geom = if_else(
        !is.na(the_geom_asd),
        the_geom_asd,
        as.character(the_geom),
        missing = NA_character_
      )) %>%
      select(-the_geom_asd)
    
    retorno <- bind_rows(estado_diario_con_thegeom_historico, estado_diario_con_thegeom)
  }
  
  return(retorno)
  
}

## no se usa aun

arreglar_informediario_viejo <- function(){
  ruta <- file.path(ruta_proyecto, "scripts/estado_diario/estado_diario_versionanterior.rds")
  anterior <- read_rds(ruta) 
  
  historico_estadodiario_versionanterior <- anterior %>% 
    select(gid,Nomenclatura,Municipio,Circuito,Posicion,Direccion,the_geom,Acumulacion) %>% 
    rename(
      Circuito_corto = Circuito,
      Circuito = Nomenclatura)
  
  
  
  
}



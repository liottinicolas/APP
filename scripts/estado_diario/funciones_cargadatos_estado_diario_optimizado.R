#########################################################################
# Funciones para procesar y mantener actualizado el estado diario
# de los contenedores de residuos.
# 
# Este script contiene funciones para:
# - Actualizar planillas RDS con estado diario
# - Calcular el estado diario de los contenedores
# - Modificar informes para registros especiales (grúa/pluma)
# - Gestionar información geométrica
#########################################################################

# Variables de referencia (solo para documentación)
# ruta_datos <- ruta_RDS_datos
# ubicaciones <- historico_ubicaciones

#' Actualiza las planillas RDS del estado diario de contenedores
#' 
#' Esta función obtiene y actualiza el estado diario de todos los contenedores,
#' procesando la información desde la última actualización hasta la fecha actual.
#' Si no existen datos previos, inicia el procesamiento desde una fecha predeterminada.
#' 
#' @param ruta_datos Ruta donde se guardará el archivo RDS
#' @return Dataframe con el estado diario global actualizado
actualizar_planillas_RDS_estado_diario <- function(ruta_datos){
  
  ubicaciones <- historico_ubicaciones
  
  # Cargar datos históricos si existen, o crear estructura vacía
  estado_diario_global <- if (file.exists(ruta_datos)) {
    readRDS(ruta_datos)
  } else {
    character(0)
  }
  
  # Si ya existen datos históricos
  if (length(estado_diario_global) > 0) {
    
    # Determinar período a procesar: desde último día registrado hasta hoy
    ultimo_dia_con_modificacion <- max(estado_diario_global$Fecha)
    inicio_dia_con_modificacion <- ultimo_dia_con_modificacion + 1
    fecha_fin <- max(historico_llenado$Fecha) + 1
    
    # Verificar si existen nuevas fechas a procesar
    fechas <- sort(unique(ubicaciones$Fecha[ubicaciones$Fecha > ultimo_dia_con_modificacion]))
    
    if(length(fechas) > 0){
      
      # Lista para almacenar los cambios de cada día
      lista_cambios <- list()
      
      # Procesar cada día desde el último registrado hasta el actual
      for(i in seq.Date(inicio_dia_con_modificacion, fecha_fin, by = "day")) {
        
        fecha <- as.Date(i, origin = "1970-01-01")
        
        # Calcular estado diario para esta fecha
        informe_del_dia <- funcion_calcular_estado_diario(fecha)
        
        # Almacenar solo si hay resultados
        if(nrow(informe_del_dia) > 0) {
          lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
          print(paste("Procesando día:", fecha))
        }
      }
      
      # Combinar todos los informes diarios en un único dataframe
      if(length(lista_cambios) > 0) {
        datos_nuevos <- bind_rows(lista_cambios)
        
        # Agregar datos de municipio y circuito corto
        estado_diario_datos_nuevos <- agregar_municipio_y_circuitocorto_df(datos_nuevos)
        estado_diario_datos_nuevos <- estado_diario_datos_nuevos %>% 
          select(gid, Circuito, Municipio, Circuito_corto, Posicion, Estado,
                 Calle, Numero, Observaciones, Fecha, Direccion, Id_viaje, 
                 the_geom, Id_motivo_inactiva, Acumulacion)
        
        # Combinar con los datos históricos
        estado_diario_global <- bind_rows(estado_diario_global, estado_diario_datos_nuevos)
      }
    }
    
  } else {
    # Primera ejecución: procesar desde fecha inicial fija
    
    fecha_inicio <- as.Date("2025-02-15")
    fecha_fin <- max(historico_llenado$Fecha) + 1
    
    # Lista para almacenar los informes diarios
    lista_cambios <- list()
    
    # Procesar todos los días desde fecha inicio hasta la fecha actual
    for(i in seq.Date(fecha_inicio, fecha_fin, by = "day")) {
      
      fecha <- as.Date(i, origin = "1970-01-01")
      
      # Calcular y modificar estado diario
      informe_del_dia <- funcion_calcular_estado_diario(fecha)
      informe_modificado <- funcion_modificar_informe_diario(informe_del_dia)
      
      # Almacenar si hay resultados
      if(nrow(informe_del_dia) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
        print(paste("Procesando día:", fecha))
      }
    }
    
    # Combinar todos los informes diarios
    if(length(lista_cambios) > 0) {
      estado_diario <- bind_rows(lista_cambios)
      
      # Agregar datos de municipio y circuito
      estado_diario_global <- agregar_municipio_y_circuitocorto_df(estado_diario)
      estado_diario_global <- estado_diario_global %>% 
        select(gid, Circuito, Municipio, Circuito_corto, Posicion, Estado,
               Calle, Numero, Observaciones, Fecha, Direccion, Id_viaje,
               the_geom, Id_motivo_inactiva, Acumulacion)
    }
  }
  
  # Completar información geométrica faltante
  ubis_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
  estado_diario_global <- funcion_agregar_the_geom_a_faltantes(
    estado_diario_global,
    ubis_existentes$ubicaciones_con_thegeom
  )
  
  # Antes de guardar el RDS
  estado_diario_global <- estado_diario_global %>%
    distinct(gid, Fecha, .keep_all = TRUE)
  
  # Guardar resultado final
  saveRDS(estado_diario_global, file = ruta_datos)
  
  return(estado_diario_global)
}


#' Calcula el estado diario de los contenedores
#' 
#' Obtiene el estado de cada contenedor para una fecha específica,
#' considerando si está activo, en mantenimiento, la acumulación
#' de días desde el último levante, etc.
#' 
#' @param dia Fecha para la cual se calculará el estado diario
#' @return Dataframe con el estado diario de los contenedores
funcion_calcular_estado_diario <- function(dia){
  
  # Obtener registros de llenado anteriores a la fecha de consulta
  llenado_levantado <- historico_llenado %>% 
    filter(Fecha < dia)
  
  # Obtener ubicaciones para el día del informe
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia)
  
  # Calcular días de acumulación desde último levante
  df_llenadodiario <- llenado_levantado %>%
    mutate(
      Acumulacion = if_else(
        Levantado == "S", 
        # Días transcurridos desde que fue levantado
        as.numeric(difftime(dia, Fecha, units = "days")), 
        NA_real_
      )
    )
  
  # Filtrar solo contenedores que fueron levantados
  df_llenadodiario_levantados <- df_llenadodiario %>% 
    filter(Levantado == "S")
  
  # Obtener el último registro de levante para cada contenedor
  df_llenadodiario_ultimodia <- df_llenadodiario_levantados %>%
    group_by(gid) %>%
    slice_max(order_by = Fecha, n = 1) %>%
    ungroup() %>% 
    distinct(gid, .keep_all = TRUE)
  
  # Unir ubicaciones con datos de último levante
  historico_ubicaciones_dia_informe <- ubicaciones_dia_informe %>%
    left_join(
      df_llenadodiario_ultimodia %>%
        select(Direccion, Turno_levantado, Fecha_hora_pasaje, Incidencia, 
               Porcentaje_llenado, Numero_caja, Id_viaje, the_geom, 
               Condicion, Id_motivo_inactiva, Acumulacion, gid),
      by = "gid"
    )
  
  # Después de la unión
  historico_ubicaciones_dia_informe <- historico_ubicaciones_dia_informe %>%
    distinct(gid, Fecha, .keep_all = TRUE)
  
  # Preparar informe diario inicial
  informe_diario <- historico_ubicaciones_dia_informe
  
  # Eliminar GIDs repetidos/inactivos
  gid_repetidos <- eliminar_gids_inactivos(historico_ubicaciones, historico_ubicaciones_dia_informe)
  
  # Filtrar para mantener solo contenedores activos
  informe_diario_filtrado <- informe_diario %>%
    anti_join(gid_repetidos, by = "gid")
  
  # Procesamiento de contenedores agregados recientemente
  contenedores_agregados <- historico_ubicaciones_cambio_de_estado %>% 
    filter(Motivo == "Agregado") %>% 
    filter(Fecha < dia)
  
  # Obtener el registro más reciente para cada contenedor agregado
  contenedores_agregados <- contenedores_agregados %>%
    group_by(gid) %>%
    slice_max(Fecha, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Calcular días de acumulación para contenedores recién agregados
  contenedores_agregados <- contenedores_agregados %>% 
    mutate(Acumulacion = as.numeric(difftime(dia, Fecha, units = "days"))) 
  
  # Actualizar acumulación en el informe con info de contenedores recién agregados
  informe_diario_filtrado <- informe_diario_filtrado %>%
    left_join(contenedores_agregados %>% select(gid, Acumulacion),
              by = "gid", suffix = c("", ".agg")) %>%
    mutate(Acumulacion = ifelse(!is.na(Acumulacion.agg) & Acumulacion.agg < Acumulacion,
                                Acumulacion.agg,
                                Acumulacion)) %>%
    select(-Acumulacion.agg)
  
  # Procesamiento de contenedores activados recientemente
  contenedores_activos <- historico_ubicaciones_cambio_de_estado %>% 
    filter(Motivo == "Activo") %>% 
    filter(Fecha < dia)   
  
  # Obtener el registro más reciente para cada contenedor activado
  contenedores_activos <- contenedores_activos %>%
    group_by(gid) %>%
    slice_max(Fecha, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # Calcular días de acumulación para contenedores recién activados
  contenedores_activos <- contenedores_activos %>% 
    mutate(Acumulacion = as.numeric(difftime(dia, Fecha, units = "days")))
  
  # Actualizar acumulación y aplicar filtros finales
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


#' Modifica el informe diario para contenedores levantados por grúa/pluma
#' 
#' Actualiza el informe diario con información de contenedores que fueron 
#' levantados por grúa o pluma pero no fueron registrados en el sistema
#' de llenado (tablet).
#' 
#' @param df_informediario Dataframe con el informe diario a modificar
#' @return Dataframe con el informe diario actualizado
funcion_modificar_informe_diario <- function(df_informediario){
  
  # Ruta de archivo para cambios de grúa/pluma sin registro en llenado
  ruta_datos_sinreg <- file.path(ruta_proyecto, "scripts/estado_diario/cambios_estado_diario_gruapluma_sin_registrarllenado.rds")
  
  # Verificar si existe archivo de cambios históricos
  cambios_estado_diario_gruapluma_sin_registrarllenado <- if (file.exists(ruta_datos_sinreg)) {
    readRDS(ruta_datos_sinreg)
  } else {
    character(0)
  }
  
  # Si hay datos históricos, procesarlos junto con los nuevos
  if (length(cambios_estado_diario_gruapluma_sin_registrarllenado) > 0){
    
    # Obtener fecha actual del informe
    dia <- max(df_informediario$Fecha)
    
    # Obtener contenedores que necesitan actualización
    contenedores_a_modificar_acumulacion <- funcion_obtener_df_de_contenedores_a_modificar_acumulacion(
      dia, 
      df_informediario
    ) %>% 
      distinct(gid, Fecha, .keep_all = TRUE)
    
    # Actualizar informe con los contenedores que requieren modificación
    informe_diario_corregido <- df_informediario %>%
      rows_update(contenedores_a_modificar_acumulacion, by = c("gid", "Fecha"))
    
    # Preparar datos a guardar (contenedores modificados hoy)
    datos_a_guardar <- informe_diario_corregido %>%
      filter(DB == "ViajesEnUnPeriodo")
    
    # Obtener cambios históricos relevantes para hoy
    cambios_estado_diario_gruapluma_sin_registrarllenado_para_hoy <- 
      cambios_estado_diario_gruapluma_sin_registrarllenado %>% 
      filter(Fecha < dia) %>% 
      select(gid, Fecha, Id_viaje, Circuito_corto, Posicion, DB, 
             Turno_levantado, Porcentaje_llenado, Fecha_hora_pasaje, Acumulacion) %>% 
      mutate(Acumulacion_actual = as.integer(dia - Fecha) + 1)
    
    # Identificar contenedores históricos presentes en el informe actual
    cambios_historicos_sinhoy <- informe_diario_corregido %>% 
      semi_join(cambios_estado_diario_gruapluma_sin_registrarllenado_para_hoy, by = "gid")
    
    # Separar contenedores no levantados por camión lateral (acumulación > 1)
    cambios_historicos_sinhoy_acumulacion_mayora1 <- cambios_historicos_sinhoy %>% 
      filter(Acumulacion != 1)
    
    # Actualizar informe con contenedores históricos no levantados
    informe_diario_corregido <- informe_diario_corregido %>%
      rows_update(cambios_historicos_sinhoy_acumulacion_mayora1, by = "gid")
    
    # Identificar contenedores ya levantados (acumulación = 1)
    cambios_historicos_sinhoy_acumulacion_iguala1 <- cambios_historicos_sinhoy %>% 
      filter(Acumulacion == 1)
    
    # Actualizar datos a guardar según estado de contenedores
    if(nrow(cambios_historicos_sinhoy_acumulacion_iguala1) > 0){
      
      # Eliminar del histórico los contenedores ya levantados
      datos_sin_contenedores_levantadosenlateral <- anti_join(
        cambios_estado_diario_gruapluma_sin_registrarllenado, 
        cambios_historicos_sinhoy_acumulacion_iguala1, 
        by = "gid"
      )
      
      datos_a_guardar <- bind_rows(datos_a_guardar, datos_sin_contenedores_levantadosenlateral)
      
    } else {
      # Mantener históricos para contenedores no levantados
      resultado <- cambios_estado_diario_gruapluma_sin_registrarllenado %>% 
        filter(gid %in% cambios_historicos_sinhoy_acumulacion_mayora1$gid)
      
      datos_a_guardar <- bind_rows(datos_a_guardar, resultado)
    }
    
  } else {
    # Procesamiento inicial si no hay datos históricos
    dia <- max(df_informediario$Fecha)
    
    # Obtener contenedores que necesitan actualización
    contenedores_a_modificar_acumulacion <- funcion_obtener_df_de_contenedores_a_modificar_acumulacion(
      dia, 
      df_informediario
    )
    
    # Actualizar informe
    informe_diario_corregido <- df_informediario %>%
      rows_update(contenedores_a_modificar_acumulacion, by = c("gid", "Fecha"))
    
    # Preparar datos a guardar
    datos_a_guardar <- informe_diario_corregido %>%
      filter(DB == "ViajesEnUnPeriodo")
  }
  
  # Guardar cambios históricos actualizados
  saveRDS(datos_a_guardar, file = ruta_datos_sinreg)
  
  return(informe_diario_corregido)
}


#' Obtiene contenedores que requieren modificación de acumulación
#' 
#' Identifica contenedores que fueron levantados por grúa/pluma sin registro
#' en sistema de llenado, para actualizar su acumulación.
#' 
#' @param fecha_consulta Fecha para la que se consultan los contenedores
#' @param df_inf_diario Dataframe con el informe diario base
#' @return Dataframe con contenedores que requieren modificación
funcion_obtener_df_de_contenedores_a_modificar_acumulacion <- function(fecha_consulta, df_inf_diario){
  
  dia <- fecha_consulta
  df_informediario <- df_inf_diario
  
  # Obtener contenedores levantados por grúa/pluma sin registro en llenado
  contenedores_levantados_por_gruapluma <- funcion_obtener_contenedores_levantados_por_grua_pluma_sin_llenado(dia)
  
  # Asignar información de turno y porcentaje de llenado
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>%
    mutate(Turno = case_when(
      Id_turno == 1 ~ "MATUTINO",
      Id_turno == 2 ~ "VESPERTINO",
      Id_turno == 3 ~ "NOCTURNO",
      TRUE ~ NA_character_
    )) %>%
    mutate(Porcentaje_llenado = 100)
  
  # Generar marca de tiempo según el turno
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>%
    mutate(Fecha_hora_pasaje = as.POSIXct(
      paste(Fecha, case_when(
        Id_turno == 1 ~ "06:00:00",
        Id_turno == 2 ~ "14:00:00",
        Id_turno == 3 ~ "22:00:00"
      )),
      format = "%Y-%m-%d %H:%M:%S"
    )) 
  
  # Seleccionar campos relevantes
  contenedores_levantados_por_gruapluma <- contenedores_levantados_por_gruapluma %>% 
    select(Fecha, Id_viaje, Circuito_corto, Posicion, DB, Turno, 
           Porcentaje_llenado, Fecha_hora_pasaje)
  
  # Identificar contenedores que requieren modificación (con acumulación > 1)
  contenedores_a_modificar_acumulacion <- inner_join(
    df_informediario, 
    contenedores_levantados_por_gruapluma, 
    by = c("Fecha", "Circuito_corto", "Posicion")
  ) %>% 
    filter(Acumulacion > 1)
  
  # Preparar dataframe final con las modificaciones necesarias
  contenedores_a_modificar_acumulacion <- contenedores_a_modificar_acumulacion %>% 
    select(gid, Fecha, Acumulacion, Id_viaje.y, DB.y, 
           Porcentaje_llenado.y, Turno, Fecha_hora_pasaje.y) %>% 
    rename(
      Id_viaje = Id_viaje.y,
      Porcentaje_llenado = Porcentaje_llenado.y,
      Turno_levantado = Turno,
      Fecha_hora_pasaje = Fecha_hora_pasaje.y,
      DB = DB.y
    ) %>% 
    mutate(
      Acumulacion = 1, # Resetear acumulación a 1 día
      Numero_caja = NA,
      Incidencia = NA_character_,
      Condicion = ""
    ) 
  
  return(contenedores_a_modificar_acumulacion)
}


#' Lista ubicaciones únicas con y sin información geométrica
#' 
#' Identifica las ubicaciones que tienen o no tienen coordenadas geométricas,
#' procesando las diferentes direcciones asociadas a cada GID.
#' 
#' @return Lista con dos elementos: ubicaciones con geometría y sin geometría
funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom <- function(){
  
  # Preparar dataframe de ubicaciones con dirección completa
  ubicaciones_totales <- historico_ubicaciones %>% 
    mutate(Direccion = ifelse(
      is.na(historico_ubicaciones$Numero),
      historico_ubicaciones$Calle,
      paste(historico_ubicaciones$Calle, historico_ubicaciones$Numero)
    ))
  
  # Extraer direcciones únicas para cada GID (primera ocurrencia)
  ubicaciones_cambios_direccion_unica <- ubicaciones_totales %>%
    group_by(gid, Direccion) %>%
    slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%
    ungroup() %>% 
    select(gid, Fecha, Estado, Municipio, Circuito_corto, Posicion, 
           Calle, Numero, Direccion, Observaciones) %>% 
    group_by(gid) %>%
    mutate(n_dir = n_distinct(Direccion)) %>% # Contar direcciones por GID
    ungroup() %>% 
    # Filtrar circuitos específicos
    filter(!(Circuito_corto %in% paste0("B_0", 1:7)))
  
  # Total de GIDs por cantidad de direcciones
  total_gid_ubicaciones_cambios_direccion_unica <- ubicaciones_cambios_direccion_unica %>% 
    group_by(gid) %>% 
    summarise(total = n())
  
  # Obtener GIDs únicos en llenado
  unique_gids <- historico_llenado %>% 
    distinct(gid)
  
  # Identificar GIDs que no están en llenado
  gid_que_no_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
    anti_join(unique_gids, by = "gid")
  
  # Identificar GIDs que sí están en llenado
  gid_que_si_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
    anti_join(gid_que_no_estan_ubicaciones, by = "gid")
  
  # Obtener direcciones del histórico de llenado
  direcciones_de_llenado <- historico_llenado %>%
    group_by(gid, Direccion) %>%
    slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%
    ungroup() %>% 
    select(gid, Fecha, Municipio, Circuito_corto, Posicion, Direccion, the_geom)
  
  # BLOQUE 1: Asociar geometría por coincidencia de GID y Dirección exacta
  ubicaciones_por_direccion_gid_iguales <- gid_que_si_estan_ubicaciones %>%
    left_join(
      direcciones_de_llenado %>% select(gid, Direccion, the_geom),
      by = c("gid", "Direccion")
    ) 
  
  # Identificar registros sin geometría
  NA_de_ubicaciones_por_direccion_gid_iguales <- ubicaciones_por_direccion_gid_iguales %>% 
    filter(is.na(the_geom))
  
  # Separar registros con geometría
  bloque_1_direcciones <- ubicaciones_por_direccion_gid_iguales %>% 
    anti_join(NA_de_ubicaciones_por_direccion_gid_iguales, by = "gid")
  
  # BLOQUE 2: Asociar geometría por coincidencia de GID y Calle
  ubicaciones_por_calle_y_gid_iguales <- NA_de_ubicaciones_por_direccion_gid_iguales %>%
    left_join(
      direcciones_de_llenado %>% select(gid, Direccion, the_geom),
      by = c("gid", "Calle" = "Direccion")
    ) %>% 
    select(-the_geom.x) %>% 
    rename(the_geom = the_geom.y)
  
  # Identificar registros sin geometría
  NA_ubicaciones_por_calle_y_gid_iguales <- ubicaciones_por_calle_y_gid_iguales %>% 
    filter(is.na(the_geom))
  
  # Separar registros con geometría
  bloque_2_calles <- ubicaciones_por_calle_y_gid_iguales %>% 
    anti_join(NA_ubicaciones_por_calle_y_gid_iguales, by = "gid")
  
  # Combinar registros con geometría de bloques 1 y 2
  the_geom_ubicaciones <- bind_rows(bloque_1_direcciones, bloque_2_calles)
  
  # BLOQUE 3: Procesar ubicaciones sin geometría por dirección ni calle
  ubicaciones_diferente_direccion_y_calle <- NA_ubicaciones_por_calle_y_gid_iguales 
  
  # Separar ubicaciones sin cambios de dirección (n_dir = 1)
  ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- 
    ubicaciones_diferente_direccion_y_calle %>% 
    filter(n_dir == 1) %>% 
    select(-the_geom)
  
  # Obtener GIDs únicos
  unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- 
    ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
    distinct(gid)
  
  # Buscar geometrías por GID en llenado
  the_geom_faltantes <- historico_llenado %>% 
    inner_join(
      unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion,
      by = "gid"
    ) %>% 
    distinct(gid, Direccion, the_geom)
  
  # Completar geometrías faltantes por GID
  ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final <- 
    ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
    left_join(
      the_geom_faltantes %>% select(gid, the_geom),
      by = "gid"
    )
  
  # Combinar todos los registros con geometría
  the_geom_ubicaciones <- bind_rows(
    the_geom_ubicaciones,
    ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final
  )
  
  # Identificar ubicaciones con cambios de dirección y sin geometría
  ubicaciones_diferente_direccion_y_calle_con_cambios_de_direccion <- 
    ubicaciones_diferente_direccion_y_calle %>% 
    filter(n_dir > 1) %>% 
    select(-the_geom)
  
  # Retornar lista con ubicaciones con y sin geometría
  return(list(
    ubicaciones_con_thegeom = the_geom_ubicaciones, 
    ubicaciones_sin_thegeom = ubicaciones_diferente_direccion_y_calle_con_cambios_de_direccion
  ))
}


#' Agrega información geométrica a registros que carecen de ella
#' 
#' Completa los datos geométricos en registros que no los tienen,
#' buscando coincidencias por GID y dirección.
#' 
#' @param df_singeom Dataframe con registros que pueden carecer de geometría
#' @param ubi_completa Dataframe con ubicaciones que tienen geometría
#' @return Dataframe con geometrías completadas donde sea posible
funcion_agregar_the_geom_a_faltantes <- function(df_singeom, ubi_completa){
  
  # Normalizar tipos de datos para garantizar compatibilidad
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
  
  # Separar registros con y sin geometría
  estado_diario_con_thegeom_historico <- df_singeom %>% 
    filter(!is.na(the_geom))
  
  estado_diario_sin_thegeom <- df_singeom %>% 
    filter(is.na(the_geom))
  
  # Procesar solo si hay registros sin geometría
  if(nrow(estado_diario_sin_thegeom) > 0){
    
    # Normalizar campo de dirección
    estado_diario_sin_thegeom <- estado_diario_sin_thegeom %>%
      mutate(Direccion = if_else(
        is.na(Numero),
        as.character(Calle),
        paste0(as.character(Calle), " ", as.character(Numero)),
        missing = NA_character_
      ))
    
    # Buscar y aplicar geometrías por coincidencia de GID y dirección
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
    
    # Combinar con registros que ya tenían geometría
    retorno <- bind_rows(estado_diario_con_thegeom_historico, estado_diario_con_thegeom)
  }
  
  return(retorno)
}


#' Arregla el formato de un informe diario antiguo
#' 
#' Convierte el formato antiguo de informe diario al nuevo formato,
#' reorganizando las columnas y renombrándolas según necesidad.
#' 
#' @note Función no utilizada actualmente
#' @return Dataframe con el formato actualizado
arreglar_informediario_viejo <- function(){
  ruta <- file.path(ruta_proyecto, "scripts/estado_diario/estado_diario_versionanterior.rds")
  anterior <- read_rds(ruta) 
  
  historico_estadodiario_versionanterior <- anterior %>% 
    select(gid, Nomenclatura, Municipio, Circuito, Posicion, Direccion, the_geom, Acumulacion) %>% 
    rename(
      Circuito_corto = Circuito,
      Circuito = Nomenclatura
    )
  
  # Función sin finalizar - pendiente implementación completa
  return(historico_estadodiario_versionanterior)
} 
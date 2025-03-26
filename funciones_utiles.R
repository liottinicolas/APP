# Funcion para obtener las ubicaciones para un dia determinado
# El día de parametro es el dia del informe
funcion_obtener_ubicaciones_por_dia <- function(dia){
  
  dia <- dia-1
  
  ubicaciones_por_dia <- historico_ubicaciones %>% 
    filter(Fecha == dia)
  
  return(ubicaciones_por_dia)
}

# Funcion para obtener los contenedores nuevos que se agregaron con respecto al día anterior
# NUEVOS GIDS, no cambios de ubicación.
funcion_obtener_contenedores_agregados <- function(dia_informe){
  
  # dia_informe <- dia_actual
  dia_informe_anterior <- dia_informe -1
  
  # Busco los días que voy a hacer el informe diario
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_nuevos_agregados <- ubicaciones_dia_informe %>%
    anti_join(
      ubicaciones_dia_anterior_informe,
      by = "gid"  # Única columna para comparar
    )
  
  return(contenedores_nuevos_agregados)
  
}

# Funcion para obtener los contenedores nuevos que se eliminaron con respecto al día anterior
# NUEVOS GIDS, no cambios de ubicación.
funcion_obtener_contenedores_eliminados <- function(dia_informe){
  
  dia_informe_anterior <- dia_informe -1
  
  # Busco los días que voy a hacer el informe diario
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_nuevos_eliminados <- ubicaciones_dia_anterior_informe %>%
    anti_join(
      ubicaciones_dia_informe,
      by = "gid"  # Única columna para comparar
    )
  
  return(contenedores_nuevos_eliminados)
  
}

# Funcion para obtener todos los contenedores que de un día para el otro cambiaron su estado
# Es sin filtro, De mantenimiento a activos y activos a mantenimiento.
funcion_obtener_contenedores_cambio_de_estado_del_dia <- function(dia_informe){
  
  #  dia_informe <- dia_actual
  dia_informe_anterior <- dia_informe -1
  
  # Busco los días que voy a hacer el informe diario
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
  
  contenedores_con_cambio_de_estado <- ubicaciones_dia_informe %>%
    anti_join(
      ubicaciones_dia_anterior_informe,
      by = c("gid","Estado")  # Única columna para comparar
    )
  
  return(contenedores_con_cambio_de_estado)
}

# Funcion para obtener todos los contenedores que de un día para el otro cambiaron su estado
# Aquellos que cambiaron de activos a mantenimiento.
funcion_obtener_contenedores_activos_a_mantenimiento_del_dia <- function(dia_informe){
  
  df <- funcion_obtener_contenedores_cambio_de_estado_del_dia(dia_informe) %>% 
    filter(Estado == "Mantenimiento")
  
  return(df)
}

funcion_obtener_cambios_de_contenedores_del_dia <- function(dia_informe){
  
  dia_anterior_informe <- dia_informe-1
  
  ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
  ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_anterior_informe)
  
  ### Anti join. Eliminar nuevos y eliminados de un dia para el otro
  # 1. Determina las columnas en común entre ambos dataframes
  columnas_comunes <- intersect(names(ubicaciones_dia_informe), names(ubicaciones_dia_anterior_informe))
  
  # 2. Elimina la(s) columna(s) que NO quieres usar para el join
  columnas_para_unir <- setdiff(columnas_comunes, "Fecha")
  
  # 3. Este dataframe muestra todos los cambios generados del día anterior
  # Toma cambios como. 
  # 1- Mantenimiento - No mantenimiento.
  # 2- Agregado de contenedor entre medio (es decir, si en la posicion 5 se agregan un contenedor, el cirpos del 6 en adelante cambia)
  # 3- Direcciones/observaciones.
  # 4- Nuevos GID.
  cambios_posiciones_y_estados <- ubicaciones_dia_informe %>%
    anti_join(ubicaciones_dia_anterior_informe, by = columnas_para_unir)
  
  return(cambios_posiciones_y_estados)
}

funcion_obtener_contenedores_mantenimientos_a_activos_del_dia <- function(dia_informe){
  
  hoy <- dia_informe
  ayer<- dia_informe -1
  cambios_hoy <- funcion_obtener_contenedores_cambio_de_estado_del_dia(hoy)
  cambios_ayer <- funcion_obtener_contenedores_cambio_de_estado_del_dia(ayer)
  
  agregados_hoy <- funcion_obtener_contenedores_agregados_historico(inicio,hoy)
  agregados_ayer <- funcion_obtener_contenedores_agregados_historico(inicio,ayer)

  
  return(df)
}

#### Funcion para obtener los levantados del dia.
funcion_contenedores_levantados_del_dia <- function(dia_informe){
  levantados <- historico_llenado %>% 
    filter(Levantado == "S") %>% 
    filter(Fecha == dia_informe)
  
  return(levantados)
}

#### Funcion para obtener los inactivos levantados en el día
funcion_contenedores_levantados_del_dia_inactivos <- function(dia_informe){
  levantados <- funcion_contenedores_levantados_del_dia(dia_informe)
  levantados <- levantados %>% 
    filter(Id_motivo_inactiva == 1)
  
  return(levantados)
}

eliminar_gids_inactivos <- function(ubicaciones,ubicacion_deldia){
  ubicaciones_repetidas <- ubicaciones %>%
    group_by(Circuito, Posicion, Fecha) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(gid, Circuito, Posicion, Fecha)
  
  # Eliminar duplicados considerando solo las columnas gid, Circuito y Posicion
  ubicaciones_repetidas_sin_duplicados <- ubicaciones_repetidas %>% 
    distinct(gid, Circuito, Posicion, .keep_all = TRUE) %>% 
    select(-Fecha)
  
  # Unir los dataframes por gid y agregar la columna Acumulacion
  ubicaciones_actualizado <- ubicaciones_repetidas_sin_duplicados %>%
    left_join(ubicacion_deldia %>% select(gid, Acumulacion), by = "gid")
  ubicaciones_actualizado <- ubicaciones_actualizado %>% 
    filter(!is.na(Acumulacion))
  
  # Filtrar para quedarnos con la fila de menor Acumulacion en caso de duplicados
  ubicaciones_filtrado <- ubicaciones_actualizado %>%
    group_by(Circuito, Posicion) %>%
    slice_min(Acumulacion, with_ties = FALSE) %>%
    ungroup()
  
  # Eliminar filas en ubicaciones_repetidas si el gid existe en ubicaciones_filtrado
  ubicaciones_repetidas_filtrado <- ubicaciones_repetidas_sin_duplicados %>%
    anti_join(ubicaciones_filtrado, by = "gid")
  
  return(ubicaciones_repetidas_filtrado)
}

# funcion_obtener_contenedores_agregados_historico <- function(inicio, fin) {
#   
#   # Si la variable global no existe, la creamos
#   if (!exists("historico_contenedores_agregados")) {
#     historico_contenedores_agregados <<- data.frame()
#   }
#   
#   # Iterar sobre el rango de fechas
#   for (dia in seq.Date(inicio, fin, by = "day")) {
#     
#     # Obtener los contenedores nuevos agregados para este día
#     contenedores_nuevos_agregados <- funcion_obtener_contenedores_agregados(dia)
#     
#     # Guardar en el histórico global si hay datos nuevos
#     if (nrow(contenedores_nuevos_agregados) > 0) {
#       historico_contenedores_agregados <<- bind_rows(historico_contenedores_agregados, contenedores_nuevos_agregados)
#     }
#   }
#   
#   # Eliminar duplicados después del bucle para conservar solo la primera aparición
#   historico_contenedores_agregados <<- historico_contenedores_agregados %>%
#     distinct(gid, .keep_all = TRUE) %>%
#     arrange(desc(Fecha))  # Ordenar por fecha en orden descendente
#   
#   return(historico_contenedores_agregados)
# }
# 
# funcion_obtener_contenedores_eliminados_historico <- function(inicio, fin) {
#   
#   # Si la variable global no existe, la creamos
#   if (!exists("historico_contenedores_eliminados")) {
#     historico_contenedores_eliminados <<- data.frame()
#   }
#   
#   # Iterar sobre el rango de fechas
#   for (dia in seq.Date(inicio, fin, by = "day")) {
#     
#     # Obtener los contenedores nuevos agregados para este día
#     contenedores_nuevos_eliminados <- funcion_obtener_contenedores_eliminados(dia)
#     
#     # Guardar en el histórico global si hay datos nuevos
#     if (nrow(contenedores_nuevos_eliminados) > 0) {
#       historico_contenedores_eliminados <<- bind_rows(historico_contenedores_eliminados, contenedores_nuevos_eliminados)
#     }
#   }
#   
#   # Eliminar duplicados después del bucle para conservar solo la primera aparición
#   historico_contenedores_eliminados <<- historico_contenedores_eliminados %>%
#     distinct(gid, .keep_all = TRUE) %>%
#     arrange(desc(Fecha))  # Ordenar por fecha en orden descendente
#   
#   return(historico_contenedores_eliminados)
# }



# funcion_obtener_contenedores_cambiodeestado_historico <- function(inicio, fin) {
#   
#   # Si la variable global no existe, la creamos
#   if (!exists("historico_contenedores_cambiados")) {
#     historico_contenedores_cambiados <<- data.frame()
#   }
#   
#   # Iterar sobre el rango de fechas
#   for (dia in seq.Date(inicio, fin, by = "day")) {
#     
#     # Obtener los contenedores nuevos agregados para este día
#     contenedores_cambiados <- funcion_obtener_contenedores_cambio_de_estado_del_dia(dia)
#     
#     # Guardar en el histórico global si hay datos nuevos
#     if (nrow(contenedores_cambiados) > 0) {
#       historico_contenedores_cambiados <<- bind_rows(historico_contenedores_cambiados, contenedores_cambiados)
#     }
#   }
#   
#   # Eliminar duplicados después del bucle para conservar solo la primera aparición
#   historico_contenedores_cambiados <<- historico_contenedores_cambiados %>%
#     distinct(gid, .keep_all = TRUE) %>%
#     arrange(desc(Fecha))  # Ordenar por fecha en orden descendente
#   
#   return(historico_contenedores_cambiados)
# }

# funcion_obtener_contenedores_de_mantenimiento_a_activos_historico <- function(inicio, fin) {
#   
#   # Si la variable global no existe, la creamos
#   if (!exists("historico_contenedores_mantenimiento_a_activos")) {
#     historico_contenedores_mantenimiento_a_activos <<- data.frame()
#   }
#   
#   # Iterar sobre el rango de fechas
#   for (dia in seq.Date(inicio, fin, by = "day")) {
#     
#     # Obtener los contenedores nuevos agregados para este día
#     contenedores_nuevos_activos <- funcion_obtener_cambios_de_contenedores_del_dia(dia)
#     
#     #### LLAMO EL HISTORICO DE NUEVOS CONTENEDORES PARA QUE ELIMINE LOS QUE SE AGREGARON POR NUEVOS.
#     historico_agregados <- funcion_obtener_contenedores_agregados_historico(inicio,fin)
#     historico_agregados<- historico_agregados %>% 
#       filter(Fecha <= dia)
#     
#     contenedores_filtrados <- anti_join(contenedores_nuevos_activos, historico_agregados, by = "gid")
#     
#     
#     # Guardar en el histórico global si hay datos nuevos
#     if (nrow(contenedores_nuevos_activos_filtrado) > 0) {
#       historico_contenedores_agregados <<- bind_rows(historico_contenedores_agregados, contenedores_nuevos_agregados)
#     }
#   }
#   
#   # Eliminar duplicados después del bucle para conservar solo la primera aparición
#   historico_contenedores_agregados <<- historico_contenedores_agregados %>%
#     distinct(gid, .keep_all = TRUE) %>%
#     arrange(desc(Fecha))  # Ordenar por fecha en orden descendente
#   
#   return(historico_contenedores_agregados)
# }

# funcion_guardar_historico_modificaciones <- function(ubicaciones){
  
  # ubicaciones <- historico_ubicaciones
  
#   ruta_historico_cambios <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/historico_cambios_estado.rds")
#   
#   # Cargo las ubicaciones modificadas que existen.
#   ubicaciones_ya_cambiadas <- if (file.exists(ruta_historico_cambios)) {
#     readRDS(ruta_historico_cambios)
#   } else {
#     character(0)
#   }
#   
#   # Si existen 
#   # Busco la ultima fecha ingresada.
#   if (length(ubicaciones_ya_cambiadas) > 0) {
#     
#     # Fecha máxima ingresada
#     max_fecha_ingresada <- max(ubicaciones_ya_cambiadas$Fecha, na.rm = TRUE)
#     inicio <- max_fecha + 1
#     
#     max_fecha <- max(ubicaciones$Fecha, na.rm = TRUE)
#     
#     fechas <- seq(inicio, max_fecha, by = "day")
#     
#     # Generar una secuencia de fechas de un día en uno
#     fechas <- seq(inicio, max_fecha, by = "day")
#     
#     # # Inicializamos un data frame vacío para almacenar los resultados
#     # resultados <- data.frame()
#     
#     # Iterar sobre cada fecha
#     for (fecha in fechas) {
#       
#       # inicio <- inicio +1
#       #  dia_anterior <- inicio - 1
#       #  dia_actual <- dia_anterior +1
#       
#       dia_anterior <- fecha - 1
#       dia_actual <- fecha
#       
#       # Busco las ubicaciones que tiene cada dia
#       dia_anterior_ubicaciones <- funcion_obtener_ubicaciones_por_dia(dia_anterior)
#       dia_actual_ubicaciones <- funcion_obtener_ubicaciones_por_dia(dia_actual)
#       
#       # Busco los contenedores que se agregaron de un dia para el otro.
#       ## NO LOS QUE CAMBIARON DE ESTADO.
#       nuevos <- funcion_obtener_contenedores_agregados(dia_actual)
#       
#       ### Hago el nuevo dia_Actual_ubicaciones eliminando los contenedores agregados nuevos
#       # Asi busco los que cambiaron solamente de estado.
#       dia_actual_ubicaciones_sin_contenedores_nuevos_agregados <- anti_join(dia_actual_ubicaciones,nuevos ,by = "gid")
#       
#       ## Busco los contenedores que cambiaron de estado
#       #contenedores_cambio_de_estado <- funcion_obtener_contenedores_cambio_de_estado_del_dia(dia_actual)
#     
#       # Busco los días que voy a hacer el informe diario
#       # ubicaciones_dia_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe)
#       # ubicaciones_dia_anterior_informe <- funcion_obtener_ubicaciones_por_dia(dia_informe_anterior)
#       
#       contenedores_con_cambio_de_estado <- dia_actual_ubicaciones %>%
#         anti_join(
#           dia_anterior_ubicaciones,
#           by = c("gid","Estado")  # Única columna para comparar
#         )
#       
#       
#       # elimino los contenedores que fueron nuevos agregados dejando solo los que cambiaron de estado.
#       cambarion_estado <- anti_join(contenedores_con_cambio_de_estado ,nuevos , by ="gid")
#       
#       ubicaciones_ya_cambiadas <- bind_rows(ubicaciones_ya_cambiadas, cambarion_estado)
#       
#       
#       # inicio <- inicio +1 
#       
#       
#       # Aquí puedes colocar el código que deseas ejecutar para cada fecha
#     } 
#     
#     
#     
#   } else {
#     min_fecha <- min(ubicaciones$Fecha, na.rm = TRUE)
#     max_fecha <- max(ubicaciones$Fecha, na.rm = TRUE)
#     
#     # desde minimo + 1
#     inicio <- min_fecha + 2
#     
#     # Generar una secuencia de fechas de un día en uno
#     fechas <- seq(inicio, max_fecha, by = "day")
#     
#     # Inicializamos un data frame vacío para almacenar los resultados
#     ubicaciones_ya_cambiadas <- data.frame()
#     
#     # Iterar sobre cada fecha
#     for (fecha in fechas) {
#       
#       # inicio <- inicio +1
#       # dia_anterior <- inicio - 1
#       # dia_actual <- dia_anterior +1
#       
#       dia_anterior <- fecha - 1
#       dia_actual <- fecha
#       
#       # Busco las ubicaciones que tiene cada dia
#       dia_anterior_ubicaciones <- funcion_obtener_ubicaciones_por_dia(dia_anterior)
#       dia_actual_ubicaciones <- funcion_obtener_ubicaciones_por_dia(dia_actual)
#       
#       # Busco los contenedores que se agregaron de un dia para el otro.
#       ## NO LOS QUE CAMBIARON DE ESTADO.
#       nuevos <- funcion_obtener_contenedores_agregados(dia_actual)
#       
#       ### Hago el nuevo dia_Actual_ubicaciones eliminando los contenedores agregados nuevos
#       # Asi busco los que cambiaron solamente de estado.
#       dia_actual_ubicaciones_sin_contenedores_nuevos_agregados <- anti_join(dia_actual_ubicaciones,nuevos ,by = "gid")
#       
#       ## Busco los contenedores que cambiaron de estado
#       contenedores_cambio_de_estado <- funcion_obtener_contenedores_cambio_de_estado_del_dia(dia_actual)
#       
#       # elimino los contenedores que fueron nuevos agregados dejando solo los que cambiaron de estado.
#       cambarion_estado <- anti_join(contenedores_cambio_de_estado ,nuevos , by ="gid")
#       
#       ubicaciones_ya_cambiadas <- bind_rows(ubicaciones_ya_cambiadas, cambarion_estado)
#       
#       
#       # inicio <- inicio +1 
#       
#       }    
#     
#     
#   }
#   
#   # Guardar el histórico actualizado en el archivo RDS
#   saveRDS(ubicaciones_ya_cambiadas, file = ruta_historico_cambios)
#   
#   
#   return(ubicaciones_ya_cambiadas)
#   
#   
# }

imprimir_repetidos <- function(df){
  duplicados <- df %>%
    group_by(across(everything())) %>%  # agrupar por todas las columnas
    filter(n() > 1) %>%                 # filtrar grupos con más de una fila
    ungroup()
  
  return(duplicados)
}





funcion_imprimir_incidencias_del_dia_por_responsable <- function(incidencias_por_gid,fecha_consulta,responsable){
  incidencias_del_dia <- incidencias_por_gid %>% 
    filter(Fecha == fecha_consulta) %>% 
    filter(Responsable == responsable) 
  
}

# inicio <- as.Date("2025-02-20")
# fin <- max(historico_estado_diario$Fecha)
# fecha_consulta <- fin
# incidencias_por_gid <- historico_completo_llenado_incidencias
# responsable <- "Grua"
# estado_diario <- historico_estado_diario

funcion_mostrar_responsables_por_incidencias <- function(incidencias_por_gid,estado_diario,inicio,fin,responsable){
  
  # Calculo las incidencias que se generaron el día anterior
  incidencias_del_dia <- funcion_imprimir_incidencias_del_dia_por_responsable(incidencias_por_gid,fin,responsable)
  
  ## calculo del día, los días de acumulación.
  acumulacion_select <- estado_diario %>%
    select(gid, Fecha, Acumulacion,Estado)

  incidencias_del_dia <- incidencias_del_dia %>%
    left_join(acumulacion_select, by = c("gid", "Fecha")) %>% 
    rename(Fecha_incidencia = Fecha)
  
  # Elimino las que ya fueron solucionadas
  incidencias_del_dia_sinsolucion <- incidencias_del_dia %>% 
    filter(Acumulacion > 1)
  
  # Calculo todas las indicencias anteriores al día anterior
  incidencias_historico_sin_ultimo_dia <- incidencias_por_gid %>% 
    filter(Fecha >= inicio) %>% 
    filter(Fecha < fin) %>% 
    filter(Responsable == responsable) %>% 
    rename(Fecha_incidencia = Fecha)
  
  # Hallo el valor de acumulacion al dia de hoy.
  informe_del_dia <- estado_diario %>% 
    filter(Fecha == fin)

    # Le agrego a las incidencias historicas, el día de acumulacion a la fecha de la consulta.
  # y al día de hoy en otra columna.
  incidencias_actualizadas_sin_ultimo_dia <- incidencias_historico_sin_ultimo_dia %>%
    left_join(
      informe_del_dia %>% 
        select(gid, Fecha, Acumulacion,Estado) %>%
        rename(fecha_informe_dia_actual = Fecha,
               Acumulacion_dia_actual = Acumulacion),
      by = "gid"
    ) %>% 
    left_join(relationship = "many-to-many",
      estado_diario %>% 
        select(gid, Fecha, Acumulacion) %>%
        rename(fecha_informe_dia_incidencia = Fecha,
               Acumulacion_dia_incidencia = Acumulacion),
      by = c("gid" = "gid", "Fecha_incidencia" = "fecha_informe_dia_incidencia")
    ) %>% 
    # rename(Acumulacion_dia_incidencia = Acumulacion_dia_actual) %>% 
    mutate(Diferencia_dias = fecha_informe_dia_actual - Fecha_incidencia) %>% 
    mutate(Diferencias_dias_de_acumulacion = Acumulacion_dia_actual - Acumulacion_dia_incidencia) %>% 
    filter(Acumulacion_dia_incidencia > 1)
  
  
  ## Filtro aquellas incidencias que no fueron solucionados
  # Resto las fechas y los dias de acumulacion, si da igual es porque no paso nada
  incidencias_actualizadas_sin_ultimo_dia <- incidencias_actualizadas_sin_ultimo_dia %>%
    filter(as.numeric(Diferencia_dias, units = "days") == Diferencias_dias_de_acumulacion) %>% 
    ### Filtro las que están en mantenimiento
    filter(is.na(Estado)) %>% 
    select(Fecha_incidencia,Diferencia_dias,gid,Municipio,Circuito_corto,Posicion,Direccion,Observaciones,Incidencia,Acumulacion_dia_actual) %>% 
    rename(Acumulacion = Acumulacion_dia_actual)
  
  # incidencias_actualizadas_sin_ultimo_dia <- incidencias_final %>% 

  
  incidencias_del_dia_sinsolucion <- incidencias_del_dia_sinsolucion %>% 
    mutate(Diferencia_dias = as.difftime(0, units = "days")) %>% 
    select(Fecha_incidencia,Diferencia_dias,gid,Municipio,Circuito_corto,Posicion,Direccion,Observaciones,Incidencia,Acumulacion)
  
  incidencias_completas <- bind_rows(incidencias_actualizadas_sin_ultimo_dia,incidencias_del_dia_sinsolucion) %>% 
    mutate(Diferencia_dias = Diferencia_dias + 1) %>% 
    arrange(desc(Diferencia_dias),Circuito_corto)
  
  df_sin_duplicados <- incidencias_completas %>%
    group_by(gid, Fecha_incidencia) %>%
    filter(n() == 1) %>%  # Sólo se conservan las combinaciones que aparecen una única vez
    ungroup()
  
  df_retorno <- df_sin_duplicados %>%
    group_by(gid) %>%
    filter(Fecha_incidencia == min(Fecha_incidencia)) %>%
    ungroup()
  # 
  # ### Busco los the_geom faltantes.
  # the_geom_totales <- ubicaciones_existentes$ubicaciones_con_thegeom
  # 
  # df_retorno <- df_retorno %>%
  #   left_join(
  #     the_geom_totales %>%
  #       select(gid, Direccion, the_geom) %>%
  #       rename(the_geom_new = the_geom),
  #     by = c("gid", "Direccion")
  #   ) %>%
  #   mutate(the_geom = coalesce(the_geom, the_geom_new)) %>%
  #   select(-the_geom_new)
  # 
  
  #### TRATANDO DE AGREGAR THEGEOM
  
  buscar_ubis <- ubicaciones_existentes$ubicaciones_con_thegeom
  
  # Unir y calcular diferencia de días entre fechas
  unido <- df_retorno %>%
    left_join(buscar_ubis, by = "gid") %>%
    mutate(dif_dias = abs(as.numeric(Fecha - Fecha_incidencia)))
  
  # Seleccionar el the_geom con menor diferencia de fecha por gid + Fecha_incidencia
  the_geom_mas_cercano <- unido %>%
    group_by(gid, Fecha_incidencia) %>%
    slice_min(order_by = dif_dias, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(gid, Fecha_incidencia, the_geom)
  
  # Unir el resultado a df_retorno
  df_retorno_final <- df_retorno %>%
    left_join(the_geom_mas_cercano, by = c("gid", "Fecha_incidencia"))
  
  
  return(df_retorno_final)
  
}

# tipo indicdencia
# Grua o Pluma
funcion_exportar_incidencias_grua_o_pluma <- function(df,tipo_incidencia){
  
  wb <- createWorkbook()
  
  # Añadir una hoja
  addWorksheet(wb, "Datos")
  
  # Escribir el data frame como tabla con formato en la hoja "Datos"
  writeDataTable(wb, sheet = "Datos", x = df, tableStyle = "TableStyleLight9")
  
  # Ajustar automáticamente el ancho de todas las columnas (1 a ncol(df_filtrado))
  setColWidths(wb, sheet = "Datos", cols = 1:ncol(df), widths = "auto")
  
  if (tipo_incidencia == "Grua") {
    nombre_archivo <- "sobrepesos_actualizados.xlsx"
  } else if (tipo_incidencia == "Pluma"){
    nombre_archivo <- "pluma_actualizados.xlsx"
  }
  
  saveWorkbook(wb, file = nombre_archivo, overwrite = TRUE)
  
  
}

imprimir_csv_pordia_ubicaciones <- function(fecha_buscada){
  
  imprimir <- historico_ubicaciones %>% 
    filter(Fecha == fecha_buscada) %>% 
    rename(GID = gid,
           Recorrido = Circuito) %>% 
    select(GID, Recorrido, Posicion, Estado, Calle, Numero, Observaciones)
  
  nombre_archivo <- paste0(format(fecha_buscada, "%Y-%m-%d"), ".csv")
  
  
  
  write.table(imprimir, 
              file = nombre_archivo,
              sep = "\t", 
              row.names = FALSE, 
              quote = FALSE, 
              fileEncoding = "ISO-8859-1")
}


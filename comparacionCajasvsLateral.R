# Función para comparar cajas vs lateral
# Post: DEVUELVE dos df, uno con la información de los camiones laterales y otro convencionales.
# Solamente con aquellos que se levantaron.
comparar_cajas_vs_lateral <- function(municipio = NULL, fecha_inicio = NULL, fecha_fin = NULL) {
  
  # Crear dataframe manual con solo los Recolector/Compactador
  datos_vehiculos <- data.frame(
    Descripción = c("Recolector/Compactador", "Recolector/Compactador", "Recolector/Compactador", 
                    "Recolector/Compactador", "Recolector/Compactador", "Recolector/Compactador", 
                    "Recolector/Compactador", "Recolector/Compactador", "Recolector/Compactador", 
                    "Recolector/Compactador", "Recolector/Compactador", "Recolector/Compactador"),
    SIM = c(3020, 3021, 3018, 3019, 3022, 3041, 3042, 3043, 3069, 3068, 3067, 3066)
  )
  
  # Crear dataframe con las matrículas formateadas
  recolectores_compactadores <- datos_vehiculos %>% 
    mutate(Matricula_formateada = paste0("SIM", SIM)) %>%
    select(Matricula_formateada)
  
  # Inicializar el dataframe base
  viajes_comparacion <- web_historico_completo_llenado_incidencias %>% 
    filter(Levantado == "S")
  
  # Aplicar filtros opcionales
  if (!is.null(municipio)) {
    viajes_comparacion <- viajes_comparacion %>% 
      filter(Municipio == municipio)
  }
  
  if (!is.null(fecha_inicio)) {
    viajes_comparacion <- viajes_comparacion %>% 
      filter(Fecha >= fecha_inicio)
  }
  
  if (!is.null(fecha_fin)) {
    viajes_comparacion <- viajes_comparacion %>% 
      filter(Fecha <= fecha_fin)
  }
  
  # Crear el nuevo dataframe combinado
  viajes_comparacion <- merge(
    x = viajes_comparacion,
    y = historico_viajes[, c("Id_viaje", "Matricula")],
    by = "Id_viaje",
    all.x = TRUE
  )
  
  # Eliminar duplicados
  viajes_comparacion <- viajes_comparacion %>%
    distinct() %>%
    filter(!(Matricula %in% paste0("SIM", c(2378, 2379, 2380, 2381, 2244, 2225))))
  
  # Crear dataframe agrupado por Id_viaje con conteo de circuitos distintos
  circuitos_por_viaje <- historico_viajes %>%
    group_by(Id_viaje) %>%
    summarise(
      Cantidad_circuitos_distintos = n_distinct(Circuito),
      .groups = 'drop'
    ) %>% 
    filter(Cantidad_circuitos_distintos == 1)

    
  
  # Agregar columna de tipo de vehículo
  viajes_comparacion <- viajes_comparacion %>%
    mutate(Tipo_vehiculo = case_when(
      Matricula %in% recolectores_compactadores$Matricula_formateada ~ "CajaDesmontable",
      TRUE ~ "Convencional"
    ))
  
  # Separar en dos dataframes
  viajes_caja_desmontable <- viajes_comparacion %>%
    filter(Tipo_vehiculo == "CajaDesmontable")
  
  viajes_convencional <- viajes_comparacion %>%
    filter(Tipo_vehiculo == "Convencional")
  
  # Mostrar información del filtro aplicado
  cat("Filtros aplicados:\n")
  cat("Municipio:", ifelse(is.null(municipio), "Todos", municipio), "\n")
  cat("Fecha inicio:", ifelse(is.null(fecha_inicio), "Sin límite", as.character(fecha_inicio)), "\n")
  cat("Fecha fin:", ifelse(is.null(fecha_fin), "Sin límite", as.character(fecha_fin)), "\n")
  cat("Total de registros (sin duplicados):", nrow(viajes_comparacion), "\n\n")
  
  # Mostrar estadísticas de cada tipo
  cat("Estadísticas por tipo de vehículo:\n")
  cat("CajaDesmontable:", nrow(viajes_caja_desmontable), "registros\n")
  cat("Convencional:", nrow(viajes_convencional), "registros\n\n")

  
  # Retornar lista con ambos dataframes
  return(list(
    caja_desmontable = viajes_caja_desmontable,
    convencional = viajes_convencional
  ))
}

## Recibe un df agrupado por Id_viaje y retorna el df con la cantidad de contenedores levantados.
funcion_agregar_total_contenedores_a_levantar_por_IdViaje <- function(df_a_completar) {
  
  historico_llenado_limpio <- historico_llenado %>% 
    distinct(gid, Id_viaje, .keep_all = TRUE)
  
  df_conteo <- historico_llenado_limpio %>%
    filter(Id_viaje %in% df_a_completar$Id_viaje) %>%
    group_by(Id_viaje) %>%
    summarise(
      total_contenedores_para_levantar = n(),
      .groups = "drop"
    )
  # Hacemos left_join para no perder información de df_a_completar
  df_resultado <- df_a_completar %>%
    left_join(df_conteo, by = "Id_viaje")
  
  return(df_resultado)
}

## Recibe un df agrupado por Id_viaje y retorna el df con la cantidad de contenedores levantados.
funcion_agregar_total_contenedores_a_levantar_por_Circuito <- function(df_a_completar) {
  
  historico_llenado_limpio <- historico_llenado %>% 
    distinct(gid, Id_viaje, .keep_all = TRUE)
  
  df_conteo <- historico_llenado_limpio %>%
    filter(Circuito %in% df_a_completar$Circuito) %>%
    group_by(Fecha,Circuito) %>%
    summarise(
      total_contenedores_para_levantar = n(),
      .groups = "drop"
    )
  # Hacemos left_join para no perder información de df_a_completar
  df_resultado <- df_a_completar %>%
    left_join(df_conteo, by = "Id_viaje")
  
  return(df_resultado)
}


prueba <- funcion_agregar_total_contenedores_a_levantar_por_IdViaje(agrupado_convencional_ch)
prueba2 <- funcion_agregar_total_contenedores_a_levantar_por_Circuito(agrupado_convencional_ch)
agrupado_convencional_ch
  

# Función para agrupar por Id_viaje y sumar contenedores
agrupar_por_viaje <- function(df_viajes) {
  
  # Verificar que el dataframe no esté vacío
  if (nrow(df_viajes) == 0) {
    cat("El dataframe está vacío. No hay datos para agrupar.\n")
    return(NULL)
  }
  
  # Agrupar solo por Id_viaje
  df_agrupado <- df_viajes %>%
    group_by(Id_viaje) %>%
    summarise(
      Total_contenedores = n(),
      .groups = 'drop'
    )
  
  # Mostrar información del agrupamiento
  cat("Agrupamiento por Id_viaje:\n")
  cat("Total de grupos únicos:", nrow(df_agrupado), "\n")
  cat("Total de contenedores:", sum(df_agrupado$Total_contenedores), "\n")
  
  
  return(df_agrupado)
}

# Función para agrupar por circuito y sumar contenedores levantados.
# Los agrupa por día, y turno.
# df_viajes <- web_historico_completo_llenado_incidencias
agrupar_por_circuito_unico <- function(df_viajes, municipio = NULL) {
  
  # Verificar que el dataframe no esté vacío
  if (nrow(df_viajes) == 0) {
    cat("El dataframe está vacío. No hay datos para agrupar.\n")
    return(NULL)
  }
  
  # Si se especifica municipio, filtrar
  if (!is.null(municipio) && municipio != "") {
    df_viajes <- df_viajes %>% 
      filter(Municipio == municipio)
  }
  
  # Agrupar por Fecha, Circuito e Id_viaje
  df_agrupado_contenedoresalevantar <- df_viajes %>%
    group_by(Fecha, Circuito, Id_viaje) %>%
    summarise(
      Total_contenedores = n(),
      .groups = 'drop'
    )
  
  df_agrupado_contenedoreslevantados <- df_viajes %>%
    filter(Levantado == "S") %>% 
    group_by(Fecha, Circuito, Id_viaje) %>%
    summarise(
      Contenedores_levantados = n(),
      .groups = 'drop'
    )
  
  df_agrupado <- df_agrupado_contenedoresalevantar %>%
    mutate(Circuito = as.character(Circuito),
           Id_viaje = as.integer(Id_viaje)) %>%
    select(Fecha, Circuito, Id_viaje, Total_contenedores) %>%
    inner_join(
      df_agrupado_contenedoreslevantados %>%
        mutate(Circuito = as.character(Circuito),
               Id_viaje = as.integer(Id_viaje)) %>%
        select(Fecha, Circuito, Id_viaje, Contenedores_levantados),
      by = c("Circuito","Id_viaje"),
      suffix = c("_levantar","_levantados")
    ) %>%
    mutate(Fecha = coalesce(Fecha_levantar, Fecha_levantados)) %>%
    select(Fecha, Circuito, Id_viaje, Total_contenedores, Contenedores_levantados)

  
  return(df_agrupado)
}


# df_viajes, es un df con los circuitos separados pero
# con la cantidad de contenedores levantados.
AGREGAR_DURACION_POR_CIRCUITO <- function(df_viajes){
  library(dplyr)
  library(lubridate)
  library(stringr)
  
  rango_cv <- historico_completo_llenado_incidencias %>%
    mutate(
      Circuito = as.character(Circuito) |> str_squish(),
      Id_viaje = suppressWarnings(as.integer(Id_viaje)),
      Fecha_hora_pasaje = ymd_hms(Fecha_hora_pasaje, tz = "America/Montevideo", quiet = TRUE)
    ) %>%
    filter(!is.na(Fecha_hora_pasaje)) %>%
    group_by(Circuito, Id_viaje) %>%
    summarise(
      inicio = min(Fecha_hora_pasaje, na.rm = TRUE),
      fin    = max(Fecha_hora_pasaje, na.rm = TRUE),
      .groups = "drop"
    )
  
  retorno <- df_viajes %>% 
    mutate(
      Circuito = as.character(Circuito) |> str_squish(),
      Id_viaje = suppressWarnings(as.integer(Id_viaje))
    ) %>%
    select(-any_of(c("inicio","fin","duracion_min"))) %>%
    left_join(rango_cv, by = c("Circuito","Id_viaje")) %>%
    mutate(
      duracion_min = case_when(
        is.na(inicio) | is.na(fin)      ~ NA_real_,
        fin < inicio                    ~ NA_real_,  # si cruza medianoche raro o datos malos
        TRUE                            ~ as.numeric(difftime(fin, inicio, units = "mins"))
      )
      # , duracion_min = round(duracion_min, 1) # si querés redondear
    )
  
  return(retorno)
}

ch_unicos <- agrupar_por_circuito_unico(df_viajes, municipio = "CH")
ch_unicos <- AGREGAR_DURACION_POR_CIRCUITO(ch_unicos)

ch_filtrado <- ch_unicos %>%
  mutate(
    circuito_num = as.integer(str_extract(Circuito, "\\d+$"))  # extrae el número al final
  ) %>%
  filter(between(circuito_num, 101, 109)) %>% 
  select(-circuito_num)



### CH ----

resultado_ch <- comparar_cajas_vs_lateral(municipio = "CH")

caja_desmontable_ch <- resultado_ch$caja_desmontable
convencional_ch <- resultado_ch$convencional


agrupado_convencional_ch <- agrupar_por_viaje(convencional_ch)
agrupado_caja_desmontable_ch <- agrupar_por_viaje(cajaDesmontable_ch)

### G  ----

resultado_g <- comparar_cajas_vs_lateral(municipio = "G")

caja_desmontable_g <- resultado_g$caja_desmontable
convencional_g <- resultado_g$convencional

agrupado_convencional_g <- agrupar_por_viaje(convencional_g)
cajaDesmontable_g <- resultado_g$caja_desmontable

agrupado_caja_desmontable_g <- agrupar_por_viaje(cajaDesmontable_g)

promedio_contenedores_convencional <- sum(agrupado_convencional_g$Total_contenedores) / nrow(agrupado_convencional_g)
cat("Promedio de contenedores por Id_viaje (Convencional):", promedio_contenedores_convencional, "\n")

promedio_contenedores_caja_desmontable <- sum(agrupado_caja_desmontable_g$Total_contenedores) / nrow(agrupado_caja_desmontable_g)
cat("Promedio de contenedores por Id_viaje (Caja Desmontable):", promedio_contenedores_caja_desmontable, "\n")



####################

# Calcular el promedio de contenedores por Id_viaje para cada tipo

promedio_contenedores_convencional <- sum(agrupado_convencional_ch$Total_contenedores) / nrow(agrupado_convencional_ch)
cat("Promedio de contenedores por Id_viaje (Convencional):", promedio_contenedores_convencional, "\n")

promedio_contenedores_caja_desmontable <- sum(agrupado_caja_desmontable_ch$Total_contenedores) / nrow(agrupado_caja_desmontable_ch)
cat("Promedio de contenedores por Id_viaje (Caja Desmontable):", promedio_contenedores_caja_desmontable, "\n")



#####
historico_incidencias_limpio <- historico_incidencias %>% 
  filter(gid > 0)

rotos <- historico_incidencias_limpio %>%
  filter(Id_incidencia == 1 | Id_incidencia == 2) %>%
  group_by(Id_viaje) %>%
  summarise(Cantidad_rotos = n(), .groups = 'drop')

agrupado_convencional_ch_conrotos <- agrupado_convencional_ch %>%
  left_join(rotos %>% select(Id_viaje,Cantidad_rotos) %>% distinct(Id_viaje, .keep_all = TRUE), by = "Id_viaje")
# Agregar una columna llamada 'total' que sume Cantidad_rotos con Total_contenedores
agrupado_convencional_ch_conrotos <- agrupado_convencional_ch_conrotos %>%
  mutate(
    Cantidad_rotos = ifelse(is.na(Cantidad_rotos), 0, Cantidad_rotos),
    total = Total_contenedores + Cantidad_rotos
  )











# Mostrar información del nuevo dataframe
cat("Dataframe de circuitos por viaje:\n")
cat("Total de viajes únicos:", nrow(circuitos_por_viaje), "\n")
cat("Promedio de circuitos distintos por viaje:", round(mean(circuitos_por_viaje$Cantidad_circuitos_distintos), 2), "\n")
cat("Máximo de circuitos distintos en un viaje:", max(circuitos_por_viaje$Cantidad_circuitos_distintos), "\n")
cat("Mínimo de circuitos distintos en un viaje:", min(circuitos_por_viaje$Cantidad_circuitos_distintos), "\n\n")

# Mostrar las primeras filas
print("Primeras filas del dataframe circuitos_por_viaje:")
print(head(circuitos_por_viaje))

# Mostrar distribución de cantidad de circuitos
print("Distribución de cantidad de circuitos distintos por viaje:")
print(table(circuitos_por_viaje$Cantidad_circuitos_distintos))








resultado_ch <- comparar_cajas_vs_lateral(municipio = "CH")

caja_desmontable_ch <- resultado_ch$caja_desmontable
convencional_ch <- resultado_ch$convencional



---- #### HORAS EN APP ----

caja_desmontable_ch_tiempos_app <- caja_desmontable_ch %>%
  group_by(Id_viaje) %>%
  summarise(
    min_fecha = min(Fecha_hora_pasaje, na.rm = TRUE),
    max_fecha = max(Fecha_hora_pasaje, na.rm = TRUE),
    diferencia = difftime(max_fecha, min_fecha, units = "mins"),
    cantidad_contenedores = n(),
    tiempo_por_contenedor = round(as.numeric(diferencia) / cantidad_contenedores, 1)
  ) %>%
  ungroup() %>%
  filter(as.numeric(diferencia) <= 480)  # solo viajes con menos de 8h de diferencia

convencional_ch_tiempos <- convencional_ch %>%
  group_by(Id_viaje) %>%
  summarise(
    min_fecha = min(Fecha_hora_pasaje, na.rm = TRUE),
    max_fecha = max(Fecha_hora_pasaje, na.rm = TRUE),
    diferencia = difftime(max_fecha, min_fecha, units = "mins"),
    cantidad_contenedores = n(),
    tiempo_por_contenedor = round(as.numeric(diferencia) / cantidad_contenedores, 0)
  ) %>%
  ungroup() %>%
  filter(as.numeric(diferencia) <= 480)

---- #### HORAS EN APP FIN ----

---- #### HORAS EN VIAJESENUNPERIODO ----

historico_viajes_filtrado <- historico_viajes %>% 
  distinct(Id_viaje, Circuito,Posicion_inicial,Posicion_final,Cantidad_levantada, .keep_all = TRUE)

  

caja_desmontable_ch_tiempos_consultaexp <- historico_viajes_filtrado %>%
  filter(Id_viaje %in% caja_desmontable_ch_tiempos$Id_viaje) %>% 
  select(Id_viaje,Turno,Hora_salida,Hora_llegada) %>% 
  distinct()
caja_desmontable_ch_tiempos_consultaexp <- caja_desmontable_ch_tiempos_consultaexp %>%
  mutate(
    duracion_viaje = as.numeric(difftime(Hora_llegada, Hora_salida, units = "mins"))
  ) %>% 
  filter(duracion_viaje > 0)

caja_desmontable_ch_total_por_viaje <- caja_desmontable_ch_totalcontenedores %>%
  distinct(Id_viaje, Circuito,Posicion_inicial,Posicion_final,Cantidad_levantada, .keep_all = TRUE) %>% 
  group_by(Id_viaje) %>%
  summarise(
    total_levantados = sum(Cantidad_levantada, na.rm = TRUE)
  ) 

caja_desmontable_ch_resumen <- caja_desmontable_ch_tiempos_consultaexp %>%
  left_join(caja_desmontable_ch_total_por_viaje, by = "Id_viaje")

caja_desmontable_ch_resumen <- caja_desmontable_ch_resumen %>%
  mutate(
    minutos_por_contenedor = round(duracion_viaje / total_levantados, 0)
  )

caja_desmontable_ch_promedios <- caja_desmontable_ch_resumen %>%
  summarise(
    promedio_contenedores = mean(total_levantados, na.rm = TRUE),
    promedio_minutos_por_contenedor = mean(minutos_por_contenedor, na.rm = TRUE)
  )

---- #### HORAS EN VIAJESENUNPERIODO fin ----



caja_desmontable_ch_tiempos_SINSENTIDO <- caja_desmontable_ch_tiempos %>%
  filter(tiempo_por_contenedor == 0)

viajes_filtrados <- historico_viajes %>%
  filter(Id_viaje %in% caja_desmontable_ch_tiempos_SINSENTIDO$Id_viaje) %>% 
  filter(Estado == "Cerrado" | Estado == "Finalizado") %>% 
  select(Id_viaje,Turno,Hora_salida,Hora_llegada,Tripulacion) %>% 
  distinct()

# Promedios para cajas desmontables
promedios_caja_desmontable <- caja_desmontable_ch_tiempos %>%
  summarise(
    promedio_contenedores = mean(cantidad_contenedores, na.rm = TRUE),
    promedio_tiempo_por_contenedor = mean(tiempo_por_contenedor, na.rm = TRUE)
  )

# Promedios para convencional
promedios_convencional <- convencional_ch_tiempos %>%
  summarise(
    promedio_contenedores = mean(cantidad_contenedores, na.rm = TRUE),
    promedio_tiempo_por_contenedor = mean(tiempo_por_contenedor, na.rm = TRUE)
  )














# =========================================================================================================

#                                 FUNCIONES PARA OBTENER EL DF DEL DÍA

# =========================================================================================================

# ===================================================================
# Función para obtener el dataframe del día del informe indicado en los parámetros sin los INACTIVOS
# 
# Entrada: Dataframe completo con todos los reportes de todos los días
# y el día, mes y año del reporte a buscar.
# Salida: Dataframe de la fecha sin INACTIVOS
# ===================================================================

funcion_paraMapear_porDia <- function(df_historico_informeDelDia, fecha_buscada){
  data_frame_del_dia <- df_historico_informeDelDia %>%
    filter(Dia_informe == fecha_buscada) %>%
  return(data_frame_del_dia)
}

#prueba <- funcion_paraMapear_porDia(historico_informe_deldia,as.Date("2024-11-21"))

# =========================================================================================================

#                                  FUNCIONES PARA EL RANKING

# =========================================================================================================

# ===================================================================
# Funcion para obtener el RANKING COMPLETO por CIRCUITO
# cod_recorrido, circuito corto, turno planificado, contenedores activos, inactivos y su %, total de contenedores
# Frecuencia, periodo, ranking de acumulación y ranking de UNA.
#
# Entrada: Dataframe del dia del informe con inactivos y dataframe del dia sin inactivos
# Salida: Dataframe completo del ranking (con la info de arriba)
# ===================================================================
 ranking <- prueba
funcion_ranking_UNA <- function(df_paraMapear){
  
  ranking <- df_paraMapear
  fecha_paraMapear <- unique(ranking$Dia_informe)
  
  
  ranking <- ranking %>% 
    group_by(Circuito) %>% 
    summarise(Acumulacion_promedio = mean(Acumulacion),
              Activos = n())
  
  inactivos_del_dia <- historico_estado_diario_soloInactivos %>% 
    filter(dia_reporte == fecha_paraMapear) %>% 
    group_by(circuito_corto) %>% 
    summarise(Inactivos = n()) %>% 
    rename(Circuito = circuito_corto)
  
  ranking$Acumulacion_promedio <- round(ranking$Acumulacion_promedio,2)
  
  ranking_actualizado <- ranking %>%
    left_join(inactivos_del_dia, by = "Circuito") %>%
    mutate(Inactivos = ifelse(is.na(Inactivos), 0, Inactivos))
  
  ranking_actualizado <- ranking_actualizado %>% 
    mutate(Total = Activos + Inactivos)
  
  ranking_actualizado <- ranking_actualizado %>% 
    mutate(Porcentaje_Activos = Activos / Total, #* 100,
           Porcentaje_Inactivos = Inactivos / Total #* 100)
    )
  
  # # Calculo el ranking
  # ranking_UNA <- dataframe_ranking_UNA_pordia(df_sin_inactivos)
  # 
  # # Calculo el total de activos, inactivos, total, % de activos e inactivos
  # total <- df_con_inactivos %>%
  #   group_by(circuito_corto) %>%
  #   summarise(
  #     Activos = sum(activo_inactivo == "ACTIVO"),
  #     Inactivos = sum(activo_inactivo == "INACTIVO"),
  #     Total = n(),
  #     Porcentaje_Activos = Activos / Total, #* 100,
  #     Porcentaje_Inactivos = Inactivos / Total #* 100
  #   )
  # 
  # # Le agrego el turno y la frecuencia.
  # df2 <-df_con_inactivos[, c("cod_recorrido","circuito_corto","periodo", "turno_planificado" ,"frecuencia")]
  # df2 <- df2 <- df2[!duplicated(df2$circuito_corto), ]
  # df_merged <- merge(total, df2, by = "circuito_corto", all.x = TRUE)
  # 
  # df_merged <- merge(df_merged, ranking_UNA, by = "circuito_corto", all.x = TRUE)
  # 
  # 
  return (df_merged)
}

# ===================================================================
# Funcion para obtener los puntos para MAPEAR EL MAPA UNA DE POLIGONOS / CIRCUITOS
#
#
# Entrada: Dataframe del ranking completo del dia
# Salida: Dataframe con el dato de circuito corto y el ranking una por circuito.
# ===================================================================

df_mapa_circuitos_UNA <- function(ranking_completo){
  mapa_circuitos_UNA <- ranking_completo %>%
    select(circuito_corto, una) %>%   # Seleccionar las columnas necesarias
    mutate(RANGO_UNAP = case_when(
      una <= 100 ~ 1,
      una > 100 & una <= 130 ~ 2,
      una > 130 & una <= 150 ~ 3,
      una > 150 & una <= 170 ~ 4,
      una > 171 ~ 5
    ))
  
  mapa_circuitos_UNA <- mapa_circuitos_UNA %>%
    select(circuito_corto,RANGO_UNAP)
  return(mapa_circuitos_UNA)
}

# ===================================================================
# Funcion para obtener los puntos para MAPEAR EL MAPA UNA DE PUNTOS
#
#
# Entrada: Dataframe del informe del día.
# Salida: Dataframe con el dato de rango unap punto a punto
# ===================================================================

df_mapa_puntos_UNA <- function(dataFrameDelDia_sin_inactivos){
  
  mapa_puntos_UNA <- dataFrameDelDia_sin_inactivos %>%
    # Selecciono los campos que me importan
    select(nombre_contenedor,frecuencia,dia_normalizado) %>%
    
    # Calculo UNA por punto
    mutate(UNA = frecuencia * dia_normalizado/7*100) %>%
    
    # Les calculo el rango 
    mutate(rango_UNAP = case_when(
      UNA <= 100 ~ 1,
      UNA > 100 & UNA <= 130 ~ 2,
      UNA > 130 & UNA <= 150 ~ 3,
      UNA > 150 & UNA <= 170 ~ 4,
      UNA > 171 ~ 5
    ))
  
  mapa_puntos_UNA <- mapa_puntos_UNA %>%
    select(nombre_contenedor,rango_UNAP)
  
  return(mapa_puntos_UNA)
  
}

# ===================================================================
# Función para obtener los contenedores que se agregaron en comparacion con el dia anterior
# Pueden ser nuevos, o que dejaron de estar inactivos. 
#
# AQUELLOS QUE TENGAN MÁS DE 1 DÍA DE ACUMULACIÓN, NO SE MUESTRAN HASTA QUE SE LEVANTE X 1ERA VEZ
#
# Entrada: Dataframe del dia del informe y dataframe del dia anterior al informe
# Salida: Dataframe con los contenedores agregados
# ===================================================================

funcion_contenedores_agregados_al_dia_del_informe <- function(hoy,ayer){
  # Usar anti_join para encontrar contenedores que estan hoy y no estaban ayer
  agregados <- anti_join(hoy, ayer, by = "nombre_contenedor")
  return (agregados)
}

# ===================================================================
# Función para obtener los contenedores que se quitaron en comparacion con el dia anterior
# Pueden ser que se sacaron, o que dejaron de estar activos. 
#
# Entrada: Dataframe del dia del informe y dataframe del dia anterior al informe
# Salida: Dataframe con los contenedores quitados
# ===================================================================

funcion_contenedores_quitados_al_dia_del_informe <- function(hoy,ayer){
  # Usar anti_join para encontrar contenedores que estaban ayer y no hoy
  quitados <- anti_join(ayer, hoy, by = "nombre_contenedor")
  return (quitados)
}



informe_diario_final <- function(dataFrameDelDia,contenedores_nuevos){
  
  # filtro aquellos que tengan valor 1 para mostrarlos (se regulariza con la levantada)
  contenedores_nuevos <- contenedores_nuevos %>%
    filter(dia_normalizado <= 3)
  
  resultado <- bind_rows(dataFrameDelDia, contenedores_nuevos)
  
  return(resultado)
  
  
}

# ===================================================================
# Función para obtener los contenedores repetidos.
# 
# Entrada: Dataframe completo con todos los reportes de todos los días
# y el día, mes y año del reporte a buscar.
# Salida: Dataframe con los repetidos, solo con el nombre de contenedor y el 1
# ===================================================================

funcion_df_repetidos <- function(data_frame_completo, dia,mes,anio){
  
  # genero el día y el día anterior para filtrar el dia
  dia_informe <- as.Date(paste0(anio,"-",mes,"-",dia))
  dia_anterior_informe <- dia_informe - 1
  
  # Calculo los que se levantaron el día del informe
  repetidos_levantados_dia_reporte <- data_frame_completo %>%
    filter(dia_reporte == dia_informe & dia_normalizado == 1) %>%
    filter(periodo != 1)
  
  # Calculo los que se levantaron el día anterio del informe
  repetidos_levantados_dia_anterior_reporte <- data_frame_completo %>%
    filter(dia_reporte == dia_anterior_informe & dia_normalizado == 1) %>%
    filter(periodo != 1)
  
  # Uno los dataframe en donde se repita el mismo contenedor y luego solo muestro
  repetidos <- repetidos_levantados_dia_reporte %>%
    inner_join(repetidos_levantados_dia_anterior_reporte, by = "nombre_contenedor")  %>%
    # Filtro y muestro solo el nombre y el día para mapearlo.
    select(nombre_contenedor,dia_normalizado.y)
  
  # Cambio el nombre
  repetidos <- repetidos %>%
    rename(CIR_POS = nombre_contenedor) %>%
    rename(ATRASO = dia_normalizado.y)
  
  return(repetidos)
}

# ===================================================================
# Función para obtener los contenedores para mapear.
# Transforma los números mayores a 7 para mapear. 
#
# Entrada: Dataframe del día
# Salida: Dataframe con los los puntos para mapear
# ===================================================================

dataframe_mapaDiario <- function(dataFrameDelDia){
  mapadiario <- dataFrameDelDia %>%
    mutate(dia_normalizado = ifelse(dia_normalizado >= 6, 6, dia_normalizado)) %>%
    select(nombre_contenedor,dia_normalizado) %>%
    rename(CIR_POS = nombre_contenedor) %>%
    rename(ATRASO = dia_normalizado)
    
  return(mapadiario)
}

# ===================================================================
# Función para contar y agrupar los días de atrado en total
# Agrupa los que tienen 1 día, 2 días, y así sucesivamente.
#
# Entrada: Dataframe del día
# Salida: Dataframe con los días agrupados y cuantos contenedores tiene esa cantidad de días de atraso.
# ===================================================================

dataframe_atrasos_contenedores_agrupados_por_dia <- function (dataFreameDelDia){
  dataFrameRetorno <- dataFreameDelDia %>%
    group_by(dia_normalizado)  %>%
    summarise(total = n())
  return(dataFrameRetorno)
}

# ===================================================================
# Función para contar y agrupar los días segun informe diario
# Hasta 2 días
# 2 a 3 dias
# 3 y 4 días
# 4 y 5 días
# Más de 5
#
# Entrada: Dataframe del día
# Salida: Dataframe con los días agrupados como se informo arriba.
# ===================================================================

dataframe_agrupacion_atrasoContenedores <- function(dataFrameDelDia){
  
  # Definir los puntos de corte y las etiquetas
  cortes <- c(-Inf, 3, 4, 5, 6, Inf)
  etiquetas <- c("Hasta 2", "entre 2-3", "entre 3-4", "entre 4-5", "más de 5")
  
  # Defino la constante contenedores CAP
  contenedores_cap <- 1069
  categoria_modificada <- "Hasta 2"  # Categoría a la que se le sumará la constante
  
  # Crear la variable categórica usando cut
  tablita <- dataFrameDelDia %>%
    mutate(Dias = cut(dia_normalizado, breaks = cortes, labels = etiquetas, right = FALSE))
  
  # Contar el número de ocurrencias en cada grupo
  resultados <- tablita %>%
    group_by(Dias) %>%
    summarize(conteo = n()) %>%
    ungroup()
  
  # Modificar el conteo de la categoría deseada
  resultados_modificados <- resultados %>%
    mutate(conteo = ifelse(Dias == categoria_modificada, conteo + contenedores_cap, conteo))
  
  # Recalcular el porcentaje basado en el conteo modificado
  resultados_modificados <- resultados_modificados %>%
    mutate(Porcentaje = (conteo / sum(conteo)) * 100) %>%
    arrange(Dias) %>%
    mutate(Porcentaje = round(Porcentaje, 2))
  
  # Agregar la fila de totales
  total <- data.frame(
    Dias = "Total",
    conteo = sum(resultados_modificados$conteo),
    Porcentaje = sum(resultados_modificados$Porcentaje)
  )
  
  # Combinar los resultados con el total
  resultados_modificados <- bind_rows(resultados_modificados, total)
  
  # Cambio nombres titulo
  resultados_modificados <- resultados_modificados %>%
    rename("N° Contenedores IM - CAP" = conteo)
  
  return(resultados_modificados)
}


# ===================================================================
# Función para obtener el ranking de los días de acumulación
#
# Entrada: Dataframe del día
# Salida: Dataframe con los rankings y el circuito corto
# ===================================================================

# Hago el ranking de contenedores activos, para un dia determinado.
dataframe_ranking_pordia <- function(dataFrameDelDia){
  ranking <- dataFrameDelDia %>%
    group_by(circuito_corto) %>%
    summarise(ranking = mean(dia_normalizado)) %>%
    summarise (circuito_corto,ranking_diario = round(ranking,2))
  return(ranking)
}

# ===================================================================
# Función para obtener el ranking de los días de acumulación POR UNA
#
# Entrada: Dataframe del día
# Salida: Dataframe con los rankings y el circuito corto + UNA y turno
# ===================================================================

# Hago el ranking de contenedores activos, para un dia determinado.
dataframe_ranking_UNA_pordia <- function(dataFrameDelDia){
  
  # calculo el ranking del circuito
  ranking_x_circuitos <- dataFrameDelDia %>%
    group_by(circuito_corto) %>%
    summarise(ranking = round(mean(dia_normalizado),2)) %>% 
    ungroup() 
  
  # Agrego al dataframe el ranking x circuito a cada punto
  # Luego le calculo el unap
  ranking_x_circuitos_en_dataframebase <- dataFrameDelDia %>%
    left_join(ranking_x_circuitos, by = "circuito_corto") %>%
    mutate(unap = ranking*frecuencia/7*100)
  
  # Agrupo los rankings
  ranking_x_circuitos_con_una <- ranking_x_circuitos_en_dataframebase %>%
    group_by(circuito_corto) %>%
    summarise(
      ranking = round(mean(ranking),2),
      una = round(mean(unap),2))
  
  # Le agrego los turnos
  rankingretorno <- ranking_x_circuitos_con_una %>%
    left_join(ranking_x_circuitos_en_dataframebase %>% 
                select(circuito_corto), by = "circuito_corto") %>%
    distinct(circuito_corto, .keep_all = TRUE)
  
  return(rankingretorno)
}

# ===================================================================
# Función para contar contenedores activos, inactivos, total y porcentaje
# 
#
# Entrada: Dataframe del día
# Salida: Dataframe separada por circuito, contando activos, inactivos y %.
# ===================================================================

dataframe_total_contenedores_activos_inactivos_por_circuito <-  function(dataFrameDelDia) {
  total <- dataFrameDelDia %>%
    group_by(cod_recorrido,circuito_corto) %>%
    summarise(
      Activos = sum(activo_inactivo == "ACTIVO"),
      Inactivos = sum(activo_inactivo == "INACTIVO"),
      Total = n(),
      Porcentaje_Activos = Activos / Total, #* 100,
      Porcentaje_Inactivos = Inactivos / Total #* 100
    )
  
  # Le agrego el turno y la frecuencia.
  df2 <-dataFrameDelDia[, c("circuito_corto","periodo", "turno_planificado" ,"frecuencia")]
  df2 <- df2 <- df2[!duplicated(df2$circuito_corto), ]
  df_merged <- merge(total, df2, by = "circuito_corto", all.x = TRUE)
  
  return (df_merged)
}

# ===================================================================
# Función para calcular el promedio del turno INDICADO POR PARAMETRO
# 
#
# Entrada: Dataframe del día, turno "M", "V", "N".
# Salida: Valor calculado.
# ===================================================================

promedio_acumulacion_por_turno <- function(ranking_UNA_dia,turno){
  total <- ranking_UNA_dia %>%
    filter(nombre_turno_levantado_abreviatura == turno)
  
  total <- round(mean(total$ranking),2)
  
  return(total)
}
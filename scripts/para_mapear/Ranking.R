# nolint start: line_length_linter, object_name_linter

# 
# asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)

## Funcion para calcular el ranking dado un día.
funcion_calcular_ranking_deldia <- function(fecha_consulta, df_informedeldia){
  
  ### Calcular el ranking total
  
  df_informedeldia_activos <- df_informedeldia %>% 
    filter(is.na(Estado))
  
  ## Los agrupo por circuito y le calculo
  
  # Obtengo los circuitos planificados para obtener la frecuencia
  circuitos_plan <- datos_circuitos
  
  # Calculo el ranking y lo junto con la frecuencia
  ranking <- df_informedeldia_activos %>% 
    group_by(Circuito_corto) %>% 
    summarise(Ranking = round(mean(Acumulacion), digits = 2)) %>%
    left_join(
      circuitos_plan %>% 
        select(circuito_corto, Frecuencia, turno_planificado) %>%
        distinct(),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      UNA = round(((Ranking * Frecuencia) / 7) * 100, 2)
    )
  
  # Calcular otros dataframes de interés

  
  
  # Ranking por turno
  ranking_por_turno <- df_informedeldia_activos %>%
    left_join(
      circuitos_plan %>% select(circuito_corto, turno_planificado),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    group_by(turno_planificado) %>%
    summarise(Ranking_turno = round(mean(Acumulacion, na.rm = TRUE), digits = 2))
  
  # Datos detallados por circuito
  detalles_circuito <- df_informedeldia_activos %>%
    group_by(Circuito_corto) %>%
    summarise(
      Acumulacion_promedio = mean(Acumulacion, na.rm = TRUE),
      Acumulacion_max = max(Acumulacion, na.rm = TRUE),
      Registros = n()
    )
  
  # Calculo de mapa polígono según escala definida para UNA
  mapa_poligono <- ranking %>%
    mutate(
      valor_escala = case_when(
        is.na(UNA) ~ NA_real_,
        UNA <= 100 ~ 1,
        UNA > 100 & UNA <= 130 ~ 2,
        UNA > 130 & UNA <= 150 ~ 3,
        UNA > 150 & UNA <= 170 ~ 4,
        UNA > 170 ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    select(Circuito_corto, UNA, valor_escala)

  # Crear un nuevo dataframe que agregue la columna Frecuencia a df_informedeldia_activos según el circuito
  df_informedeldia_activos_con_frecuencia <- df_informedeldia_activos %>%
    left_join(
      circuitos_plan %>% select(circuito_corto, Frecuencia),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      UNA = round(((Acumulacion * Frecuencia) / 7) * 100, 2)
    )
  
  # Retornar una lista con múltiples dataframes
  return(list(
    ranking_principal = ranking,
    ranking_turnos = ranking_por_turno,
    detalles = detalles_circuito,
    mapa_poligono = mapa_poligono,
    datos_activos = df_informedeldia_activos_con_frecuencia
  ))
}


fecha_consulta <- as.Date("2025-07-09")
df_informedeldia <- historico_estado_diario %>% 
  filter(Fecha == fecha_consulta)

asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)
ranking_por_circuito <- asd$ranking_principal
mapa_poligono <- asd$mapa_poligono
total <- asd$datos_activos

  # Obtener las frecuencias únicas presentes en el dataframe total y crear una tabla con ellas
  tabla_frecuencias <- total %>%
    distinct(Frecuencia) %>%
    arrange(Frecuencia) %>%
    mutate(
      Frec_texto = case_when(
        Frecuencia == 3 ~ "3 veces por semana",
        Frecuencia == 3.5 ~ "48 horas",
        Frecuencia == 2.33 ~ "72 horas",
        Frecuencia == 7 ~ "Diaria",
        TRUE ~ as.character(Frecuencia)
      ),
      Periodo = round(7 / Frecuencia, 2),
      UNA_minimo_dia = round(1 / Periodo, 4) * 100,
      UNA_minimo_turno = round((round(1 / Periodo, 4) * 100) / 3, 2),
      # El ciclo es la cantidad de semanas que tarda en repetirse el patrón de acumulación
      ciclo_semanal = case_when(
        Frecuencia == 3.5 ~ 2,    # 14 días (2 semanas) para repetir el patrón
        Frecuencia == 2.33 ~ 3,   # 21 días (3 semanas) para repetir el patrón
        TRUE ~ 1                  # Para otras frecuencias, se asume 1 semana
      ),
      dias_totales_ciclo = ciclo_semanal * 7,
      # Cálculo de días con 3, 2 y 1 días de acumulación en el ciclo completo
      dias_3_acum = case_when(
        Frecuencia == 3.5 ~ 0,    # Nunca hay 3 días de acumulación en el ciclo de 14 días
        Frecuencia == 3 ~ 1,      # Para frecuencia 3: 1 día con 3 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 3 de acumulación
        Frecuencia == 7 ~ 0,      # Diario, nunca se acumulan 3 días
        TRUE ~ 0
      ),
      dias_2_acum = case_when(
        Frecuencia == 3.5 ~ 7,    # En 14 días, hay 7 días con 2 de acumulación
        Frecuencia == 3 ~ 2,      # Para frecuencia 3: 2 días con 2 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 2 de acumulación
        Frecuencia == 7 ~ 0,      # Diario, nunca se acumulan 2 días
        TRUE ~ pmax(0, dias_totales_ciclo - round(Frecuencia * ciclo_semanal)) # Aproximación para otras frecuencias
      ),
      dias_1_acum = case_when(
        Frecuencia == 3.5 ~ 7,    # En 14 días, hay 7 días con 1 de acumulación
        Frecuencia == 3 ~ 3,      # Para frecuencia 3: 3 días con 1 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 1 de acumulación
        Frecuencia == 7 ~ dias_totales_ciclo, # Diario, todos los días con 1 de acumulación
        TRUE ~ round(Frecuencia * ciclo_semanal) # Aproximación para otras frecuencias
      ),
      # Cálculo de UNA óptima según la fórmula solicitada
      Una_optima = round(
        (
          (dias_3_acum * UNA_minimo_dia*3) +
          (dias_2_acum * UNA_minimo_dia*2) +
          (dias_1_acum * UNA_minimo_dia*1)  
        ) / dias_totales_ciclo,
        2
      )
    )


  # Calcular el valor óptimo global (igual para todos)
  valor_optimo_global <- mean(tabla_frecuencias$Una_optima, na.rm = TRUE)

  # Agregar columna con el valor óptimo global a la tabla
  tabla_frecuencias <- tabla_frecuencias %>%
    mutate(Una_optima_global = round(valor_optimo_global, 2))

# Agrupar los datos activos (total) por la columna Frecuencia y cuantificar la cantidad de registros por cada frecuencia
resumen_frecuencia <- total %>%
  group_by(Frecuencia) %>%
  summarise(Cantidad = n()) %>%
  arrange(Frecuencia)



####### DF DEL INFORME DIARIO + LA FRECUENCIA + EL PEERÍODO ######


  # Crear una copia del dataframe historico_informe_diario
  historico_informe_diario_con_frecuencia <- historico_estado_diario

  # Anexar la información de Frecuencia y Periodo según el "cod_recorrido" (antes "Circuito")
  historico_informe_diario_con_frecuencia <- historico_informe_diario_con_frecuencia %>%
    left_join(
      datos_circuitos %>% 
        select(cod_recorrido, Frecuencia, Periodo),
      by = c("Circuito" = "cod_recorrido")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      Acumulacion = as.numeric(Acumulacion),
      UNA = ((Acumulacion * Frecuencia) / 7) * 100
    )

  ####### DF PARA OBTENER LA CANTIDAD DE CONTENEDORES QUE HAY SEGÚN SU FRECUENCIA ######
  
      # Crear un nuevo dataframe que agrupe por día y por Frecuencia, contando el total de filas para cada combinación
  df_agrupado_dia_frecuencia <- historico_informe_diario_con_frecuencia %>%
    filter(is.na(Estado)) %>% 
    group_by(Fecha, Frecuencia,Periodo) %>%
    summarise(Total = n()) %>%
    ungroup()
  
#######################################################################################################
  
  
  # Crear un nuevo dataframe que agrupe por día y por Frecuencia, TENIENDO la cantidad de contenedores 
  # con equis cantidad de días de acumulación, separado por día y frecuencia.
  df_agrupado_dia_acumulacion <- historico_informe_diario_con_frecuencia %>%
    filter(is.na(Estado)) %>% 
    group_by(Fecha, Frecuencia,Periodo, Acumulacion) %>%
    summarise(Total = n()) %>%
    ungroup()
  
  # Agrego la una según la acumulación y la frecuencia
  df_agrupado_dia_acumulacion <- df_agrupado_dia_acumulacion %>% 
    mutate(una_por_acumulacion  = Acumulacion/Periodo) %>% 
    mutate(una_total_por_Acumulacion = una_por_acumulacion * Total)

  # Crear un nuevo dataframe que, para cada fecha y frecuencia, sume la cantidad de filas y el total de una_total_por_acumulacion,
  # y además agregue la información de Total de df_agrupado_dia_frecuencia cuando coinciden Fecha y Frecuencia.
  # Ahora se agrega la columna UNA_Por_Frecuencia que es una_total_por_Acumulacion / Total
  df_una_total_por_fecha_frecuencia <- df_agrupado_dia_acumulacion %>%
    group_by(Fecha, Frecuencia) %>%
    summarise(
      total_una_por_fecha_frecuencia = sum(una_total_por_Acumulacion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df_agrupado_dia_frecuencia %>% select(Fecha, Frecuencia, Periodo, Total),
      by = c("Fecha", "Frecuencia")
    ) %>%
    mutate(
      UNA_Por_Frecuencia = ifelse(Total > 0, total_una_por_fecha_frecuencia / Total, NA_real_),
      UNA_Por_Frecuencia_Normalizado = UNA_Por_Frecuencia * 100
    ) %>%
    left_join(
      tabla_frecuencias %>% select(Frecuencia, Una_optima, Una_optima_global),
      by = "Frecuencia"
    )
  
  

  # Filtrar por cada tipo de frecuencia y crear un dataframe para cada una
  df_una_frecuencia_3 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 3)
  df_una_frecuencia_3_5 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 3.5)
  df_una_frecuencia_2_33 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 2.33)
  df_una_frecuencia_7 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 7)

  # Nuevo dataframe que agrupa el total por día (independientemente de la frecuencia)
  # Ahora se anexa a cada fila el valor de valor_optimo_global
  df_una_total_por_fecha <- df_agrupado_dia_acumulacion %>%
    group_by(Fecha) %>%
    summarise(
      total_una_por_fecha = sum(una_total_por_Acumulacion, na.rm = TRUE),
      total_contenedores = sum(Total, na.rm = TRUE)
    ) %>%
    mutate(
      UNA_Por_Dia = ifelse(total_contenedores > 0, total_una_por_fecha / total_contenedores, NA_real_),
      UNA_Por_Dia_Normalizado = UNA_Por_Dia * 100
    ) %>%
    mutate(
      valor_optimo_global = unique(tabla_frecuencias$Una_optima_global)
    )

  # Función para graficar Fecha vs UNA_Por_Dia_Normalizado y valor_optimo_global
  graficar_UNA_por_dia <- function(df, titulo = "UNA por Día en el tiempo") {
    library(ggplot2)
    ggplot(df, aes(x = Fecha)) +
      geom_line(aes(y = UNA_Por_Dia_Normalizado), color = "steelblue", size = 1) +
      geom_point(aes(y = UNA_Por_Dia_Normalizado), color = "darkred", size = 2) +
      geom_line(aes(y = valor_optimo_global), color = "orange", linetype = "dotted", size = 1) +
      labs(
        title = titulo,
        x = "Fecha",
        y = "UNA por Día Normalizado"
      ) +
      theme_minimal() +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
      theme(legend.position = "none")
  }
  # Ejemplo de uso:
  # graficar_UNA_por_dia(df_una_total_por_fecha, "UNA por Día en el tiempo")

# Función para graficar Fecha vs UNA_Por_Frecuencia, Una_optima y Una_optima_global dado un dataframe
graficar_UNA_por_frecuencia <- function(df, titulo = "UNA por Frecuencia en el tiempo") {
  library(ggplot2)
  ggplot(df, aes(x = Fecha)) +
    geom_line(aes(y = UNA_Por_Frecuencia_Normalizado), color = "steelblue", size = 1) +
    geom_point(aes(y = UNA_Por_Frecuencia_Normalizado), color = "darkred", size = 2) +
    geom_line(aes(y = Una_optima), color = "forestgreen", linetype = "dashed", size = 1) +
    geom_line(aes(y = Una_optima_global), color = "orange", linetype = "dotted", size = 1) +
    labs(
      title = titulo,
      x = "Fecha",
      y = "UNA por Frecuencia"
    ) +
    theme_minimal() +
    scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
    theme(legend.position = "none")
}

# Ejemplo de uso:
# graficar_UNA_por_frecuencia(df_una_frecuencia_3, "UNA por Frecuencia 3 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_3_5, "UNA por Frecuencia 3.5 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_2_33, "UNA por Frecuencia 2.33 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_7, "UNA por Frecuencia 7 en el tiempo")


# Ordenar el ranking por UNA y actualizar Excel
ranking_ordenado <- ranking_por_circuito %>%
  arrange(desc(UNA)) %>%
  select(Circuito_corto, Ranking, UNA, turno_planificado)
# 
# # Definir las rutas de los archivos Excel
# excel_path_load <- file.path("scripts", "para_mapear", "Ranking_base.xlsx")
# excel_path_save <- file.path("scripts", "para_mapear", "Ranking_base2.xlsx")
# 
# # Cargar el archivo Excel existente
# wb <- loadWorkbook(excel_path_load)
# 
# # Escribir el ranking ordenado en la hoja 1 a partir de la celda E18
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(Circuito_corto, Ranking),
#   startCol = 5,  # Columna E
#   startRow = 18,
#   colNames = FALSE  # No escribir nombres de columnas
# )
# 
# # Escribir UNA en la columna G
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(UNA),
#   startCol = 8,  # Columna G
#   startRow = 18,
#   colNames = FALSE
# )
# 
# # Escribir turno en la columna H
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(turno_planificado),
#   startCol = 9,  # Columna H
#   startRow = 18,
#   colNames = FALSE
# )
# 
# # Guardar el archivo Excel con el nuevo nombre
# saveWorkbook(wb, excel_path_save, overwrite = TRUE)
# 
# # Mensaje de confirmación
# cat("Ranking actualizado exitosamente en", excel_path_save, "\n")
# 
# # nolint end
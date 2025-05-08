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
  
  # Retornar una lista con múltiples dataframes
  return(list(
    ranking_principal = ranking,
    ranking_turnos = ranking_por_turno,
    detalles = detalles_circuito,
    mapa_poligono = mapa_poligono,
    datos_activos = df_informedeldia_activos
  ))
}


fecha_consulta <- as.Date("2025-04-10")
df_informedeldia <- historico_estado_diario %>% 
  filter(Fecha == fecha_consulta)

asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)
rankgin <- asd$ranking_principal
MAPAP <- asd$mapa_poligono


# nolint end
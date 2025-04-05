fecha_consulta <- as.Date("2025-04-04")
df_informedeldia <- estado_diario_global %>% 
  filter(Fecha == fecha_consulta)

funcion_calcular_ranking_deldia <- function(fecha_consulta, df_informedeldia){
  
  ### Calcular el ranking total
  
  df_informedeldia_activos <- df_informedeldia %>% 
    filter(is.na(Estado))
  
  ## Los agrupo por circuito y le calculo
  
  # Obtengo los circuitos planificados para obtener la frecuencia
  circuitos_plan <- turnos_planificados_por_circuito()
  
  # Calculo el ranking y lo junto con la frecuencia
  ranking <- df_informedeldia_activos %>% 
    group_by(Circuito_corto) %>% 
    summarise(Ranking = round(mean(Acumulacion), digits = 2)) %>%
    left_join(
      circuitos_plan %>% 
        select(circuito_corto, Frecuencia) %>%
        distinct(),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      UNA = round(((Ranking * Frecuencia) / 7) * 100, 2)
    )
  
  return(ranking)
}
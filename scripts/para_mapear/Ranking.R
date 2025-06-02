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

source("scripts/para_mapear/circuitos_planificados.R")



inf_estado_diario <- historico_estado_diario


fecha_consulta <- as.Date("2025-05-23")
df_informedeldia <- inf_estado_diario %>% 
  filter(Fecha == fecha_consulta)

asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)
rankingg <- asd$ranking_principal
MAPAP <- asd$mapa_poligono

# Ordenar el ranking por UNA y actualizar Excel
ranking_ordenado <- rankingg %>%
  arrange(desc(UNA)) %>%
  select(Circuito_corto, Ranking, UNA, turno_planificado)

# Definir las rutas de los archivos Excel
excel_path_load <- file.path("scripts", "para_mapear", "Ranking_base.xlsx")
excel_path_save <- file.path("scripts", "para_mapear", "Ranking_base2.xlsx")

# Cargar el archivo Excel existente
wb <- loadWorkbook(excel_path_load)

# Escribir el ranking ordenado en la hoja 1 a partir de la celda E18
writeData(
  wb,
  sheet = 1,
  x = ranking_ordenado %>% select(Circuito_corto, Ranking),
  startCol = 5,  # Columna E
  startRow = 18,
  colNames = FALSE  # No escribir nombres de columnas
)

# Escribir UNA en la columna G
writeData(
  wb,
  sheet = 1,
  x = ranking_ordenado %>% select(UNA),
  startCol = 8,  # Columna G
  startRow = 18,
  colNames = FALSE
)

# Escribir turno en la columna H
writeData(
  wb,
  sheet = 1,
  x = ranking_ordenado %>% select(turno_planificado),
  startCol = 9,  # Columna H
  startRow = 18,
  colNames = FALSE
)

# Guardar el archivo Excel con el nuevo nombre
saveWorkbook(wb, excel_path_save, overwrite = TRUE)

# Mensaje de confirmación
cat("Ranking actualizado exitosamente en", excel_path_save, "\n")

# nolint end
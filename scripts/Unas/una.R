library(dplyr)

  
source("scripts/para_mapear/circuitos_planificados.R")
funcion_calcular_ranking_deldia_total <- function(df_informedeldia){
  
  ### Calcular el ranking total
  
  df_informedeldia_activos <- df_informedeldia
  
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


llenado <- readRDS("scripts/db/10484_llenado/historico_llenado.rds")
df_informedeldia <- readRDS("scripts/estado_diario/historico_estado_diario.rds")

df_informe_desfasado <- df_informedeldia
df_informe_desfasado$Fecha <- as.Date(df_informe_desfasado$Fecha) + 1

df_informe_desfasado <- df_informe_desfasado %>% 
  filter(Acumulacion < 9)


paramapear_con_frecuencia <- funcion_calcular_ranking_deldia_total(df_informe_desfasado)
paramapear_con_frecuencia <- paramapear_con_frecuencia$datos_activos

anexar_info <- function(paramapear_con_frecuencia, llenado) {
  stopifnot(all(c("gid","Fecha") %in% names(paramapear_con_frecuencia)),
            all(c("gid","Fecha","Incidencia","Porcentaje_llenado","Condicion") %in% names(llenado)))
  library(dplyr)
  
  x <- paramapear_con_frecuencia
  y <- llenado %>%
    select(gid, Fecha,Levantado,Fecha_hora_pasaje, Incidencia, Porcentaje_llenado, Condicion)
  
  # Alineo tipos de fecha
  if (!inherits(x$Fecha, "Date")) x <- mutate(x, Fecha = as.Date(Fecha))
  if (!inherits(y$Fecha, "Date")) y <- mutate(y, Fecha = as.Date(Fecha))
  
  left_join(x, y, by = c("gid", "Fecha"))
}

informe_completo <- anexar_info(paramapear_con_frecuencia, llenado)

df_filtrado_BA_RL <- informe_completo %>%
  filter(grepl("Basura Afuera|Requiere Limpieza", Condicion))

df_sumado_BA_RL <- df_filtrado_BA_RL %>%
  group_by(Fecha, Municipio,Frecuencia ,UNA) %>%
  summarise(total_contenedores_BA_RL = n(), .groups = "drop")

df_sumado_completo <- informe_completo %>%
  group_by(Fecha, Municipio,Frecuencia , UNA) %>%
  summarise(total_contenedores_con_misma_UNA = n(), .groups = "drop")

# df_final <- df_sumado_completo %>%
#   left_join(df_sumado_BA_RL, by = c("Fecha","Municipio","UNA")) %>%
#   select(Fecha, Municipio, UNA,
#          total_contenedores_con_misma_UNA,
#          total_contenedores_BA_RL) %>% 
  

df_final <- df_sumado_completo %>%
  left_join(df_sumado_BA_RL, by = c("Fecha","Municipio","UNA")) %>%
  mutate(
    total_contenedores_BA_RL = coalesce(total_contenedores_BA_RL, 0),
    porc_BA_RL = round(total_contenedores_BA_RL / total_contenedores_con_misma_UNA * 100,2)
  ) %>%
  select(Fecha, Municipio, UNA,
         total_contenedores_con_misma_UNA,
         total_contenedores_BA_RL,
         porc_BA_RL) %>% 
  filter(Fecha < Sys.Date())




#### SOLO LEVANTADOS

informe_completo_soloLevantados <- informe_completo %>% 
  filter(Levantado == "S")

df_filtrado_BA_RL_sololevantados <- informe_completo_soloLevantados %>%
  filter(grepl("Basura Afuera|Requiere Limpieza", Condicion))

df_sumado_BA_RL_sololevantados <- df_filtrado_BA_RL %>%
  group_by(Fecha, Municipio,Frecuencia , UNA) %>%
  summarise(total_contenedores_BA_RL = n(), .groups = "drop")

df_sumado_completo_sololevantados <- informe_completo_soloLevantados %>%
  group_by(Fecha, Municipio,Frecuencia , UNA) %>%
  summarise(total_contenedores_con_misma_UNA = n(), .groups = "drop")

df_final_sololevantados <- df_sumado_completo_sololevantados %>%
  left_join(df_sumado_BA_RL_sololevantados, by = c("Fecha","Municipio","UNA","Frecuencia")) %>%
  mutate(
    total_contenedores_BA_RL = coalesce(total_contenedores_BA_RL, 0),
    porc_BA_RL = round(total_contenedores_BA_RL / total_contenedores_con_misma_UNA * 100,2)
  ) %>%
  select(Fecha, Municipio, UNA, 
         Frecuencia,
         total_contenedores_con_misma_UNA,
         total_contenedores_BA_RL,
         porc_BA_RL) %>% 
  filter(Fecha < Sys.Date())



## fILTRO AQUELLOS QUE TENGAN MÁS DE 300 CONTENEDORES LEVANTADOS EN ESE VALOR DE UNA

df_final_sololevantados_ADEF_mas300 <- df_final_sololevantados %>% 
  filter(total_contenedores_con_misma_UNA >= 300) %>% 
  filter(Municipio %in% c("A","D","F","G"))

df_final_sololevantados_UNAmayorA100 <- df_final_sololevantados %>% 
  filter(UNA > 100) %>% 
  filter(Municipio %in% c("A","D","F","G"))

## filtro ADEF
# 
# library(openxlsx)
# 
# # Crear workbook
# wb <- createWorkbook()
# 
# # Agregar hoja
# addWorksheet(wb, "Datos")
# 
# # Escribir data frame con formato de tabla
# writeDataTable(wb, sheet = "Datos", x = df_final_sololevantados_UNAmayorA100, tableStyle = "TableStyleMedium9")
# 
# # Guardar como Excel
# saveWorkbook(wb, "mis_datos.xlsx", overwrite = TRUE)















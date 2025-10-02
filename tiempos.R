

library(dplyr)

llenado <- readRDS("scripts/db/10484_llenado/historico_llenado.rds")


llenado_agosto_ch <- llenado %>% 
  filter(Fecha >= "2025-08-01") %>% 
  filter(Municipio == "CH" | Circuito_corto == "B_103") %>% 
  arrange(desc(Fecha))

write.table(llenado_agosto_ch, "llenado_agostosetiembre2025.csv", sep = "\t", row.names = FALSE,na = "")


tiempos_por_circuito <- llenado_agosto_ch %>%
  group_by(Id_viaje, Circuito) %>%
  summarise(
    hora_inicio_PrimerContenedor = if (all(is.na(Fecha_hora_pasaje))) as.POSIXct(NA) else min(Fecha_hora_pasaje, na.rm = TRUE),
    hora_fin_UltimoContenedor = if (all(is.na(Fecha_hora_pasaje))) as.POSIXct(NA) else max(Fecha_hora_pasaje, na.rm = TRUE),
    minutos_por_circuito = ifelse(is.na(hora_inicio_PrimerContenedor) | is.na(hora_fin_UltimoContenedor),
                          NA_real_,
                          as.numeric(difftime(hora_fin_UltimoContenedor, hora_inicio_PrimerContenedor, units = "mins"))),
    total_contenedores_levantados = n(),
    .groups = "drop"
  ) %>% 
  mutate(minutos_por_contenedor = round(minutos_por_circuito / total_contenedores_levantados, 2))

write.table(tiempos_por_circuito, "tiempos_por_circuito.csv", sep = "\t", row.names = FALSE,na = "")



tiempos_viaje <- llenado_agosto %>%
  group_by(Id_viaje) %>%
  summarise(
    hora_inicio_PrimerContenedor = if (all(is.na(Fecha_hora_pasaje))) as.POSIXct(NA) else min(Fecha_hora_pasaje, na.rm = TRUE),
    hora_fin_UltimoContenedor = if (all(is.na(Fecha_hora_pasaje))) as.POSIXct(NA) else max(Fecha_hora_pasaje, na.rm = TRUE),
    minutos_por_idViaje = ifelse(is.na(hora_inicio_PrimerContenedor) | is.na(hora_fin_UltimoContenedor),
                          NA_real_,
                          as.numeric(difftime(hora_fin_UltimoContenedor, hora_inicio_PrimerContenedor, units = "mins"))),
    total_contenedores_levantados = n(),
    .groups = "drop"
  ) %>% 
  mutate(minutos_por_contenedor = round(minutos_por_idViaje / total_contenedores_levantados, 2))

write.table(tiempos_viaje, "tiempos_viaje.csv", sep = "\t", row.names = FALSE,na = "")

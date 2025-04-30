

pesadas <- read_delim("scripts/viajes/10450-20250417-120537.csv", 
                                     delim = "\t", escape_double = FALSE, 
                                     trim_ws = TRUE)

viajes <- historico_viajes %>% 
  filter(Fecha >= "2025-04-14") %>% 
  filter(Peso_neto == 0) %>% 
  filter(Lugar_salida == 50) %>% 
  filter((Estado != "Pendiente") & (Estado != "En viaje") & (Estado != "Suspendido")) %>% 
  filter(Cantidad_levantada > 0)

viajes_pesadas <- merge(
  viajes,
  pesadas,
  by = "Id_viaje",
  all = FALSE,          # FALSE equivale a inner join
  suffixes = c(".viajes", ".pesadas")
) 
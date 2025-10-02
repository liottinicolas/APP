
##### FILTRAR CONTENEDORES MAL UBICADOS ----

filtrar_contenedores_malubicados <- function(fecha_inicio_fueradelugar,
                                             fecha_fin_fueradelugar,
                                             df_incidencias_completo) {
  
  ## --- Filtrado base ---
  retorno <- df_incidencias_completo %>% 
    filter(Fecha >= fecha_inicio_fueradelugar & Fecha <= fecha_fin_fueradelugar) %>%
    filter(Condicion == "Fuera de Lugar" | Incidencia == "Contenedor Cruzado") %>% 
    arrange(desc(Fecha), Circuito,desc(Posicion))
  
  ## --- Recuentos ---
  retorno_conteo <- retorno %>% 
    group_by(gid) %>% 
    summarise(
      repite_contenedor_cruzado = sum(Incidencia == "Contenedor Cruzado", na.rm = TRUE),
      repite_fuera_de_lugar     = sum(Condicion  == "Fuera de Lugar",    na.rm = TRUE),
      Veces = repite_contenedor_cruzado + repite_fuera_de_lugar,
      .groups = "drop"
    )
  
  ## --- Resultado con metadatos únicos por gid ---
  cols_keep <- c(
    "gid", "Municipio", "Circuito", "Circuito_corto",
    "Posicion", "Direccion", "Observaciones"
  )
  
  resultado <- retorno %>% 
    select(all_of(cols_keep)) %>%     
    distinct(gid, .keep_all = TRUE) %>%  
    left_join(retorno_conteo, by = "gid") %>% 
    arrange(desc(Veces))
  
  ## --- Devolver ambos data frames en una lista ---
  return(
    list(
      total_veces = resultado,  # tabla resumida
      historico   = retorno     # filas filtradas originales
    )
  )
}


probando_incidencias <- historico_completo_llenado_incidencias
inicio <- as.Date("2025-05-01")
fin <- as.Date("2025-06-12")

salida <- filtrar_contenedores_malubicados(inicio, fin, probando_incidencias)

# Accedes así:
historico_malubicados <- salida$historico
total_malubicados   <- salida$total_veces












##### PLANILLA PARA ZL DE TEYMA ----


library(dplyr)
library(tidyr)
library(writexl)

# 📅 Fecha objetivo
fecha_objetivo <- as.Date("2025-08-05")

# 🧹 Filtrar histórico por la fecha
df_filtrado <- historico_ubicaciones %>%
  filter(as.Date(Fecha) == fecha_objetivo)

# 🧮 Contar cuántos contenedores hay por circuito
conteos <- df_filtrado %>%
  count(Circuito_corto, name = "cantidad")

# 🔢 Máximo de columnas necesarias
max_contenedores <- max(conteos$cantidad, na.rm = TRUE)

# 🧱 Armar vector con "" y ❌
df_expandido <- conteos %>%
  rowwise() %>%
  mutate(vector = list(
    c(rep("", cantidad), rep("❌", max_contenedores - cantidad))
  )) %>%
  ungroup()

# 🧷 Expandir columnas Pos_1, Pos_2, ...
df_expandido <- df_expandido %>%
  mutate(vector = lapply(vector, function(x) setNames(as.list(x), paste0("Pos_", seq_along(x))))) %>%
  unnest_wider(vector)

# 💾 Exportar a Excel
write_xlsx(df_expandido, path = "tabla_circuitos_emoji.xlsx")




# ARREGLAR PESADAS PRUEBA ----
library(dplyr)
library(stringr)
library(hms)
library(lubridate)

pesada_2020_2023 <- read_delim("archivos/10450_pesadas/2020_a_2023.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                           Hora = col_time(format = "%H:%M:%S")), 
                          trim_ws = TRUE)

pesada_2024 <- read_delim("archivos/10450_pesadas/2024.csv", 
                            delim = "\t", escape_double = FALSE, 
                            col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                             Hora = col_time(format = "%H:%M:%S")), 
                            trim_ws = TRUE)

prueba_pesada <- rbind(pesada_2020_2023,pesada_2024)


prueba_pesada_im <- prueba_pesada %>% 
  filter(str_detect(Matricula, "^SIM")) 

prueba_pesada_im <- prueba_pesada_im %>%
  mutate(
    Id_Turno = case_when(
      # Turno 3: 22:00 a 05:59
      Hora >= as_hms("22:00:00") | Hora < as_hms("06:00:00") ~ 3,
      # Turno 1: 06:00 a 13:59
      Hora >= as_hms("06:00:00") & Hora < as_hms("14:00:00") ~ 1,
      # Turno 2: 14:00 a 21:59
      Hora >= as_hms("14:00:00") & Hora < as_hms("22:00:00") ~ 2,
      # En caso de que haya una hora que no caiga en ninguno de los rangos (opcional)
      TRUE ~ NA_real_
    )
  )
# Crear la columna Turno según el Id_Turno
prueba_pesada_im <- prueba_pesada_im %>%
  mutate(
    Turno = case_when(
      Id_Turno == 1 ~ "MATUTINO",
      Id_Turno == 2 ~ "VESPERTINO",
      Id_Turno == 3 ~ "NOCTURNO",
      TRUE ~ "Desconocido"
    )
  )

prueba_pesada_im <- prueba_pesada_im %>%
  relocate(Id_Turno, Turno, .after = Hora)

# Crear la columna Fecha_viaje según la lógica del Id_Turno
prueba_pesada_im <- prueba_pesada_im %>%
  mutate(
    Fecha_viaje = case_when(
      Id_Turno == 3 ~ Fecha - days(1),  # Restar 1 día si el turno es 3
      TRUE ~ Fecha                     # Mantener la misma fecha si el turno es 1 o 2
    )
  )

# Reorganizar el data frame para que Fecha_viaje quede después de Fecha
prueba_pesada_im <- prueba_pesada_im %>%
  relocate(Fecha_viaje, .after = Fecha)

#### TIPO VEHÍCULOS POR MATRICULAS ----

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

# Agregar columna de tipo de vehículo
prueba_viajes <- prueba_viajes %>%
  mutate(Tipo_vehiculo = case_when(
    Matricula %in% recolectores_compactadores$Matricula_formateada ~ "CajaDesmontable",
    TRUE ~ "Convencional"
  ))







### UBICACIONES, CORRECCIONES ----

ubisccee <- historico_ubicaciones %>% 
  filter(Fecha == "2025-09-26")

diario <- historico_estado_diario 
  

ubisdfr <- historico_DFR_ubicaciones %>% 
  filter(Fecha == "2025-09-26") 

eliminados <- historico_DFR_ubicaciones_DEBAJA %>% 
  arrange(desc(FACT))


elim_last <- eliminados %>%
  mutate(GID = as.character(GID),
         FECHA_HASTA = ymd(FECHA_HASTA)) %>%        # ajusta si no es AAAA-MM-DD
  arrange(GID, desc(FECHA_HASTA)) %>%
  distinct(GID, .keep_all = TRUE) %>%
  select(GID, FECHA_HASTA)

historico_estado_diario <- historico_estado_diario %>%
  mutate(gid = as.character(gid)) %>%
  left_join(elim_last, by = c("gid" = "GID"))

historico_estado_diario <- historico_estado_diario %>% 
  filter(is.na(FECHA_HASTA) | Fecha <= FECHA_HASTA)


u1 <- ubisccee %>% distinct(gid, .keep_all = TRUE)
u2 <- ubisdfr  %>% distinct(gid, .keep_all = TRUE)

solo_sccee <- u1 %>% 
  anti_join(u2 %>% select(gid), by = "gid") %>% 
  mutate(origen = "solo_sccee")

solo_dfr <- u2 %>% 
  anti_join(u1 %>% select(gid), by = "gid") %>% 
  mutate(origen = "solo_dfr")

diferencias_por_gid <- bind_rows(solo_sccee, solo_dfr)


#### 








contenedor_no_esta <- historico_completo_llenado_incidencias %>%
  filter(Incidencia == "Contenedor No Está") %>% 
  group_by(gid) %>% 
  summarise(Veces = n()) %>% 
  slice_max(Veces, n = 10) 

ultimas <- historico_ubicaciones %>%
  group_by(gid) %>%
  slice_max(Fecha, n = 1, with_ties = FALSE) %>%  # una fila: la de fecha mayor
  ungroup() %>%
  select(gid, Circuito, Posicion, Calle, Numero)

contenedor_no_esta_enriq <- contenedor_no_esta %>%
  left_join(ultimas, by = "gid")


contenedor_fueradelugar <- historico_completo_llenado_incidencias %>%
  filter(grepl("Fuera de Lugar", Condicion)) %>%  
  group_by(gid) %>% 
  summarise(Veces = n()) %>% 
  slice_max(Veces, n = 10) 

ultimas <- historico_ubicaciones %>%
  group_by(gid) %>%
  slice_max(Fecha, n = 1, with_ties = FALSE) %>%  # una fila: la de fecha mayor
  ungroup() %>%
  select(gid, Circuito, Posicion, Calle, Numero)

contenedor_fueradelugar <- contenedor_fueradelugar %>%
  left_join(ultimas, by = "gid")














# Probar obtener direcciones con ubicacion POINT (1 sola) ----

# install.packages(c("sf","nominatimlite"))  # una vez
library(sf)
library(nominatimlite)

pt_utm <- st_sfc(st_point(c(576631, 6143058)), crs = 32721)
pt_wgs <- st_transform(pt_utm, 4326)
lonlat <- st_coordinates(pt_wgs)[1,]
lon <- lonlat[1]; lat <- lonlat[2]
 

library(tidygeocoder)
asd <- reverse_geocode(
  data.frame(lat = lat, long = lon),
  lat = lat, long = long,
  method = "osm", full_results = TRUE
)

# probando unir
library(sf); library(dplyr); library(units)

probar <- historico_DFR_ubicaciones %>% 
  filter(Fecha == "2025-09-29")

reclamos_final_prueba <- reclamos_final %>% 
  arrange(desc(FECHA_INGRESO_RECLAMO)) %>% 
  head(2)

x <- st_transform(reclamos_final_prueba, 32721)
y <- st_transform(probar, 32721)

idx <- st_nearest_feature(x, y)                 # índice del y más cercano
out <- bind_cols(x, st_drop_geometry(y[idx, ])) # “left join” por cercanía

dist_m <- st_distance(x, y[idx, ], by_element = TRUE) %>% set_units("m") %>% drop_units()
out$dist_m <- dist_m


## Probar obtener direcciones con ubicacion POINT ----



library(sf)
library(dplyr)
library(tidygeocoder)

# sf en UTM 21S -> WGS84
sf_wgs <- historico_DFR_ubicaciones_DEBAJA |> st_transform(4326)
coords <- st_coordinates(sf_wgs)

# anclo lon/lat al df y redondeo para joinear sin ruido
df_all <- sf_wgs |>
  st_drop_geometry() |>
  mutate(
    long = coords[,1],
    lat  = coords[,2],
    long_r = round(long, 7),
    lat_r  = round(lat, 7)
  )

# coord únicas a geocodificar
uniq <- df_all |> distinct(long_r, lat_r) |>
  rename(long = long_r, lat = lat_r)

# reverse geocoding OSM (vectorizado)
geo_uniq <- reverse_geocode(
  uniq, lat = lat, long = long,
  method = "osm", full_results = TRUE
)

# traer resultados a todas las filas
geo_all <- df_all |>
  left_join(geo_uniq, by = c("long_r" = "long", "lat_r" = "lat"))

# armar salida con dirección legible y campos útiles
ubicaciones_con_direccion <- geo_all |>
  mutate(
    direccion = if_else(!is.na(house_number) | !is.na(road),
                        paste0(coalesce(road, ""), " ", coalesce(house_number, "")),
                        display_name)
  ) |>
  select(
    id, GID, REGION, COD_RECORRIDO, POSICION, FECHA_DESDE, FECHA_HASTA,
    OBSERVACIONES, COD_MUNICIPIO,
    lat, long, direccion, display_name, house_number, road,
    neighbourhood, suburb, city, town, village, state, postcode, country
  )


ruta_RSD_historico_con_ubicacion <- file.path(ruta_proyecto, "scripts/db/DFR_ubicaciones/historico_DFR_posiciones_DEBAJA_condireccion.rds")

saveRDS(geo_all, ruta_RSD_historico_con_ubicacion)
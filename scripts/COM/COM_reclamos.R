library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(readr) # parse_number
library(googlesheets4)
library(janitor)
library(lubridate)
library(hms)

source("scripts/para_mapear/circuitos_planificados.R")


cargar_df_completo_ZL <- function(){
  
  gs4_deauth()
  url <- "https://docs.google.com/spreadsheets/d/1FguUTUmtHrbDJPOI6qav3e1IXiV5byH1uVv0DnkzMWk/edit?gid=1615275865#gid=1615275865"
  zonalimpia_base <- read_sheet(
    url,
    sheet = "REGISTRO DE ZONA LIMPIA",
    range = "A:AC")

  zonalimpia <- zonalimpia_base %>% 
    # filter(FECHA == fecha_ver) %>% 
    select(FECHA,MUN,CIRCUITO,EMPRESA,`DESDE CONT.`,TURNO,MATRÍCULA,`HORARIO COMIENZO`,`HORARIO FINALIZACIÓN`,`POSICIONES ATENDIDAS`,`REALIZADO HASTA CONT.`,`PESADA EN DFR`, `Nº DE AUTORIZACIÓN`) %>% 
    rename(
      Fecha_ZL                 = FECHA,
      Municipio                   = MUN,
      Circuito_corto              = CIRCUITO,
      Empresa               = EMPRESA,
      Contenedor_inicial            = `DESDE CONT.`,
      Turno_ZL                 = TURNO,
      Matricula             = `MATRÍCULA`,
      Hora_inicio      = `HORARIO COMIENZO`,
      Hora_fin  = `HORARIO FINALIZACIÓN`,
      Posiciones_atendidas  = `POSICIONES ATENDIDAS`,
      Contenedor_final  = `REALIZADO HASTA CONT.`,
      Pesada_en_dfr         = `PESADA EN DFR`,
      Numero_autorizacion        = `Nº DE AUTORIZACIÓN`
    )

  zonalimpia$Fecha_ZL <- as.Date(zonalimpia$Fecha_ZL, format = "%d/%m/%Y")
  
  zonalimpia <- zonalimpia %>% 
    filter(!is.na(Contenedor_inicial)) %>% 
    filter(!is.na(Matricula)) %>% 
    filter(!if_any(c(Hora_inicio, Hora_fin),
                   ~ is.na(.x) | grepl("^[-\\s]*$", .x)))
  
  zonalimpia <- zonalimpia |> mutate(Hora_inicio = as_hms(Hora_inicio))  # queda 06:04:00
  zonalimpia <- zonalimpia |> mutate(Hora_fin = as_hms(Hora_fin))  # queda 06:04:00
  
  zonalimpia <- zonalimpia %>%
    mutate(
      # Asegura Date
      Fecha_ZL = as.Date(Fecha_ZL),
      
      # Normaliza turno
      turno = toupper(trimws(as.character(Turno_ZL))),
      
      # Toma solo HH:MM:SS como hms (evita comparar char vs hms)
      t_ini = parse_time(as.character(Hora_inicio)),  # -> hms
      t_fin = parse_time(as.character(Hora_fin)),     # -> hms
      
      # Regla: NOCTURNO y < 09:00 suma +1 día
      add_ini = as.integer(turno == "NOCTURNO" & !is.na(t_ini) & t_ini < hms(hours = 9)),
      add_fin = as.integer(turno == "NOCTURNO" & !is.na(t_fin) & t_fin < hms(hours = 9)),
      
      # Construye POSIXct en Montevideo
      Hora_inicio_dt = make_datetime(
        year(Fecha_ZL), month(Fecha_ZL), day(Fecha_ZL) + coalesce(add_ini, 0L),
        hour = hour(t_ini), min = minute(t_ini), sec = second(t_ini),
        tz = "America/Montevideo"
      ),
      Hora_fin_dt = make_datetime(
        year(Fecha_ZL), month(Fecha_ZL), day(Fecha_ZL) + coalesce(add_fin, 0L),
        hour = hour(t_fin), min = minute(t_fin), sec = second(t_fin),
        tz = "America/Montevideo"
      )
    ) %>%
    select(-turno, -t_ini, -t_fin, -add_ini, -add_fin)
  
  
  # Reemplaza NULL/longitud 0 por NA dentro de cada list-column
  zonalimpia <- zonalimpia %>%
    mutate(across(where(is.list),
                  ~ map(.x, ~ if (is.null(.x) || length(.x) == 0) NA else .x)))
  
  # Si la list-column tiene solo escalares, la aplanamos a character
  zonalimpia <- zonalimpia %>%
    mutate(across(where(is.list), ~ if (all(map_int(.x, length) <= 1))
      map_chr(.x, ~ if (is.null(.x)) NA_character_ else as.character(.x))
      else .x))
  
  # Le pongo al CIRCUITO EL _ SEPARADOR A101 -> A_101
  zonalimpia$Circuito_corto <- str_replace_all(zonalimpia$Circuito_corto, "(?<=\\p{L})(?=\\d)", "_")
  
  zonalimpia <- zonalimpia %>%
    filter(!is.na(Circuito_corto)) %>%
    filter(str_detect(Circuito_corto, "^(CH|[A-G])"))
  
  return(zonalimpia)
}

  



zonalimpia_nuevo <- cargar_df_completo_ZL()
fecha_inicio <- min(zonalimpia_nuevo$Fecha_ZL)
fecha_fin <- max(zonalimpia_nuevo$Fecha_ZL)


# Elegir rango u hoja por nombre


# Reclamos
#X2025 <- read_csv("scripts/COM/2025.csv")
# ZL
#zonalimpia <- read_csv("scripts/COM/zonalimpia.csv")


#zonalimpia_nuevo <- df



fecha_maxima <- max(zonalimpia_nuevo$FECHA)-1

# Arreglar el df.


## Funcion para expander las posiciones de ZL ----
expandar_posiciones <- function(df,
                                col_ini = "Contenedor_inicial",
                                col_fin = "Contenedor_final",
                                col_out = "Posiciones_atendidas") {
  ini_sym <- sym(col_ini)
  fin_sym <- sym(col_fin)
  
  to_num <- function(x) {
    if (is.numeric(x)) as.integer(x) else parse_number(as.character(x))
  }
  
  df %>%
    mutate(
      .ini = to_num(!!ini_sym),
      .fin = to_num(!!fin_sym),
      .seq = map2(.ini, .fin, ~ {
        if (is.na(.x) || is.na(.y)) integer(0)
        else if (.x == .y) .x
        else seq(.x, .y)
      })
    ) %>%
    filter(lengths(.seq) > 0) %>%
    unnest(.seq) %>%
    mutate(!!col_out := .seq) %>%
    select(-.ini, -.fin, -.seq)
}

## FIN - Funcion para expander las posiciones de ZL ----


# Uso
zonalimpia_extendida <- expandar_posiciones(zonalimpia_nuevo)
zonalimpia_extendida <- zonalimpia_extendida %>% 
  rename(Posicion = Posiciones_atendidas) %>%
  filter(Fecha_ZL < Sys.Date())

# Arreglo la hora de hora fin e inicio.

clean_hhmm <- function(x) {
  x <- as.character(x) |> str_squish()
  x <- str_replace_all(x, "[；;:．。hH]", ":")
  x <- str_replace_all(x, "[^0-9:]", "")
  x <- ifelse(str_detect(x, "^\\d{3,4}$"),
              paste0(substr(x, 1, nchar(x)-2), ":", substr(x, nchar(x)-1, nchar(x))), x)
  hhmm <- str_extract(x, "\\b\\d{1,2}:\\d{2}\\b")
  hh <- suppressWarnings(as.integer(str_extract(hhmm, "^\\d{1,2}")))
  mm <- suppressWarnings(as.integer(str_extract(hhmm, "(?<=:)\\d{2}")))
  hhmm[is.na(hh) | is.na(mm) | hh < 0 | hh > 23 | mm < 0 | mm > 59] <- NA
  hhmm
}

  zonalimpia_extendida <- zonalimpia_extendida %>%
  mutate(
    Fecha_ZL = as.Date(Fecha_ZL),
    Hora_inicio_cl = clean_hhmm(Hora_inicio),
    Hora_fin_cl    = clean_hhmm(Hora_fin),
    
    # NOCTURNO: 22:00–23:59 usa Fecha_ZL; 00:00–08:00 usa Fecha_ZL + 1
    Hora_inicio_dt = as.POSIXct(
      paste(
        Fecha_ZL + if_else(
          toupper(Turno_ZL) == "NOCTURNO" &
            !is.na(Hora_inicio_cl) &
            (
              as.integer(str_extract(Hora_inicio_cl, "^\\d{1,2}")) < 8 |
                (
                  as.integer(str_extract(Hora_inicio_cl, "^\\d{1,2}")) == 8 &
                    as.integer(str_extract(Hora_inicio_cl, "(?<=:)\\d{2}")) == 0
                )
            ),
          1L, 0L
        ),
        Hora_inicio_cl
      ),
      format = "%Y-%m-%d %H:%M", tz = "America/Montevideo"
    ),
    
    Hora_fin_dt = as.POSIXct(
      paste(
        Fecha_ZL + if_else(
          toupper(Turno_ZL) == "NOCTURNO" &
            !is.na(Hora_fin_cl) &
            (
              as.integer(str_extract(Hora_fin_cl, "^\\d{1,2}")) < 8 |
                (
                  as.integer(str_extract(Hora_fin_cl, "^\\d{1,2}")) == 8 &
                    as.integer(str_extract(Hora_fin_cl, "(?<=:)\\d{2}")) == 0
                )
            ),
          1L, 0L
        ),
        Hora_fin_cl
      ),
      format = "%Y-%m-%d %H:%M", tz = "America/Montevideo"
    )
    
    # Duracion_horas = as.numeric(difftime(Hora_fin_dt, Hora_inicio_dt, units = "hours")),
    # Hora_inicio_h  = hour(Hora_inicio_dt) + minute(Hora_inicio_dt)/60,
    # Hora_fin_h     = hour(Hora_fin_dt) + minute(Hora_fin_dt)/60
  ) %>%
  select(-Hora_inicio_cl, -Hora_fin_cl)  # opcional: limpiar helpers


  
## Funcion para que por día, me busque las posiciones, y las anexe  

funcion_obtener_posicionesdeldia <- function(fecha_datos){
  hist_dia <- historico_ubicaciones %>%
    filter(Fecha == fecha_datos) %>% 
    mutate(Fecha = as.Date(Fecha), Posicion = as.integer(Posicion)) %>%
    select(Fecha,Circuito_corto, Posicion, gid,Estado)
  
  return(hist_dia)

}
## Buscar el gid en posiciones.

fecha_ini <- min(zonalimpia_extendida$Fecha_ZL)
fecha_fin <- max(zonalimpia_extendida$Fecha_ZL)

 dias <- seq.Date(fecha_ini, fecha_fin, by = "day")

pos_diarias <- map_dfr(dias, \(d) {
  df <- funcion_obtener_posicionesdeldia(as.Date(d))
  cat("Procesando día:", format(d, "%Y-%m-%d"), "\n")
  df %>%
    mutate(
      Fecha = as.Date(Fecha),                         # asegura Date
      Circuito_corto = as.character(Circuito_corto),
      Posicion = as.integer(Posicion),
      gid = as.character(gid)
    ) %>%
    select(Fecha, Circuito_corto, Posicion,Estado, gid)
}) 

# Limpieza con historico dfr.

debaja <- historico_DFR_ubicaciones_DEBAJA %>%
  transmute(
    gid_chr     = as.character(GID),
    fecha_baja  = as.Date(FECHA_HASTA)
  ) %>%
  filter(!is.na(fecha_baja)) %>%
  group_by(gid_chr) %>%
  summarise(fecha_baja = max(fecha_baja), .groups = "drop")  # última baja por gid

# 2) Remover en pos_diarias las filas con Fecha <= fecha_baja
pos_diarias_filtrado <- pos_diarias %>%
  mutate(
    gid   = as.character(gid),
    Fecha = as.Date(Fecha)
  ) %>%
  left_join(debaja, by = c("gid" = "gid_chr")) %>%
  filter(is.na(fecha_baja) | Fecha > fecha_baja) %>%   # elimina <= baja
  select(-fecha_baja)


# Normaliza tipos
zle <- zonalimpia_extendida %>%
  mutate(
    Fecha_ZL       = as.Date(Fecha_ZL),
    Circuito_corto = as.character(Circuito_corto),
    Posicion       = as.integer(Posicion)
  )

y <- pos_diarias_filtrado %>%
  select(-geometry) %>%                      # geometry fuera
  mutate(
    Fecha          = as.Date(Fecha),
    Circuito_corto = as.character(Circuito_corto),
    Posicion       = as.integer(Posicion),
    gid            = as.character(gid)
  )

# Opción A: un solo gid por combinación (toma el primero por orden)
y_uniq <- y %>%
  arrange(Fecha, Circuito_corto, Posicion, gid) %>%
  group_by(Fecha, Circuito_corto, Posicion) %>%
  summarise(gid = first(gid), .groups = "drop")

zle_con_gid <- zle %>%
  left_join(
    y_uniq,
    by = c("Fecha_ZL" = "Fecha",
           "Circuito_corto" = "Circuito_corto",
           "Posicion" = "Posicion"),
    relationship = "many-to-one"
  )



## que pasa con gids vacios?

zonalimpia_extendida_congid_vacios <- zle_con_gid %>% 
  filter(is.na(gid))

####

zle_con_gid_completos <- zle_con_gid %>% 
  filter(!is.na(gid))


## probar municipio A

zle_con_gid_completos_MUN_A <- zle_con_gid_completos %>% 
  filter(Municipio == "A")


cargar_ultimo_levante_recursivo <- function(df_zle,fecha_completar){
  
  hist_S_max <- historico_llenado %>%
    filter(Fecha <= fecha_completar) %>% 
    mutate(
      gid = as.character(gid),
      Fecha_hora_pasaje = to_dt(Fecha_hora_pasaje)
    ) %>%
    filter(Levantado == "S", !is.na(Fecha_hora_pasaje)) %>%
    group_by(gid) %>%
    summarise(Fecha_hora_pasaje = max(Fecha_hora_pasaje), .groups = "drop")
  
  # 2) Traerlo a zonalimpia_extendida_congid
  df_zle <- df_zle %>%
    mutate(gid = as.character(gid)) %>%
    left_join(hist_S_max, by = "gid")
  
  return(df_zle)
  
}

cargar_ultimo_levante_recursivo_doshoras <- function(df_zle,fecha_completar){
  
  hist_S_max <- historico_llenado %>%
    filter(Fecha <= fecha_completar) %>%
    mutate(gid = as.character(gid),
           Fecha_hora_pasaje = to_dt(Fecha_hora_pasaje)) %>%
    filter(Levantado == "S", !is.na(Fecha_hora_pasaje)) %>%
    group_by(gid) %>%
    arrange(desc(Fecha_hora_pasaje), .by_group = TRUE) %>%
    summarise(
      Fecha_mas_reciente_levante_Contenedor = first(Fecha_hora_pasaje),
      Segunda_fecha_mas_reciente_levante_Contenedor = nth(Fecha_hora_pasaje, 2),            # NA si no hay 2
      .groups = "drop"
    )
  
  # 2) Traerlo a zonalimpia_extendida_congid
  df_zle <- df_zle %>%
    mutate(gid = as.character(gid)) %>%
    left_join(hist_S_max, by = "gid")
  
  return(df_zle)
  
}

to_dt <- function(x) {
  if (inherits(x, "POSIXt")) x else parse_date_time(
    x, orders = c("Y-m-d H:M:S", "Y-m-d H:M", "d/m/Y H:M:S", "d/m/Y H:M"),
    tz = "America/Montevideo", quiet = TRUE
  )
}


fecha_ini_it <- min(zle_con_gid_completos_MUN_A$Fecha_ZL)
fecha_fin_it <- max(zle_con_gid_completos_MUN_A$Fecha_ZL)

cols <- c("Fecha_ZL","Municipio","Circuito_corto","Empresa","Contenedor_inicial",
          "Turno_ZL","Matricula","Hora_inicio","Hora_fin","Posicion",
          "Contenedor_final","Pesada_en_dfr","Numero_autorizacion",
          "Hora_inicio_dt","Hora_fin_dt","gid")

acumulado <- zle_con_gid_completos_MUN_A %>% select(all_of(cols)) %>% slice(0)

while (fecha_ini_it <= fecha_fin_it) {
  print(fecha_ini_it)
  df_zldeldia <- zle_con_gid_completos_MUN_A %>% 
    filter(Fecha_ZL == fecha_ini_it)
  if(nrow(df_zldeldia) > 0){
    df_zldeldia <- cargar_ultimo_levante_recursivo_doshoras(df_zldeldia,fecha_ini_it)
    acumulado <- bind_rows(acumulado,df_zldeldia)
  }
  
  fecha_ini_it <- fecha_ini_it + 1
  
}



## probar que es el levante a




# zonalimpia_extendida_congid

## Traer la hora y fecha del último levante.

# # parser robusto para varios formatos
# to_dt <- function(x) {
#   if (inherits(x, "POSIXt")) x else parse_date_time(
#     x, orders = c("Y-m-d H:M:S", "Y-m-d H:M", "d/m/Y H:M:S", "d/m/Y H:M"),
#     tz = "America/Montevideo", quiet = TRUE
#   )
# }

# # 1) Último pasaje con S por gid
# hist_S_max <- historico_llenado %>%
#   filter(Fecha < fecha_ver) %>% 
#   mutate(
#     gid = as.character(gid),
#     Fecha_hora_pasaje = to_dt(Fecha_hora_pasaje)
#   ) %>%
#   filter(Levantado == "S", !is.na(Fecha_hora_pasaje)) %>%
#   group_by(gid) %>%
#   summarise(Fecha_hora_pasaje = max(Fecha_hora_pasaje), .groups = "drop")
# 
# # 2) Traerlo a zonalimpia_extendida_congid
# zonalimpia_extendida_congid_masultimolevante <- zle_con_gid_completos %>%
#   mutate(gid = as.character(gid)) %>%
#   left_join(hist_S_max, by = "gid")


# Agrego la frecuencia.
datos_por_circuito <- datos_circuitos

freq_key <- datos_por_circuito %>%
  mutate(circuito_corto = as.character(circuito_corto)) %>%
  arrange(desc(FechaFin), desc(Fecha_inicio)) %>%      # prioriza la más reciente
  distinct(circuito_corto, .keep_all = TRUE) %>%
  select(circuito_corto, Frecuencia) %>% 
  mutate(Frecuencia = as.numeric(Frecuencia))

zonalimpia_extendida_congid_masultimolevante_confrecuencia <- acumulado %>%
  mutate(Circuito_corto = as.character(Circuito_corto)) %>%
  left_join(freq_key, by = c("Circuito_corto" = "circuito_corto"))

# ME quedo acá con el horario más reciente a la zona limpia
zonalimpia_extendida_congid_masultimolevante_confrecuencia <- zonalimpia_extendida_congid_masultimolevante_confrecuencia %>%
  mutate(
    Fecha_hora_pasaje_REAL = case_when(
      Fecha_mas_reciente_levante_Contenedor         <= Hora_inicio_dt ~ Fecha_mas_reciente_levante_Contenedor,
      Segunda_fecha_mas_reciente_levante_Contenedor <= Hora_inicio_dt ~ Segunda_fecha_mas_reciente_levante_Contenedor,
      TRUE ~ as.POSIXct(NA)
    )
  ) 

zonalimpia_extendida_congid_masultimolevante_confrecuencia_horasrecoleccion <-
  zonalimpia_extendida_congid_masultimolevante_confrecuencia %>%
  mutate(
    Fecha_hora_pasaje_REAL = if (inherits(Fecha_hora_pasaje_REAL, "POSIXt")) Fecha_hora_pasaje_REAL else
      parse_date_time(Fecha_hora_pasaje_REAL,
                      orders = c("Y-m-d H:M:S","Y-m-d H:M","d/m/Y H:M:S","d/m/Y H:M"),
                      tz = "America/Montevideo", quiet = TRUE),
    Dif_horas_pasaje_desde_inicio   = as.numeric(difftime(Hora_inicio_dt, Fecha_hora_pasaje_REAL, units = "hours")),
    Dif_minutos_pasaje_desde_inicio = as.numeric(difftime(Hora_inicio_dt, Fecha_hora_pasaje_REAL, units = "mins"))
  )

zonalimpia_extendida_congid_masultimolevante_confrecuencia_horasrecoleccion_una <-
  zonalimpia_extendida_congid_masultimolevante_confrecuencia_horasrecoleccion %>% 
dplyr::mutate(UNA_inicio_ZL = (Dif_horas_pasaje_desde_inicio/(7/Frecuencia)/24)*100)



z_final <- zonalimpia_extendida_congid_masultimolevante_confrecuencia_horasrecoleccion_una %>%
  mutate(gid = as.character(gid),
         Fecha_ZL = as.Date(Fecha_ZL)) %>%
  left_join(
    pos_diarias_filtrado %>%
      mutate(gid = as.character(gid),
             Fecha = as.Date(Fecha)) %>%
      transmute(gid, Fecha, Posicion, Estado),           # lo que vas a anexar
    by = c("gid", "Fecha_ZL" = "Fecha", "Posicion" = "Posicion")
  ) 
    


  
planilla_final_ZL <- z_final %>% 
  filter(is.na(Estado)) %>% 
  arrange(gid,desc(Fecha_ZL),desc(Hora_inicio_dt))


drive <- planilla_final_ZL %>% 
  select(Municipio,Circuito_corto,gid,Hora_inicio_dt,Fecha_hora_pasaje_REAL,UNA_inicio_ZL)


library(writexl)

write_xlsx(drive, "drve.xlsx")







### RECLAMOS ----
url <- "https://ckan-data.montevideo.gub.uy/dataset/c34e11ea-c547-46d5-80c6-d0bf283c002f/resource/ccb644b1-79b0-4caa-a353-6cfa816f3f70/download/reclamos.zip"

library(readr)

url <- "https://ckan-data.montevideo.gub.uy/dataset/45a29508-20d4-4adb-8b23-4f6e2cfec2be/resource/bbdbd017-bb79-4b17-b995-69077238ce54/download/solicitudes_al_com_2025.csv"

datos <- read_csv(url)

# X2025 <- read_csv("scripts/COM/2025.csv")

reclamos <- datos %>% 
  filter(incidente == "Residuos esparcidos" | incidente == "Basura fuera del contenedor") %>% 
  filter(tipo_de_reclamo == "VALIDO") %>% 
  filter(!is.na(longitud))

reclamos_ver <- reclamos %>% 
  group_by(estado) %>% 
  summarise(total = n())



# 1) Asegurá CRS del histórico
historico_sf <- st_as_sf(historico_DFR_ubicaciones)   # ya trae geometry
if (is.na(st_crs(historico_sf))) st_crs(historico_sf) <- 32721  # UTM 21S

# 2) Pasá reclamos (lon/lat) a sf y al mismo CRS
reclamos_sf <- st_as_sf(reclamos, coords = c("longitud","latitud"), crs = 4326, remove = FALSE) |>
  st_transform(st_crs(historico_sf))

# # 3) Match más cercano + distancia  
# idx  <- st_nearest_feature(reclamos_sf, historico_sf)
# dist <- st_distance(reclamos_sf, historico_sf[idx, ], by_element = TRUE)
# 
# out <- reclamos_sf |>
#   mutate(gid_cercano = historico_sf$gid[idx],
#          posicion_cercana = historico_sf$Posicion[idx],
#          distancia_m = as.numeric(dist)) |>
#   filter(distancia_m <= 20)  # opcional umbral

library(sf); library(dplyr)

match_por_cercania <- function(reclamos, historico, umbral = NULL) {
  historico_sf <- st_as_sf(historico); if (is.na(st_crs(historico_sf))) st_crs(historico_sf) <- 32721
  reclamos_sf  <- st_as_sf(reclamos, coords = c("longitud","latitud"), crs = 4326, remove = FALSE) |>
    st_transform(st_crs(historico_sf))
  
  idx  <- st_nearest_feature(reclamos_sf, historico_sf)
  dist <- st_distance(reclamos_sf, historico_sf[idx, ], by_element = TRUE)
  
  out <- reclamos_sf |>
    mutate(gid_cercano = historico_sf$gid[idx],
           posicion_cercana = historico_sf$Posicion[idx],
           distancia_m = as.numeric(dist))
  
  if (!is.null(umbral)) out <- out |> filter(distancia_m <= umbral) # si NULL, no filtra
  out
}

reclamos_A <- reclamos %>% 
  filter(municipio == "A") %>% 
  filter(circuito != "-") %>% 
  filter(fecha_de_reclamo > ymd_hms("2025-06-07 00:00:00", tz = "America/Montevideo"))

# Uso:
res_todo   <- match_por_cercania(reclamos, historico_DFR_ubicaciones)          # sin umbral
res_50m    <- match_por_cercania(reclamos_A, historico_DFR_ubicaciones, 50)      # con umbral




## Unir con la información

res_sin_geo <- res_50m %>% st_drop_geometry()   # o: st_set_geometry(NULL)

# out <- res_sin_geo %>%
#   group_by(gid_cercano) %>% 
#   summarise(total = n())

## Agrupo los reclamos por fecha y gid.
out <- res_sin_geo %>%
  group_by(dia = as.Date(fecha_de_reclamo), gid_cercano) %>%
  summarise(n = n(), .groups = "drop")



### intengo agrupar


# # 1) Normalizo y dedupe en out (una fila por gid_cercano-dia)
# out_clean <- out %>%
#   mutate(
#     gid_cercano = as.character(gid_cercano),
#     dia = as.Date(dia)
#   ) %>%
#   group_by(gid_cercano, dia) %>%
#   summarise(n = first(n), .groups = "drop")  # o max(n) si querés

# 2) Join a planilla_final_ZL y agrego la columna n
planilla_final_ZL2 <- planilla_final_ZL %>%
  mutate(
    gid = as.character(gid),
    Fecha_ZL = as.Date(Fecha_ZL)
  ) %>%
  left_join(out, by = c("gid" = "gid_cercano", "Fecha_ZL" = "dia"))



## Cuantos tienen reclamos de los zl hechos??
planilla_final_ZL2_conreclamos <- planilla_final_ZL2 %>% 
  filter(!is.na(n))



# zonalimpia2: usa Fecha_ZL si existe, si no usa Fecha
zl2 <- planilla_final_ZL2 %>%
  mutate(
    gid = as.character(gid),
    fecha = as.Date(if ("Fecha_ZL" %in% names(.)) Fecha_ZL else Fecha)
  ) %>%
  distinct(gid, fecha, .keep_all = TRUE)

# out: una fila por (gid_cercano, dia)
out_clean <- out %>%
  mutate(gid_cercano = as.character(gid_cercano),
         dia = as.Date(dia)) %>%
  group_by(gid_cercano, dia) %>%
  summarise(n = first(n), .groups = "drop")

# Filas de out que NO encontraron match en zonalimpia2
out_sin_match <- out_clean %>%
  anti_join(zl2, by = c("gid_cercano" = "gid", "dia" = "fecha"))

# Solo las claves no matcheadas
claves_no_match <- out_sin_match %>% select(gid_cercano, dia)

# (Opcional) Las de zonalimpia2 sin match en out
zl2_sin_match <- zl2 %>%
  anti_join(out_clean, by = c("gid" = "gid_cercano", "fecha" = "dia"))

















library(sf)
library(leaflet)
library(dplyr)

historico_DFR_ubicaciones_hoy <- historico_DFR_ubicaciones %>% 
  filter(Fecha == fecha_ver)

# --- 1) Preparar datos como sf y al mismo CRS (WGS84) ---
# histórico: ya tiene geometry; si no trae CRS, forzá 32721 (UTM 21S) y pasá a 4326
historico_sf <- st_as_sf(historico_DFR_ubicaciones_hoy)
if (is.na(st_crs(historico_sf))) st_crs(historico_sf) <- 32721
historico_ll <- st_transform(historico_sf, 4326)

# reclamos: columnas longitud/latitud -> POINT en 4326
reclamos_sf <- st_as_sf(reclamos, coords = c("longitud","latitud"), crs = 4326, remove = FALSE)

# --- 2) Extensión conjunta para encuadrar el mapa ---
b1 <- st_bbox(reclamos_sf); b2 <- st_bbox(historico_ll)
lng1 <- min(b1["xmin"], b2["xmin"]); lat1 <- min(b1["ymin"], b2["ymin"])
lng2 <- max(b1["xmax"], b2["xmax"]); lat2 <- max(b1["ymax"], b2["ymax"])

# --- 3) Mapa ---
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Capa: Reclamos (rojo)
  addCircleMarkers(
    data = reclamos_sf,
    radius = 5, stroke = FALSE, fillOpacity = 0.8,
    color = "red",
    group = "Reclamos",
    popup = ~sprintf(
      "<b>Reclamo:</b> %s<br><b>Fecha:</b> %s<br><b>Dirección:</b> %s",
      incidente, as.character(fecha_de_reclamo), direccion
    )
  ) %>%
  
  # Capa: Histórico DFR (azul)
  addCircleMarkers(
    data = historico_ll,
    radius = 5, stroke = FALSE, fillOpacity = 0.8,
    color = "blue",
    group = "Histórico DFR",
    popup = ~sprintf(
      "<b>gid:</b> %s<br><b>Circuito:</b> %s<br><b>Posición:</b> %s",
      gid, Circuito, Posicion
    )
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Reclamos", "Histórico DFR"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright",
            colors = c("red","blue"),
            labels = c("Reclamos","Histórico DFR"),
            title = "Capas") %>%
  fitBounds(lng1, lat1, lng2, lat2)







#### probar cargar google sheets

install.packages("googlesheets4")   # 1a vez
library(googlesheets4)

# Si la hoja es pública:
gs4_deauth()
df <- read_sheet("https://docs.google.com/spreadsheets/d/1FguUTUmtHrbDJPOI6qav3e1IXiV5byH1uVv0DnkzMWk/edit?gid=1615275865#gid=1615275865")

# Si es privada:
gs4_auth()  # abre login en el navegador
df <- read_sheet("https://docs.google.com/spreadsheets/d/ID/edit#gid=0", sheet = 1)

# Elegir rango u hoja por nombre
df <- read_sheet("https://docs.google.com/spreadsheets/d/1FguUTUmtHrbDJPOI6qav3e1IXiV5byH1uVv0DnkzMWk/edit?gid=1615275865#gid=1615275865", sheet = "REGISTRO DE ZONA LIMPIA")




























# Paquetes
library(sf)
library(readr)
library(dplyr)
library(tools)

url <- "https://ckan-data.montevideo.gub.uy/dataset/c34e11ea-c547-46d5-80c6-d0bf283c002f/resource/ccb644b1-79b0-4caa-a353-6cfa816f3f70/download/reclamos.zip"

# 1) Descargar y descomprimir a temp
tmp_zip <- tempfile(fileext = ".zip")
dir_out <- tempfile(pattern = "zip_unpacked_")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
download.file(url, tmp_zip, mode = "wb", quiet = TRUE)
unzip(tmp_zip, exdir = dir_out)

# 2) Detectar qué hay adentro
files <- list.files(dir_out, recursive = TRUE, full.names = TRUE)

# 3) Si hay GPKG
gpkg <- files[grepl("\\.gpkg$", files, ignore.case = TRUE)]
if (length(gpkg) > 0) {
  # leer todas las capas
  capas <- st_layers(gpkg[1])$name
  g <- do.call(rbind, lapply(capas, function(nm) st_read(gpkg[1], layer = nm, quiet = TRUE)))
  g <- st_make_valid(g)
  print(g)
} else {
  # 4) Si hay SHP
  shp <- files[grepl("\\.shp$", files, ignore.case = TRUE)]
  if (length(shp) > 0) {
    g <- st_read(shp[1], quiet = TRUE)
    g <- st_make_valid(g)
    print(g)
  } else {
    # 5) Si hay CSV: intentar lon/lat comunes
    csv <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
    if (length(csv) > 0) {
      df <- suppressMessages(read_csv(csv[1], show_col_types = FALSE))
      # heurística de columnas
      lon_cols <- c("lon","long","longitude","x","coord_x","longitud")
      lat_cols <- c("lat","latitude","y","coord_y","latitud")
      lon <- intersect(tolower(names(df)), lon_cols)[1]
      lat <- intersect(tolower(names(df)), lat_cols)[1]
      if (!is.na(lon) && !is.na(lat)) {
        # usar nombres originales respetando mayúsculas
        lon <- names(df)[match(lon, tolower(names(df)))]
        lat <- names(df)[match(lat, tolower(names(df)))]
        g <- st_as_sf(df, coords = c(lon, lat), crs = 4326, remove = FALSE)
        print(g)
      } else {
        message("CSV sin columnas lon/lat reconocibles. Devuelvo data.frame.")
        print(df)
      }
    } else {
      stop("No se encontró GPKG, SHP ni CSV dentro del ZIP.")
    }
  }
}

# 6) Transformar a WGS84 si hace falta
if (exists("g") && inherits(g, "sf") && st_crs(g) != st_crs(4326)) {
  g <- st_transform(g, 4326)
}

# 7) Resultado en 'g' (sf) o 'df' (data.frame)
if (exists("g")) {
  cat("Registros:", nrow(g), "\nCRS:", st_crs(g)$input, "\n")
}
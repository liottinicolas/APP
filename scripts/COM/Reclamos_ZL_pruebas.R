### RECLAMOS ----
url <- "https://ckan-data.montevideo.gub.uy/dataset/c34e11ea-c547-46d5-80c6-d0bf283c002f/resource/ccb644b1-79b0-4caa-a353-6cfa816f3f70/download/reclamos.zip"

library(readr)

url <- "https://ckan-data.montevideo.gub.uy/dataset/45a29508-20d4-4adb-8b23-4f6e2cfec2be/resource/bbdbd017-bb79-4b17-b995-69077238ce54/download/solicitudes_al_com_2025.csv"

datos <- read_csv(url)

# listado_incidencias <- datos %>% 
#   distinct(repetido)


# install.packages("forcats")
# Cargar paquetes
library(dplyr)
library(ggplot2)
library(lubridate)  # para fechas
library(forcats)    # para trabajar con factores

# 1) Vista rápida de la estructura
glimpse(datos)

# 2) Limpieza básica y variables derivadas ------------------------

datos_limpio <- datos %>% 
  mutate(
    # Asegurar tipo de municipio en mayúsculas y sin espacios
    municipio = toupper(trimws(municipio)),
    
    # Convertir fechas: si ya son POSIXct/Date esto no rompe
    fecha_de_reclamo = as.POSIXct(fecha_de_reclamo),
    fecha_resuelto   = as.POSIXct(fecha_resuelto),
    
    # Crear fecha "solo día" por si querés agrupar sin hora
    fecha_reclamo_dia = as.Date(fecha_de_reclamo),
    fecha_resuelto_dia = as.Date(fecha_resuelto),
    
    # Tiempo de resolución en días (NA si no tiene fecha_resuelto)
    tiempo_resolucion_dias = as.numeric(fecha_resuelto_dia - fecha_reclamo_dia),
    
    # Repetido a factor legible (ajustar según cómo venga)
    repetido = case_when(
      repetido %in% c(1, "1", "SI", "Si", "sí", "S") ~ "Repetido",
      repetido %in% c(0, "0", "NO", "No", "N")       ~ "No repetido",
      TRUE                                           ~ "Desconocido"
    ),
    repetido = factor(repetido, levels = c("No repetido", "Repetido", "Desconocido"))
  )

datos_limpio <- datos_limpio %>% 
  mutate(
    # Diferencia en días (redondeo implícito a decimales de día)
    tiempo_resolucion_dias  = as.numeric(fecha_resuelto_dia - fecha_reclamo_dia),
    
    # Diferencia en horas (usa fecha y hora completas)
    tiempo_resolucion_horas = as.numeric(difftime(fecha_resuelto, fecha_de_reclamo, units = "hours"))
  )

# Chequear niveles de municipio
datos_limpio %>% 
  count(municipio, sort = TRUE)

# Chequear principales tipos de reclamo
datos_limpio %>% 
  count(incidente, sort = TRUE) %>% 
  head(20)

# 3) Indicadores básicos -----------------------------------------

# Reclamos por municipio
reclamos_por_muni <- datos_limpio %>% 
  count(municipio, name = "n_reclamos") %>% 
  arrange(desc(n_reclamos))

reclamos_por_muni

# Reclamos por circuito
reclamos_por_circuito <- datos_limpio %>% 
  count(circuito, name = "n_reclamos") %>% 
  arrange(desc(n_reclamos))

reclamos_por_circuito

# Reclamos por incidente
reclamos_por_incidente <- datos_limpio %>% 
  count(incidente, name = "n_reclamos") %>% 
  arrange(desc(n_reclamos))

reclamos_por_incidente

# 4) Tiempos de resolución ---------------------------------------

# Estadísticos globales de tiempo de resolución
resumen_tiempo_global <- datos_limpio %>% 
  summarise(
    # Cantidad de reclamos que tienen fecha de resuelto (o sea, están cerrados)
    n_con_resuelto = sum(!is.na(tiempo_resolucion_dias)),
    
    # Tiempo promedio de resolución en días (media aritmética)
    media_dias     = mean(tiempo_resolucion_dias, na.rm = TRUE),
    
    # Tiempo mediano de resolución en días (el "del medio": 50% se resuelven antes y 50% después)
    mediana_dias   = median(tiempo_resolucion_dias, na.rm = TRUE),
    
    # Percentil 90 en días: 90% de los reclamos se resuelven en este tiempo o menos, 10% demoran más
    p90_dias       = quantile(tiempo_resolucion_dias, 0.90, na.rm = TRUE),
    
    # Máximo tiempo de resolución en días (el caso más lento)
    max_dias       = max(tiempo_resolucion_dias, na.rm = TRUE),
    
    # Tiempo promedio de resolución en horas
    media_horas    = mean(tiempo_resolucion_horas, na.rm = TRUE),
    
    # Tiempo mediano de resolución en horas
    mediana_horas  = median(tiempo_resolucion_horas, na.rm = TRUE),
    
    # Percentil 90 en horas
    p90_horas      = quantile(tiempo_resolucion_horas, 0.90, na.rm = TRUE),
    
    # Máximo tiempo de resolución en horas
    max_horas      = max(tiempo_resolucion_horas, na.rm = TRUE)
  )

resumen_tiempo_global

# Tiempo de resolución por municipio
resumen_tiempo_muni <- datos_limpio %>% 
  group_by(municipio) %>% 
  summarise(
    n_reclamos          = n(),
    n_con_resuelto      = sum(!is.na(tiempo_resolucion_dias)),
    media_dias          = mean(tiempo_resolucion_dias, na.rm = TRUE),
    mediana_dias        = median(tiempo_resolucion_dias, na.rm = TRUE),
    p90_dias            = quantile(tiempo_resolucion_dias, 0.90, na.rm = TRUE)
  ) %>% 
  arrange(desc(media_dias))

resumen_tiempo_muni

# 5) Repetidos ---------------------------------------------------

resumen_repetidos_muni <- datos_limpio %>% 
  group_by(municipio) %>% 
  summarise(
    n_reclamos      = n(),
    n_repetidos     = sum(repetido == "Repetido", na.rm = TRUE),
    porcentaje_rep  = 100 * n_repetidos / n_reclamos
  ) %>% 
  arrange(desc(porcentaje_rep))

resumen_repetidos_muni

# 6) Visualizaciones ---------------------------------------------

# Reclamos por municipio (barra)
ggplot(reclamos_por_muni, aes(x = fct_reorder(municipio, n_reclamos),
                              y = n_reclamos)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Municipio",
    y = "Número de reclamos",
    title = "Reclamos por municipio"
  )

# Tiempo de resolución por municipio (boxplot)
ggplot(
  datos_limpio %>% filter(!is.na(tiempo_resolucion_dias)),
  aes(x = municipio, y = tiempo_resolucion_dias)
) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    x = "Municipio",
    y = "Tiempo de resolución (días)",
    title = "Distribución del tiempo de resolución por municipio"
  )

# Serie de tiempo: reclamos por día
reclamos_por_dia <- datos_limpio %>% 
  count(fecha_reclamo_dia, name = "n_reclamos") %>% 
  arrange(fecha_reclamo_dia)

ggplot(reclamos_por_dia, aes(x = fecha_reclamo_dia, y = n_reclamos)) +
  geom_line() +
  labs(
    x = "Fecha de reclamo",
    y = "Número de reclamos",
    title = "Evolución diaria de reclamos"
  )

library(readODS)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(openxlsx)



# -------------------------------------------------------------------------------------
# PARÁMETROS
# -------------------------------------------------------------------------------------

# Obtener el directorio donde estás parado
directorio_actual <- getwd()

# Definir el nombre de tu carpeta/subruta
RUTA_SNAPSHOT <- "scripts/informe operativa"

# Combinar ambos para obtener la ruta completa al archivo
ruta_archivo <- file.path(directorio_actual, RUTA_SNAPSHOT, "pruebaods.ods")

datos <- read_ods(ruta_archivo)

# Transformamos ambas columnas a formato fecha y hora
datos <- datos %>%
  mutate(across(c(`Fecha No Levante`, `Fecha Último Levante`), dmy_hm))

datos <- datos %>% 
  mutate(
    Oficina = ifelse(
      grepl("^B_0?[1-7](\\b|$)", Circuito),
      "Fideicomiso", "IM"
    ))

datos <- datos %>%
  filter(Oficina == "IM")

# Creamos la columna Municipio extrayendo las letras iniciales
datos <- datos %>%
  mutate(Municipio = sub("_.*", "", Circuito)) # Borra todo lo que esté después del "_"


MOTIVOS_EXCLUIR_ATRASO <- c(
  "No Está (20)","Roto (18)","Fuego (19)","Sobrepeso (11)",
  "Fuera de Alcance (21)","Tapa Bloqueda (14)",
  "Buzonera Girada (24)","Volcado (22)"
)

MOTIVOS_GRUA <- c(
  "Roto (18)",
  "Sobrepeso (11)",
  "Fuera de Alcance (21)",
  "Buzonera Girada (24)",
  "Cruzado(23)",
  "Calle Cerrada(13)"
)

MOTIVO_NO_ESTA <- "No Está (20)"
MOTIVO_FUEGO   <- "Fuego (19)"


# 2. Creamos la columna y la reubicamos
datos <- datos %>%
  mutate(Acumulacion_horas = Sys.time() - `Fecha Último Levante`) %>%
  relocate(Acumulacion_horas, .after = `Fecha Último Levante`) %>% 
  mutate(Acumulacion_dias = round(as.numeric(difftime(Sys.time(), 
                                                      `Fecha Último Levante`, 
                                                      units = "days")))) %>% 
  relocate(Acumulacion_dias, .after = Acumulacion_horas)

datos_mayora3dias <- datos %>% 
  filter(Acumulacion_horas >= 72) %>% 
  filter(is.na(`Fecha No Levante`))

# Creamos el nuevo dataframe excluyendo los motivos de la lista
datos_filtrados <- datos_mayora3dias %>%
  filter(!(`Motivo No Levante` %in% names(MOTIVOS_EXCLUIR_ATRASO)))

# 1. Crear la columna Municipio extrayendo las letras antes del "_"
# 2. Categorizar los días de acumulación
datos_mayora3dias <- datos_mayora3dias %>%
  mutate(
    Municipio = sub("_.*", "", Circuito),
    Categoria_Atraso = case_when(
      Acumulacion_dias >= 6 ~ ">=6",
      Acumulacion_dias == 5 ~ "5",
      Acumulacion_dias == 4 ~ "4",
      Acumulacion_dias == 3 ~ "3",
      TRUE ~ "Otros"
    )
  )

# 3. Crear la tabla resumen (Pivoteo)
resumen_atrasos <- datos_mayora3dias %>%
  filter(Categoria_Atraso != "Otros") %>% # Solo nos interesan las columnas de la imagen
  group_by(Municipio, Categoria_Atraso) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Categoria_Atraso, values_from = conteo, values_fill = 0)

# 4. Ordenar columnas, calcular total por fila y agregar fila de TOTAL general
tabla_final <- resumen_atrasos %>%
  # Seleccionamos y ordenamos las columnas según tu imagen
  select(Municipio, any_of(c(">=6", "5", "4", "3"))) %>%
  # Sumamos horizontalmente
  mutate(total = rowSums(select(., -Municipio))) %>%
  # Agregamos la fila de totales al final
  bind_rows(
    summarise(., 
              Municipio = "TOTAL", 
              across(where(is.numeric), sum))
  )

# Visualizar el resultado
print(tabla_final)


## Tabla 1 pronto.

compactar_tramos <- function(x) {
  if (length(x) == 0) return("")
  x <- sort(unique(as.numeric(x)))
  if (length(x) == 1) return(as.character(x))
  
  # Lógica para encontrar saltos en la secuencia
  df <- data.frame(val = x) %>%
    mutate(diff = c(1, diff(val)),
           group = cumsum(diff != 1)) %>%
    group_by(group) %>%
    summarise(range = if(n() > 1) paste0(min(val), "-", max(val)) else as.character(min(val)))
  
  paste(df$range, collapse = ", ")
}


# Usamos tu objeto datos_mayora3dias (asegúrate de que tenga la columna Municipio que creamos)
detalle_at <- datos_mayora3dias %>%
  # 1. Creamos la categoría de atraso para agrupar
  mutate(atraso = case_when(
    Acumulacion_dias >= 6 ~ ">=6",
    Acumulacion_dias == 5 ~ "5",
    Acumulacion_dias == 4 ~ "4",
    Acumulacion_dias == 3 ~ "3",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(atraso)) %>%
  # 2. Agrupamos por los campos de la imagen
  group_by(Municipio, atraso, Circuito) %>%
  summarise(
    cantidad = n(),
    # Aquí usamos la función que definimos arriba
    tramos_posicion = compactar_tramos(Posición), 
    # La fecha de levante (formateada como la imagen 26/01/26)
    fecha_levante = format(min(`Fecha Último Levante`, na.rm = TRUE), "%d/%m/%y"),
    .groups = "drop"
  ) %>%
  # 3. Ordenamos igual que el reporte
  mutate(atraso_fact = factor(atraso, levels = c(">=6", "5", "4", "3"))) %>%
  arrange(Municipio, atraso_fact, desc(cantidad)) %>%
  select(-atraso_fact) # Quitamos el auxiliar de orden








no_esta <- datos %>% filter(`Motivo No Levante` == MOTIVO_NO_ESTA)

resumen_no_esta <- no_esta %>%
  group_by(Circuito) %>%
  summarise(
    cantidad = n(),
    tramos_posicion = compactar_tramos(Posición),
    .groups = "drop"
  ) %>%
  arrange(desc(cantidad))









grua_data <- datos %>% filter(`Motivo No Levante` %in% MOTIVOS_GRUA)

resumen_grua <- grua_data %>%
  group_by(Circuito) %>%
  summarise(
    suma_una = sum(`% de acumulación (UNA)`, na.rm = TRUE),
    cantidad = n(),
    tramos_posicion = compactar_tramos(Posición),
    fecha_ult_levante_mas_vieja = as.Date(min(`Fecha Último Levante`, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(suma_una))

# Detalle individual para la operativa de grúa
listado_grua_detalle <- grua_data %>%
  select(Circuito, Posición, `Fecha Último Levante`) %>%
  mutate(fecha_ult_levante = as.Date(`Fecha Último Levante`)) %>%
  arrange(fecha_ult_levante, Circuito, Posición)
# Paquetes
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringi)

source("scripts/para_mapear/circuitos_planificados.R")

# ---------- 1) Plan maestro: días por circuito ----------
# Un registro por circuito, con vector de días (minúsculas, es-ES)
plan_circuitos <- tibble::tibble(
  CIRCUITO  = c("A_101","B_203"),
  MUNICIPIO = c("A","B"),
  dias      = list(
    c("lunes","miércoles","viernes"),
    c("martes","jueves","sábado")
  )
)

# ---------- 2) Generar calendario según plan ----------
# desde/hasta = rango de fechas que querés cubrir
desde <- dmy("01/08/2025")
hasta <- dmy("30/09/2025")

# helper: orden y normalización de nombres de día
orden_dias <- c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
nom_dia <- function(fecha) {
  # weekdays() respeta locale; normalizamos a minúsculas sin tildes raras
  stri_trans_tolower(weekdays(fecha))
}

# Todas las fechas del rango
cal_fechas <- tibble::tibble(DIA = seq.Date(desde, hasta, by = "day")) %>%
  mutate(NOMBREDIA = nom_dia(DIA)) %>%
  filter(NOMBREDIA %in% orden_dias)

# Explota plan (una fila por día de servicio)
plan_expl <- plan_circuitos %>%
  tidyr::unnest_longer(dias, values_to = "NOMBREDIA") %>%
  mutate(NOMBREDIA = factor(NOMBREDIA, levels = orden_dias, ordered = TRUE))

# Calendario esperado: solo fechas cuyo día coincide con el plan
calendario <- plan_expl %>%
  inner_join(cal_fechas, by = "NOMBREDIA") %>%
  arrange(CIRCUITO, DIA) %>%
  group_by(CIRCUITO) %>%
  mutate(
    dia0          = min(DIA),
    dia_ciclo     = as.integer(DIA - dia0),
    dia_ciclo_sem = dia_ciclo %% 7
  ) %>%
  ungroup() %>%
  select(DIA, NOMBREDIA, MUNICIPIO, CIRCUITO, dia_ciclo, dia_ciclo_sem)

# Frecuencia y período teóricos por circuito (constantes dado el plan)
freq_resumen <- plan_expl %>%
  count(CIRCUITO, name = "frecuencia_sem") %>%
  mutate(Periodo = 7 / frecuencia_sem)

# ---------- 3) Inyectar conteos operativos cuando existan ----------
# Ejemplo de “conteos” que vas a ir actualizando día a día:
# columnas mínimas: DIA, CIRCUITO, (opcional: ID_TURNO, TURNO), ACTIVO, INACTIVO
conteos <- tibble::tibble(
  DIA = as.Date(c("2025-08-18","2025-08-20","2025-08-22")),
  CIRCUITO = "A_101",
  ID_TURNO = 1L,
  TURNO    = "Matutino (06 a 14 hrs.)",
  ACTIVO   = c(30,30,30),
  INACTIVO = c(0,0,0)
)

# Tabla final estilo “la tuya”
tabla_final <- calendario %>%
  left_join(freq_resumen, by = "CIRCUITO") %>%
  left_join(conteos, by = c("DIA","CIRCUITO")) %>%
  mutate(
    TOTAL = ACTIVO %>% replace_na(0) + INACTIVO %>% replace_na(0)
  ) %>%
  transmute(
    DIA,
    NOMBREDIA = as.character(NOMBREDIA),
    Frecuencia = frecuencia_sem,
    Periodo,
    ID_TURNO, TURNO,
    MUNICIPIO, CIRCUITO,
    ACTIVO = ACTIVO %>% replace_na(0),
    INACTIVO = INACTIVO %>% replace_na(0),
    TOTAL
  ) %>%
  arrange(CIRCUITO, DIA)

# ---------- 4) Alta de un circuito nuevo ----------
# Solo agregalo al plan y re-ejecutá desde "plan_expl":
# plan_circuitos <- add_row(plan_circuitos,
#   CIRCUITO="C_777", MUNICIPIO="C", dias=list(c("lunes","jueves"))
# )
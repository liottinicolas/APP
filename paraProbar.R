
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

fecha_inicio_fueradelugar <- inicio
fecha_fin_fueradelugar <- fin
df_incidencias_completo <- probando_incidencias


probando_incidencias <- historico_completo_llenado_incidencias
inicio <- as.Date("2025-05-01")
fin <- as.Date("2025-06-12")



salida <- filtrar_contenedores_malubicados(inicio, fin, probando_incidencias)

# Accedes así:
historico_malubicados <- salida$historico
total_malubicados   <- salida$total_veces





tryCatch({
  
  ##--- 1. Crear el libro --------
  wb <- createWorkbook()
  
  ##--- 2. Hoja «Resumen» (== resultado) ----------
  addWorksheet(wb, "Resumen")
  
  writeDataTable(
    wb, sheet = "Resumen",
    x = total_malubicados,                       # <-- tu data frame resumido
    tableStyle = "TableStyleLight9"
  )
  
  setColWidths(
    wb, sheet = "Resumen",
    cols  = 1:ncol(resultado),
    widths = "auto"
  )
  
  ## Estilo de fechas (reutilizable)
  dateStyle <- createStyle(numFmt = "dd/mm/yyyy")
  
  # Aplica el estilo si la columna «Fecha» existe en este df
  if ("Fecha" %in% names(total_malubicados)) {
    addStyle(
      wb, sheet = "Resumen", style = dateStyle,
      cols = which(names(total_malubicados) == "Fecha"),
      rows = 2:(nrow(total_malubicados) + 1),
      gridExpand = TRUE
    )
  }
  
  ##--- 3. Hoja «Filtrado» (== retorno) ----------
  addWorksheet(wb, "Filtrado")
  
  writeDataTable(
    wb, sheet = "Filtrado",
    x = historico_malubicados,                         # <-- filas filtradas originales
    tableStyle = "TableStyleLight9"
  )
  
  setColWidths(
    wb, sheet = "Filtrado",
    cols  = 1:ncol(historico_malubicados),
    widths = "auto"
  )
  
  # Aplica el estilo de fecha también aquí (si corresponde)
  if ("Fecha" %in% names(historico_malubicados)) {
    addStyle(
      wb, sheet = "Filtrado", style = dateStyle,
      cols = which(names(historico_malubicados) == "Fecha"),
      rows = 2:(nrow(historico_malubicados) + 1),
      gridExpand = TRUE
    )
  }
  
  ##--- 4. Guardar en disco ----------
  nombre_archivo <- file.path(
    CONFIGURACION$DIRECTORIO_SALIDA,
    paste0("contenedores_fuera_de_lugar_entre ",inicio," y ",fin, ".xlsx")
  )
  
  saveWorkbook(wb, file = nombre_archivo, overwrite = TRUE)
  
  escribir_log("INFO", paste("Archivo generado:", nombre_archivo))
  
}, error = function(e) {
  manejar_error(e, paste("Error exportando el excel de los mal ubicados"))
})



adrian <- historico_estado_diario %>%
  filter(Municipio == "A") %>% 
  filter(Fecha >= "2025-07-21") %>% 
  filter(Fecha <= "2025-07-27") %>% 
  select(Fecha,gid,Circuito_corto,Posicion,Estado,Direccion,Acumulacion)

adrian[is.na(adrian)] <- ""

write.csv(adrian, "municipio_a_julio.csv", row.names = FALSE, na = "")













library(dplyr)
library(tidyr)
library(writexl)

# 📅 Fecha objetivo
fecha_objetivo <- as.Date("2025-08-05")

# 🧹 Filtrar histórico por la fecha
df_filtrado <- web_historico_ubicaciones %>%
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







# CONTAR CONTENEDORES ROTOS ---- 

asd1 <- historico_incidencias_completas %>% 
  filter((Fecha> "2025-01-01") & (Fecha<"2025-06-30"))%>% 
  filter(Id_incidencia == 18)

rotos_llenado <- historico_llenado %>% 
  filter((Fecha> "2025-01-01") & (Fecha<"2025-06-30")) %>% 
  filter(str_detect(Condicion, "Requiere Mantenimiento")) %>%  
  distinct(gid, .keep_all = TRUE)

df_combinado <- inner_join(
  asd1,
  rotos_llenado,
  by = c("Fecha", "Circuito", "Posicion", "Id_viaje")
)
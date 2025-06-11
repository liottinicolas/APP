# Cargar librerías necesarias
library(openxlsx)
library(dplyr)

# Cargar el ranking actual
source("scripts/para_mapear/Ranking.R")

# Ordenar el ranking por UNA
ranking_ordenado <- rankgin %>%
  arrange(desc(UNA)) %>%
  select(circuito_corto, UNA)

# Cargar el archivo Excel existente
wb <- loadWorkbook("scripts/para_mapear/Ranking_base2.xlsx")

# Escribir el ranking ordenado en la hoja 1 a partir de la celda E18
writeData(
  wb,
  sheet = 1,
  x = ranking_ordenado,
  startCol = 5,  # Columna E
  startRow = 18,
  colNames = FALSE  # No escribir nombres de columnas
)

# Guardar el archivo Excel
saveWorkbook(wb, "scripts/para_mapear/Ranking_base2.xlsx", overwrite = TRUE)

# Mensaje de confirmación
cat("Ranking actualizado exitosamente en Ranking_base2.xlsx\n") 
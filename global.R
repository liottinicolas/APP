# global.R

# 1. Lista de paquetes necesarios
paquetes_necesarios <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "htmlwidgets",
  "dplyr", "tidyr", "purrr", "readr", "stringr", "stringi", "lubridate",
  "ggplot2", "plotly", "leaflet", "leaflet.extras", "sf",
  "knitr", "rmarkdown", "openxlsx", "writexl", "readxl",
  "rsconnect", "here", "R6", "tools", "magrittr"
)

# 2. Verificar localmente que todos estén instalados
faltantes <- setdiff(paquetes_necesarios, installed.packages()[, "Package"])
if (length(faltantes) > 0) {
  install.packages(faltantes, repos = options("repos")$repos)
}
# Cargarlos
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

# 3. Configuración de rutas y proyecto
library(here)        # para rutas portables
ruta_proyecto <- here()

# 4. Zona horaria y opciones globales
Sys.setenv(TZ = "America/Montevideo")
options(
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# 5. Mensaje de arranque (opcional, útil en logs)
message("Entorno global cargado. Proyecto en: ", ruta_proyecto)

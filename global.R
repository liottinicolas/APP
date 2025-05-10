# Lista de paquetes necesarios
paquetes_necesarios <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "htmlwidgets", "dplyr",
  "tidyr", "purrr", "readr", "stringr", "lubridate", "ggplot2", "plotly",
  "leaflet", "leaflet.extras", "sf", "knitr", "rmarkdown", "openxlsx",
  "writexl", "readxl", "rsconnect", "here", "R6", "tools", "magrittr"
)

# Función para instalar y cargar paquetes
instalar_y_cargar <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
  library(paquete, character.only = TRUE)
}

# Aplicar la función a todos los paquetes
invisible(lapply(paquetes_necesarios, instalar_y_cargar))


# Configuración del entorno
ruta_proyecto <- here()
Sys.setenv(TZ = "America/Montevideo")

# Configuración adicional del entorno
options(
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)



# Función para verificar e instalar paquetes
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      message(sprintf("Instalando el paquete %s...", package))
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# Paquetes necesarios agrupados por funcionalidad
packages <- list(
  # Paquetes base para Shiny
  shiny = c("shiny", "shinydashboard", "shinyWidgets", "DT", "htmlwidgets"),
  
  # Paquetes para manipulación de datos
  data_manipulation = c("dplyr", "tidyr", "purrr", "readr", "stringr"),
  
  # Paquetes para fechas y tiempo
  date_time = c("lubridate"),
  
  # Paquetes para visualización
  visualization = c("ggplot2", "plotly"),
  
  # Paquetes para mapas
  mapping = c("leaflet", "leaflet.extras", "sf"),
  
  # Paquetes para documentos y reportes
  documents = c("knitr", "rmarkdown"),
  
  # Paquetes para archivos Excel
  excel = c("openxlsx", "writexl", "readxl"),
  
  # Paquetes para despliegue y utilidades
  utilities = c("rsconnect", "here", "R6", "tools")
)

# Instalar y cargar todos los paquetes
tryCatch({
  # Aplanar la lista de paquetes
  all_packages <- unlist(packages)
  
  # Verificar e instalar paquetes
  check_and_install_packages(all_packages)
  
  message("Todos los paquetes se han cargado correctamente.")
}, error = function(e) {
  message(sprintf("Error al cargar los paquetes: %s", e$message))
  stop(e)
})

# Configuración del entorno
ruta_proyecto <- here()
Sys.setenv(TZ = "America/Montevideo")

# Configuración adicional del entorno si es necesaria
options(
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# Cargar paquetes necesarios
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(htmlwidgets)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(sf)
library(knitr)
library(rmarkdown)
library(openxlsx)
library(writexl)
library(readxl)
library(rsconnect)
library(here)
library(R6)
library(tools)

# Configuración del entorno
ruta_proyecto <- here()
Sys.setenv(TZ = "America/Montevideo")

# Configuración adicional del entorno
options(
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)



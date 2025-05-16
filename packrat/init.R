source("packrat/init.R")

# Configuración del repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Lista de paquetes necesarios
packages <- c(
  "shiny",
  "shinydashboard",
  "stringi",
  "dplyr",
  "tidyr",
  "ggplot2",
  "DT",
  "lubridate"
)

# Función para instalar paquetes si no están instalados
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    if (!require("utils", character.only = TRUE)) {
      stop("El paquete 'utils' no está disponible")
    }
    utils::install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Instalar paquetes faltantes
sapply(packages, install_if_missing) 
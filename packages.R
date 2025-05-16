# Configuración del repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Lista de paquetes necesarios con sus dependencias
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

# Función para instalar y cargar paquetes
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    # Intentar instalar desde binarios primero
    tryCatch({
      message(sprintf("Intentando instalar %s desde binarios...", package))
      install.packages(package, type = "binary", quiet = TRUE)
    }, error = function(e) {
      message(sprintf("Error al instalar %s desde binarios, intentando desde fuente...", package))
      # Si falla, intentar desde fuente
      tryCatch({
        install.packages(package, type = "source", quiet = TRUE)
      }, error = function(e2) {
        message(sprintf("Error al instalar %s: %s", package, e2$message))
        stop(sprintf("No se pudo instalar el paquete %s", package))
      })
    })
    
    # Intentar cargar el paquete
    tryCatch({
      library(package, character.only = TRUE)
      message(sprintf("Paquete %s cargado exitosamente", package))
    }, error = function(e) {
      stop(sprintf("No se pudo cargar el paquete %s: %s", package, e$message))
    })
  }
}

# Instalar y cargar todos los paquetes
for (pkg in packages) {
  tryCatch({
    install_and_load(pkg)
  }, error = function(e) {
    message(sprintf("Error con el paquete %s: %s", pkg, e$message))
  })
} 
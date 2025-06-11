# Función para forzar la reinstalación de htmltools
forzar_reinstalacion_htmltools <- function() {
  # Desinstalar htmltools si está cargado
  if("htmltools" %in% .packages()) {
    detach("package:htmltools", unload = TRUE, force = TRUE)
  }
  
  # Forzar la desinstalación
  try(remove.packages("htmltools"), silent = TRUE)
  
  # Limpiar la caché de paquetes
  unlink(list.files(tempdir(), pattern = "downloaded_packages", full.names = TRUE), recursive = TRUE)
  
  # Instalar la versión específica de htmltools
  install.packages("htmltools", repos = "https://cloud.r-project.org", dependencies = TRUE)
}

# Lista de paquetes necesarios en orden de dependencia
paquetes <- c(
  "htmltools",    # Primero htmltools
  "shiny",        # Luego shiny
  "shinydashboard",
  "leaflet",
  "DT",
  "dplyr",
  "sf",
  "magrittr"
)



# Función para instalar/actualizar paquetes
instalar_actualizar_paquetes <- function(paquetes) {
  # Primero forzar la reinstalación de htmltools
  forzar_reinstalacion_htmltools()
  
  # Desinstalar todos los paquetes primero
  for(pkg in rev(paquetes)) {
    if(pkg %in% .packages()) {
      detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
    }
    try(remove.packages(pkg), silent = TRUE)
  }
  
  # Instalar paquetes en orden
  for(pkg in paquetes) {
    cat(sprintf("Instalando %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    if(!require(pkg, character.only = TRUE)) {
      stop(sprintf("Error al instalar %s", pkg))
    }
  }
}

# Ejecutar la instalación/actualización
instalar_actualizar_paquetes(paquetes)

# Verificar las versiones instaladas
for(pkg in paquetes) {
  cat(sprintf("Versión de %s: %s\n", pkg, packageVersion(pkg)))
} 
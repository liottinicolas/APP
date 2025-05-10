# Cargar paquetes necesarios
library(sf)
library(dplyr)

# Función que modifica como está escrito "the_geom".
modificar_the_geom <- function(df) {
  # Verificamos si la columna 'the_geom' existe en el dataframe
  if(!"the_geom" %in% colnames(df)) {
    stop("La columna 'the_geom' no está presente en el dataframe.")
  }
  
  # Modificar la columna 'the_geom' eliminando el espacio después de 'POINT(' y la coma entre las coordenadas
  df <- df %>%
    mutate(the_geom = gsub("POINT \\(\\s+", "POINT (", gsub(",\\s*", " ", the_geom)))
  
  return(df)
}


############################# Funciones para web ###############

### MAPA ####
# Crear una función para extraer las coordenadas de POINT (x y)
extract_coords <- function(point_str) {
  # Remover 'POINT (' y ')' del string
  coords <- gsub("POINT \\(|\\)", "", point_str)
  # Separar las coordenadas en x e y
  coords <- as.numeric(unlist(strsplit(coords, " ")))
  return(coords)
}

modificar_coordenadas_paramapa <- function(df_ingreso){
  df_retorno <- df_ingreso %>%
    rowwise() %>%
    mutate(
      x = extract_coords(the_geom)[1],
      y = extract_coords(the_geom)[2]
    ) %>%
    ungroup()
  
  # Crear un objeto sf con las coordenadas UTM (ajustar CRS según tu zona)
  df_sf <- st_as_sf(df_retorno, coords = c("x", "y"), crs = 32721)  # EPSG 32721 para UTM Zona 21S
  
  # Convertir las coordenadas a WGS84 (Latitud, Longitud)
  df_sf_wgs84 <- st_transform(df_sf, crs = 4326)
  
  # Extraer las coordenadas transformadas (Lat, Long)
  df_retorno <- df_retorno %>%
    mutate(
      lon = st_coordinates(df_sf_wgs84)[, 1],
      lat = st_coordinates(df_sf_wgs84)[, 2]
    )
  
  return(df_retorno)
}

cargar_opciones_responsable <- function(nombre_responsable){
  df_incidencias <- data.frame(
    motivo_no_levante = 0:26,
    descripcion = c(
      "Levantado",
      "Rotura con retorno a circuito",
      "Rotura sin retorno a circuito",
      "Horas permiso auxiliar",
      "Horas permiso chofer",
      "Horas permiso aux y chof",
      "Medidas gremiales",
      "Habilitado tarde (Mantenimiento)",
      "Demora en cantera",
      "Camion a Lavadero",
      "No Levantado por Feria",
      "Sobrepeso",
      "Auto",
      "Calle Cerrada",
      "Tapa Bloqueda",
      "Persona en el Interior del Cont.",
      "Capacidad del Camion y/o Tiempo",
      "Sin ticket de cantera",
      "Contenedor Roto (choque, desfonde, etc.)",
      "Fuego",
      "Contenedor No Está",
      "Contenedor Fuera de Alcance",
      "Contenedor Volcado",
      "Contenedor Cruzado",
      "Buzonera Girada",
      "Otros",
      "Viaje suspendido"
    ),
    accion_requerida = c("Sin_accion",        # Levantado
                         "Levantar",          # Rotura con retorno a circuito
                         "Levantar",          # Rotura sin retorno a circuito
                         "Levantar",          # Horas permiso auxiliar
                         "Levantar",          # Horas permiso chofer
                         "Levantar",          # Horas permiso aux y chof
                         "Levantar",          # Medidas gremiales
                         "Levantar",          # Habilitado tarde (Mantenimiento)
                         "Levantar",          # Demora en cantera
                         "Levantar",          # Camion a Lavadero
                         "Levantar",          # No Levantado por Feria 
                         "Requiere_accion",   # Sobrepeso                  # Grua
                         "Levantar",          # Auto                       # Auto, hacer planilla x mes para ver si repite
                         "Requiere_accion",   # Calle Cerrada
                         "Requiere_accion",   # Tapa Bloqueda               # mantenimiento? correo a
                         "Levantar",          # Persona en el Interior del Cont.
                         "Levantar",          # Capacidad del Camion y/o Tiempo
                         "Levantar",          # Sin ticket de cantera
                         "Requiere_accion",   # Contenedor Roto (choque, desfonde, etc.)  # mantenimiento y operativa
                         "Levantar",   # Fuego
                         "Requiere_accion",   # Contenedor No Está                  # SPP
                         "Requiere_accion",   # Contenedor Fuera de Alcance              # PLUMA
                         "Requiere_accion",   # Contenedor Volcado                  # PLUMA
                         "Requiere_accion",   # Contenedor Cruzado                  # PLUMA
                         "Requiere_accion",   # Buzonera Girada                     # PLUMA
                         "Requiere_accion",   # Otros
                         "Levantar"           # Viaje suspendido
    ),
    responsable = c(
      "Sin responsable",        # Levantado
      "Operativa",          # Rotura con retorno a circuito
      "Operativa",          # Rotura sin retorno a circuito
      "Operativa",          # Horas permiso auxiliar
      "Operativa",          # Horas permiso chofer
      "Operativa",          # Horas permiso aux y chof
      "Operativa",          # Medidas gremiales
      "Operativa",          # Habilitado tarde (Mantenimiento)
      "Operativa",          # Demora en cantera
      "Operativa",          # Camion a Lavadero
      "Operativa",          # No Levantado por Feria 
      "Grua",   # Sobrepeso                  # Grua
      "Seguimiento SPP",          # Auto                       # Auto, hacer planilla x mes para ver si repite
      "Seguimiento SPP",   # Calle Cerrada
      "Mantenimiento",   # Tapa Bloqueda               # mantenimiento? correo a
      "Operativa",          # Persona en el Interior del Cont.
      "Operativa",          # Capacidad del Camion y/o Tiempo
      "Operativa",          # Sin ticket de cantera
      "Mantenimiento",   # Contenedor Roto (choque, desfonde, etc.)  # mantenimiento y operativa
      "Operativa",   # Fuego
      "Seguimiento SPP",   # Contenedor No Está                  # SPP
      "Pluma",   # Contenedor Fuera de Alcance              # PLUMA
      "Pluma",   # Contenedor Volcado                  # PLUMA
      "Pluma",   # Contenedor Cruzado                  # PLUMA
      "Pluma",   # Buzonera Girada                     # PLUMA
      "Seguimiento SPP",   # Otros
      "Operativa"           # Viaje suspendido
    )
  )
  
  df_incidencias <- df_incidencias %>% 
    filter(responsable == nombre_responsable) %>% 
    pull(descripcion)
  
  return (df_incidencias)
}


### Funciones para mapa - centrar en montevideo

# Crear un punto en UTM (EPSG:32721) para el centro de Montevideo
montevideo_utm <- st_sfc(st_point(c(570955.05392504, 6144038.03601591)), crs = 32721)

# Convertir a latitud y longitud (EPSG:4326)
montevideo_wgs84 <- st_transform(montevideo_utm, crs = 4326)

# Extraer las coordenadas lat/lon de Montevideo
coords_montevideo <- st_coordinates(montevideo_wgs84)
lat_montevideo <- coords_montevideo[2]
lng_montevideo <- coords_montevideo[1]

opciones <- cargar_opciones_responsable("Pluma")
opciones_filtradas_grua <- opciones[opciones != "Contenedor Volcado"]



cargar_opciones_condiciones <- function(){
  df_condiciones <- data.frame(
    id_condicion = 0:7,
    descripcion_condicion = c(
      "Todos",
      "Basura afuera",
      "Dos ciclos",
      "Escombro",
      "Fuera de lugar",
      "Poda",
      "Requiere limpieza",
      "Requiere Mantenimiento"
    )
  ) %>% 
    # Crear un named vector para los choices
    pull(descripcion_condicion)
  
  return(df_condiciones)
  
}
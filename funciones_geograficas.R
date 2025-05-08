# Funciones para manipulación de coordenadas y geometrías

#' Modifica el formato de la columna the_geom
#' @param df DataFrame que contiene la columna the_geom
#' @return DataFrame con la columna the_geom modificada
modificar_the_geom <- function(df) {
  if(!"the_geom" %in% colnames(df)) {
    stop("La columna 'the_geom' no está presente en el dataframe.")
  }
  
  df <- df %>%
    mutate(the_geom = gsub("POINT \\(\\s+", "POINT (", gsub(",\\s*", " ", the_geom)))
  
  return(df)
}

#' Extrae coordenadas de un string POINT
#' @param point_str String en formato POINT (x y)
#' @return Vector numérico con las coordenadas [x, y]
extract_coords <- function(point_str) {
  coords <- gsub("POINT \\(|\\)", "", point_str)
  coords <- as.numeric(unlist(strsplit(coords, " ")))
  return(coords)
}

#' Modifica coordenadas para visualización en mapa
#' @param df_ingreso DataFrame con coordenadas UTM
#' @return DataFrame con coordenadas WGS84 (lat/lon)
modificar_coordenadas_paramapa <- function(df_ingreso){
  df_retorno <- df_ingreso %>%
    rowwise() %>%
    mutate(
      x = extract_coords(the_geom)[1],
      y = extract_coords(the_geom)[2]
    ) %>%
    ungroup()
  
  df_sf <- st_as_sf(df_retorno, coords = c("x", "y"), crs = 32721)
  df_sf_wgs84 <- st_transform(df_sf, crs = 4326)
  
  df_retorno <- df_retorno %>%
    mutate(
      lon = st_coordinates(df_sf_wgs84)[, 1],
      lat = st_coordinates(df_sf_wgs84)[, 2]
    )
  
  return(df_retorno)
}

# Coordenadas de Montevideo para centrado del mapa
montevideo_utm <- st_sfc(st_point(c(570955.05392504, 6144038.03601591)), crs = 32721)
montevideo_wgs84 <- st_transform(montevideo_utm, crs = 4326)
coords_montevideo <- st_coordinates(montevideo_wgs84)
lat_montevideo <- coords_montevideo[2]
lng_montevideo <- coords_montevideo[1] 

source("scripts/db/10338_incidencias/funciones_incidencias.R")

# Especificar la ruta a la carpeta que contiene los libros de Excel
ruta_carpeta_incidencias <- paste0(ruta_proyecto,"/archivos/base/10338_incidencias")

# Definir la ruta donde se guardará el archivo .rds del histórico
archivords_historico_incidencias <- paste0(ruta_proyecto, "/scripts/db/10338_incidencias/historico_incidencias.rds")
# Definir la ruta donde se guardará la lista de archivos procesados

# Es una lista de CSV que se procesó, para no volver a repetir la información a cargar.
ruta_archivords_historico_incidencias <- paste0(ruta_proyecto, "/scripts/db/10338_incidencias/archivos_aplicados_historico_incidencias.rds")

# Cargar la lista de archivos procesados previamente, si existe
archivos_procesados <- if (file.exists(ruta_archivords_historico_incidencias)) {
  # Si el archivo existe, se carga la lista de archivos procesados
  readRDS(ruta_archivords_historico_incidencias)
} else {
  # Si no existe, inicializamos con una lista vacía
  character(0)
}

# Obtener la lista de archivos de csv en la carpeta
lista_archivos_incidencias <- dir(ruta_carpeta_incidencias, pattern = "*.csv")

# Filtrar solo los archivos que aún no han sido procesados
# Busca del listado de todos los csv de origen, aquellos que fueron guardados, y solo procesa los nuevos.
archivos_nuevos <- setdiff(lista_archivos_incidencias, archivos_procesados)

if (length(archivos_nuevos) > 0){ 
  # Cargar cada libro de Excel usando map() y procesar el data frame
  lista_data_frames_incidencias <- map(archivos_nuevos, ~ {
    
    # Leo cada archivo -                                                                                        # El locale es para transformar las , por . que R los entiende como decimales.
    tabla_actual_incidencias <- read_delim(paste0(ruta_carpeta_incidencias, "/", .x),
                                           delim = "\t",
                                           escape_double = FALSE,
                                           trim_ws = TRUE,
                                           locale = locale(encoding = "ISO-8859-1"))
    
    return(tabla_actual_incidencias)
  })
  
  # Unir todos los data frames en uno solo
  historico_incidencias <- bind_rows(!!!lista_data_frames_incidencias)
  
  historico_incidencias <- historico_incidencias %>% 
    filter(Posiciones != "VIAJE")
  
  incidencias_del_dia <- historico_incidencias
  
  
  # Separar la columna 'rango' en 'inicio' y 'fin'
  incidencias_del_dia <- incidencias_del_dia %>% 
    separate(Posiciones, into = c("inicio", "fin"), sep = " - ")
  
  
  # Crear nuevas filas para cada posición desde inicio hasta fin
  incidencias_del_dia_expandido <- incidencias_del_dia %>%
    rowwise() %>%
    mutate(posicion = list(seq(inicio, fin))) %>% # Crear la lista de posiciones
    unnest(posicion) %>%  # Expandir la lista en filas
    select(Circuito, Dia, id_viaje, posicion, cod_inci, Descripcion)  # Seleccionar las columnas deseadas
  
  # Agrego el nombre del contenedor
  incidencias_del_dia_expandido <- incidencias_del_dia_expandido %>%
    mutate(nombre_contenedor = case_when(
      posicion < 10 ~ paste0(Circuito,"0",posicion),
      posicion >= 10 ~ paste0(Circuito,posicion))
    )
  
  incidencias_del_dia_expandido$Dia <- as.Date(incidencias_del_dia_expandido$Dia, format = "%d/%m/%Y")
  
  historico_informe_diario_disminuido <- historico_estado_diario_con_direccion %>% 
    select(Dia_informe,gid,nombre_contenedor) %>% 
    mutate(Dia = Dia_informe - 1)  # Crear la columna de fecha ajustada
  
  # Realizar el join para agregar `gid` a `historico_incidencias`
  incidencias_del_dia_expandido <- incidencias_del_dia_expandido %>%
    left_join(historico_informe_diario_disminuido %>% select(Dia, nombre_contenedor, gid),
              by = c("Dia" = "Dia", "nombre_contenedor" = "nombre_contenedor"))
  
  # Cargar el histórico anterior si existe, de lo contrario usar solo los datos nuevos
  historico_incidencias <- if (file.exists(archivords_historico_incidencias)) {
    # Cargar el histórico previamente guardado
    readRDS(archivords_historico_incidencias)
  } else {
    # Si no existe un histórico previo, el histórico será solo los nuevos datos
    incidencias_del_dia_expandido
  }
  
  # Actualizar el histórico con los datos nuevos
  historico_incidencias <- bind_rows(historico_incidencias, incidencias_del_dia_expandido)
  historico_incidencias <- historico_incidencias %>% 
    distinct()
  
  # Guardar el histórico actualizado en el archivo RDS
  saveRDS(historico_incidencias, file = archivords_historico_incidencias)
  
  # Actualizar la lista de archivos procesados, añadiendo los nuevos
  archivos_procesados  <- c(archivos_procesados , archivos_nuevos)
  
  # Guardar la lista actualizada de archivos procesados
  saveRDS(archivos_procesados , file = ruta_archivords_historico_incidencias)
  
  rm(lista_data_frames_incidencias,incidencias_del_dia,incidencias_del_dia_expandido)
} else {
  historico_incidencias <- readRDS(archivords_historico_incidencias)
}




funcion_actualizar_incidencias_10334 <- function(
    archivos_nuevos,
    ruta_carpeta_archivos){
  
  
  lista_data_frames_incidencias <- map(archivos_nuevos, function(x) {
    
    # Si 'x' empieza con una letra de unidad seguida de ":" (o con "/" en sistemas Unix),
    # se asume que ya es una ruta absoluta.
    if (grepl("^(?:[A-Za-z]:|/)", x)) {
      full_path <- x
    } else {
      full_path <- file.path(ruta_carpeta_archivos, x)
    }
    
    
    # Leer el archivo usando la ruta completa
    tabla_actual_incidencias <- read_delim(full_path,
                                       delim = "\t", 
                                       escape_double = FALSE,
                                       trim_ws = TRUE,
                                       locale = locale(encoding = "ISO-8859-1"))
    
   
    
    # Retornar el dataframe modificado
    tabla_actual_incidencias
  })
  
  
  incidencias_nuevo <- bind_rows(!!!lista_data_frames_incidencias)
  
  incidencias_nuevo <- incidencias_nuevo %>% 
    filter(Posiciones != "VIAJE")
  
  incidencias_nuevo <- incidencias_nuevo %>% 
    separate(Posiciones, into = c("inicio", "fin"), sep = " - ")
  
  # Crear nuevas filas para cada posición desde inicio hasta fin
  incidencias_nuevo <- incidencias_nuevo %>%
    rowwise() %>%
    mutate(Posicion = list(seq(inicio, fin))) %>% # Crear la lista de posiciones
    unnest(Posicion) %>%  # Expandir la lista en filas
    select(Circuito, Dia, id_viaje, Posicion, cod_inci, Descripcion)  # Seleccionar las columnas deseadas
  
  incidencias_nuevo <- incidencias_nuevo %>% 
    rename(Fecha = Dia,
           Id_viaje = id_viaje,
           Id_incidencia = cod_inci)
   
  
  
  
  incidencias_nuevo$Fecha <- as.Date(incidencias_nuevo$Fecha, format = "%d/%m/%Y")
  
  incidencias_nuevo <- incidencias_nuevo %>%
    mutate(Id_incidencia = ifelse(is.na(Id_incidencia), "Sin datos", Id_incidencia)) %>% 
    mutate(Id_incidencia = as.integer(Id_incidencia))
    
  
  incidencias_nuevo <- funcion_agregar_responsables(incidencias_nuevo)
  
  ## Borro los duplicados
  incidencias_nuevo <- incidencias_nuevo %>% 
    distinct()
  
  return(incidencias_nuevo)
  
}


funcion_agregar_gid_incidencias <- function(datos_nuevos){
  #####agregar el GID
  
  #incidencias_sin_gid <- datos_nuevos
  incidencias_sin_gid <- datos_nuevos
  
  ### Creo la columna gid vacía, en caso de no existir.
  if (!"gid" %in% names(incidencias_sin_gid)) {
    incidencias_sin_gid <- incidencias_sin_gid %>%
      mutate(gid = NA_character_)
  }
  
  # Busco las fechas minimas y máximas que no tienen agregado el GID.
  max_fecha_incidencias_sin_gid <- max(incidencias_sin_gid$Fecha, na.rm = TRUE)
  min_fecha_incidencias_sin_gid <- min(incidencias_sin_gid$Fecha, na.rm = TRUE)
  
  secuencia_fechas <- seq(from = min_fecha_incidencias_sin_gid, to = max_fecha_incidencias_sin_gid, by = "day")
  
  # fecha <- as.Date("2025-02-27")
  
  # Recorrer cada fecha de la secuencia
  for (fecha in secuencia_fechas) {
    
    # Filtrar las ubicaciones para la fecha actual
    historico_ubicaciones_pordia <- historico_ubicaciones %>% 
      filter(Fecha == fecha)
    
    # Renombrar la columna 'gid' a 'gid_new' en la tabla de ubicaciones
    datos_fecha <- historico_ubicaciones_pordia %>% 
      select(gid_new = gid, Fecha, Circuito, Posicion)
    
    incidencias_sin_gid <- incidencias_sin_gid %>%
      left_join(
        historico_ubicaciones_pordia %>% 
          select(gid_new = gid, Fecha, Circuito, Posicion),
        by = c("Fecha", "Circuito", "Posicion" ),
        relationship = "many-to-many"
      ) %>%
      mutate(
        gid = coalesce(gid, as.character(gid_new))
      ) %>%
      select(-gid_new)
  }
  
  incidencias_sin_gid <- incidencias_sin_gid %>%
    mutate(
      gid = if_else(is.na(gid), "-1", gid)
    )
  
  return(incidencias_sin_gid)
}


dataframe_responsable_incidencias <- function(){
  
  
  # Funciones relacionadas a las incidencias
  df_incidencias <- data.frame(
    Id_Motivo_no_levante = -1:26,
    descripcion = c(
      "Sin datos",
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
      "Contenedor Caído en Tolva
"
    ),
    Accion_requerida = c("Sin datos", # vacio
                          "Sin_accion",        # Levantado
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
                         "Levantar"           # Contenedor Caído en Tolva

    ),
    Responsable = c(
      "Sin datos", # vacio
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
      "Operativa"           # Contenedor Caído en Tolva

    )
  )
  
  return(df_incidencias)
  
}

# df <- incidencias_nuevo
funcion_agregar_responsables <- function(df){
  responsables_incidencias <- dataframe_responsable_incidencias()
  
  union_por_id <- df %>% 
    left_join(responsables_incidencias, by = c("Id_incidencia" = "Id_Motivo_no_levante"))
  
  # Unión por Descripcion
  union_por_desc <- df %>% 
    left_join(responsables_incidencias, by = c("Descripcion" = "descripcion"))
  
  # Combinar ambas uniones: si hay coincidencia en el join por Id, se usa; sino se toma la del join por Descripcion.
  resultado <- df %>% 
    mutate(
      Accion_requerida = coalesce(union_por_id$Accion_requerida, union_por_desc$Accion_requerida),
      Responsable      = coalesce(union_por_id$Responsable, union_por_desc$Responsable)
    ) %>% 
    filter(!is.na(Descripcion))
  
  
  
  return(resultado)
}
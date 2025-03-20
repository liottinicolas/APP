
funcion_actualizar_viajesEnUnPeriodo_10334 <- function(
    archivos_nuevos,
    ruta_carpeta_archivos){
  
  
  lista_data_frames_llenado <- map(archivos_nuevos, function(x) {
    
    # Si 'x' empieza con una letra de unidad seguida de ":" (o con "/" en sistemas Unix),
    # se asume que ya es una ruta absoluta.
    if (grepl("^(?:[A-Za-z]:|/)", x)) {
      full_path <- x
    } else {
      full_path <- file.path(ruta_carpeta_archivos, x)
    }
    
    
    # Leer el archivo usando la ruta completa
    tabla_actual_ubicaciones <- read_delim(full_path,
                                       delim = "\t", 
                                       escape_double = FALSE,
                                       trim_ws = TRUE,
                                       locale = locale(encoding = "ISO-8859-1"))
    
     tabla_actual_ubicaciones$PESO_NETO <- as.numeric(tabla_actual_ubicaciones$PESO_NETO)
    
    
    # Retornar el dataframe modificado
    tabla_actual_ubicaciones
  })
  
  
  viajes_nuevo <- bind_rows(!!!lista_data_frames_llenado)
  
  viajes_nuevo$DIA <- as.Date(viajes_nuevo$DIA, format = "%d/%m/%Y")
  
  viajes_nuevo$PESO_NETO <- as.numeric(viajes_nuevo$PESO_NETO)
  
  # Convertir HORA_SALIDA y HORA_LLEGADA a POSIXct agregando la fecha del DIA
  viajes_nuevo <- viajes_nuevo %>%
    mutate(
      HORA_SALIDA = as.POSIXct(paste(DIA, HORA_SALIDA), format = "%Y-%m-%d %H:%M:%S"),
      HORA_LLEGADA = as.POSIXct(paste(DIA, HORA_LLEGADA), format = "%Y-%m-%d %H:%M:%S")
    ) %>%
    # Modificar las horas según ID_TURNO
    mutate(
      HORA_SALIDA = case_when(
        ID_TURNO == 3 & hour(HORA_SALIDA) >= 0 & hour(HORA_SALIDA) < 6 ~ HORA_SALIDA + days(1),
        TRUE ~ HORA_SALIDA
      ),
      HORA_LLEGADA = case_when(
        ID_TURNO == 3 & hour(HORA_LLEGADA) >= 0 & hour(HORA_LLEGADA) < 6 ~ HORA_LLEGADA + days(1),
        TRUE ~ HORA_LLEGADA
      )
    )
  
  ## Borro los duplicados
  viajes_nuevo <- viajes_nuevo %>% 
    distinct()
  
  ## Cambio los nombres a minisculas todo
  viajes_nuevo <- viajes_nuevo %>%
    rename_with(~ sapply(.x, function(x) {
      paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
    }))
      
    
  viajes_nuevo <- viajes_nuevo %>% 
    rename(Fecha = Dia)
  
    
  
  return(viajes_nuevo)
  
}



# fecha_consulta <- as.Date("2025-02-15")
funcion_obtener_contenedores_levantados_por_grua_pluma_sin_llenado <- function(fecha_consulta){

Plumas <- c("SIM2378", "SIM2379", "SIM2380" , "SIM2381")
Gruas <- c("SIM2244", "SIM2225")

viajes_del_dia <- historico_viajes %>% 
  filter(Estado == "Finalizado" | Estado == "Cerrado") %>% 
  filter(Lugar_salida == 50) %>% 
  filter(!startsWith(Circuito, "OLC")) %>% 
  filter(Matricula %in% Plumas | Matricula %in% Gruas) %>% 
  filter(Cantidad_levantada != 0) %>% 
  filter(Fecha == fecha_consulta)

## este ya esta ok, falta cambiar posicion incial por posicion 
solo1levantado_deldia <- viajes_del_dia %>% 
  filter(Cantidad_levantada == 1)

### busco los que tuvieron más de 1 levantado para separar.
masde1levantado_deldia <- viajes_del_dia %>% 
  filter(Cantidad_levantada > 1)

masde1levantado_deldia_sin_pendientes <- masde1levantado_deldia %>% 
  filter(Cant_sin_levantar == 0)



masde1levantado_deldia <- masde1levantado_deldia_sin_pendientes %>%
  mutate(Posicion = map2(Posicion_inicial, Posicion_final, seq)) %>%
  unnest(cols = c(Posicion)) %>%
  select(-Posicion_inicial, -Posicion_final)

# masde1levantado_deldia <- masde1levantado_deldia_sin_pendientes %>%
#   rowwise() %>%
#   mutate(Posicion = list(seq(Posicion_inicial, Posicion_final))) %>%
#   unnest(cols = c(Posicion)) %>%
#   ungroup() %>%
#   select(-Posicion_inicial, -Posicion_final)

### aca tengo que unir los que tuvieron 1 solo levantado
### los que tiene más de dos levantados, sin pendientes y ya separados

bloque_1_levantados <- solo1levantado_deldia %>% 
  rename(Posicion = Posicion_inicial) %>% 
  select(Fecha,Id_viaje,Id_turno,Municipio,Circuito,Circuito_corto,Posicion) %>% 
  mutate(DB = "ViajesEnUnPeriodo")

bloque_2_levantados <- masde1levantado_deldia %>% 
  select(Fecha,Id_viaje,Id_turno,Municipio,Circuito,Circuito_corto,Posicion) %>% 
  mutate(DB = "ViajesEnUnPeriodo")

retorno <- bind_rows(bloque_1_levantados,bloque_2_levantados)

#### TODO

### Falta filtrar los que tenían mas de 1 contenedor a levantar, y les quedó pendiente alguno
# Cantidad levantada > 1 y cantidad_sinlevantar > 1

# son pocos ver dsp.

return(retorno)

}


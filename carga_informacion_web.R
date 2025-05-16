# nolint start: line_length_linter, object_name_linter

  ruta_RDS_datos <- file.path(ruta_proyecto, "scripts/estado_diario/historico_estado_diario.rds")
  web_historico_estado_diario <- read_rds(ruta_RDS_datos)
  
  ruta_RDS_llenado_completo <- file.path(ruta_proyecto, "scripts/llenado_completo/historico_llenado_completo.rds")
  web_historico_completo_llenado_incidencias <- read_rds(ruta_RDS_llenado_completo)
  
  ruta_RDS_historico_ubicaciones <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/historico_ubicaciones.rds")
  web_historico_ubicaciones <- read_rds(ruta_RDS_historico_ubicaciones)
  
  # Cálculo de fechas relevantes
  ultima_fecha_registro <- max(web_historico_estado_diario$Fecha, na.rm = TRUE)
  fecha_informe_diario <- ultima_fecha_registro + 1
  escribir_log("INFO", paste("Última fecha de registro:", ultima_fecha_registro, 
                             "- Fecha informe diario:", fecha_informe_diario))

  inicio <- as.Date(CONFIGURACION$FECHA_INICIO_ANALISIS)
  
  ruta_RDS_ubicaciones_conthegeom <- file.path(ruta_proyecto, "scripts/db/10393_ubicaciones/ubicaciones_con_thegheom.rds")
  ubicaciones_existentes <- readRDS(ruta_RDS_ubicaciones_conthegeom)

  # Carga de circuitos planificados
  ruta_RDS_circuitos_planificados <- file.path(ruta_proyecto, "scripts/para_mapear/circuitos_planificados.rds")
  web_circuitos_planificados <- readRDS(ruta_RDS_circuitos_planificados)

# nolint end

funcion_obtener_cambios_deestado_de_contenedores <- function(df) {
  df %>%
    # 1. Asegurarnos de que Fecha sea Date
    mutate(Fecha = as.Date(Fecha)) %>%
    # 2. Ordenar por gid y Fecha descendente (más reciente primero)
    arrange(gid, desc(Fecha)) %>%
    # 3. Agrupar por gid para detectar bloques
    group_by(gid) %>%
    # 4. Calcular el estado anterior y si hay un cambio real
    mutate(
      prev_estado = lag(Estado),
      cambio = case_when(
        # ambos NA → no cambio
        is.na(Estado) & is.na(prev_estado)            ~ FALSE,
        # uno NA y otro no → cambio
        xor(is.na(Estado), is.na(prev_estado))        ~ TRUE,
        # ambos no NA → comparar valores
        TRUE                                          ~ Estado != prev_estado
      ),
      # 5. Crear un identificador de bloque (run) acumulando cada cambio
      run_id = cumsum(row_number() == 1 | cambio)
    ) %>%
    # 6. Para cada bloque (gid + run_id), quedarnos sólo con 1ª y última fila
    group_by(gid, run_id) %>%
    filter(
      row_number() == 1 |       # primera fila del bloque
        row_number() == n()       # última fila del bloque
    ) %>%
    ungroup() %>%
    # 7. Quitamos las columnas auxiliares
    select(-prev_estado, -cambio, -run_id)
}

# @df_deldia es el df del informe diario
# Pre: Solo se le envía los que están en mantenimiento.
funcion_cargar_dias_que_esta_en_mantenimiento <- function(df){
  
  ### Funciones internas a utilizar acá.
  
  # Funcion 1
  
  ### 1 Obtengo del df del informe diario
  ### 2 filtro por el estado 
  get_gids_by_estado <- function(df, estado) {
    # 1. Obtengo los gid del informe diario
    # filtrados por el estado
    
    fecha_maxima_estado <- max(df$Fecha)
    
    gids_filtrados <- df %>%
      filter(Estado == estado) %>%
      pull(gid)
    
    # 2. busco en web_historico el historico de sus cambios
    # de aquellos gid que estaban con un "estado" en particular.
    result <- web_historico_ubicaciones %>%
      filter(Fecha <= fecha_maxima_estado) %>% 
      filter(gid %in% gids_filtrados)
    
    
    
    # 3. Obtengo todos los cambios que tuvo en la historia ese contenedor
    cambios <- funcion_obtener_cambios_deestado_de_contenedores(result)
    
    if(estado == "Mantenimiento"){
      cambios_deestado <- cambios %>% 
        filter(Estado == estado)
    }
    
    # 4. Devolver el data.frame filtrado
    return(cambios)
  }
  
  ## Funcion 2
  
  obtener_estados_con_diferencia <- function(df) {
    # 1. Asegurarnos de que Fecha sea Date
    df2 <- df %>%
      mutate(Fecha = as.Date(Fecha)) 
    
    # 2. Para cada (gid, Estado), la última fecha
    ultimos_por_estado <- df2 %>%
      group_by(gid, Estado) %>%
      summarise(ultima_fecha = max(Fecha), .groups = "drop")
    
    # 3. Por cada gid, quedarnos con hasta 2 estados (los dos de fecha más reciente)
    top2 <- ultimos_por_estado %>%
      group_by(gid) %>%
      arrange(desc(ultima_fecha), .by_group = TRUE) %>%
      slice_head(n = 2) %>%
      # 4. Calcular Diferencia:
      mutate(
        Diferencia = if (n() >= 2) {
          as.numeric(difftime(ultima_fecha[1], ultima_fecha[2], units = "days"))
        } else {
          as.numeric(difftime(ultima_fecha, min(df2$Fecha), units = "days"))
        }
      ) %>%
      ungroup()
    
    # 5. Unir con el df original para extraer sólo las filas de interés
    resultado <- df2 %>%
      inner_join(
        top2,
        by = c("gid", 
               "Estado", 
               "Fecha" = "ultima_fecha")
      )
    
    return(resultado)
  }
  
  ##############################################
  
  estado <- "Mantenimiento"
  
  # DF con los estados completos de los contenedores que estan en mantenimiendo el día seleccionado.
  historico_contenedores_en_mantenimiento <- get_gids_by_estado(df, "Mantenimiento")
  
  fecha_maxima_estado <- max(df$Fecha)
  
  # del df anterior,
  diferencias_estados_contenedores <- obtener_estados_con_diferencia(historico_contenedores_en_mantenimiento)
  
  ## Filtro que sea con los datos de hoy
  diferencias_estados_contenedores <- diferencias_estados_contenedores %>% 
    filter(Fecha == fecha_maxima_estado)
  
  return(diferencias_estados_contenedores)
}


  # gid_buscado <- 100304
# inicio <- as.Date("2025-03-01")
# fin <- as.Date("2025-05-01")
funcion_obtener_estado_de_contenedores_porgid <- function(gid_buscado,inicio,fin) {
  
  ubi_porgid <- web_historico_ubicaciones %>% 
    filter(gid == gid_buscado) %>% 
    filter(Fecha >= inicio) %>% 
    filter(Fecha <= fin) %>% 
    select(Fecha,gid,Estado) %>% 
    arrange(desc(Fecha))
  
  return(ubi_porgid)
  
}
  
 
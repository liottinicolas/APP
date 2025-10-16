# nolint start: line_length_linter, object_name_linter

source("scripts/para_mapear/circuitos_planificados.R")

# asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)

# Funcion para calcular el ranking dado un día.
funcion_calcular_ranking_deldia <- function(fecha_consulta, df_informedeldia){
  
  ### Calcular el ranking total
  
  df_informedeldia_activos <- df_informedeldia %>% 
    filter(is.na(Estado))
  
  ## Los agrupo por circuito y le calculo
  
  # Obtengo los circuitos planificados para obtener la frecuencia
  circuitos_plan <- datos_circuitos
  
  # Calculo el ranking y lo junto con la frecuencia
  ranking <- df_informedeldia_activos %>% 
    group_by(Circuito_corto) %>% 
    summarise(Ranking = round(mean(Acumulacion), digits = 2)) %>%
    left_join(
      circuitos_plan %>% 
        select(circuito_corto, Frecuencia, turno_planificado) %>%
        distinct(),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      UNA = round(((Ranking * Frecuencia) / 7) * 100, 2)
    )
  
  # Calcular otros dataframes de interés

  
  
  # Ranking por turno
  ranking_por_turno <- df_informedeldia_activos %>%
    left_join(
      circuitos_plan %>% select(circuito_corto, turno_planificado),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    group_by(turno_planificado) %>%
    summarise(Ranking_turno = round(mean(Acumulacion, na.rm = TRUE), digits = 2))
  
  # Datos detallados por circuito
  detalles_circuito <- df_informedeldia_activos %>%
    group_by(Circuito_corto) %>%
    summarise(
      Acumulacion_promedio = mean(Acumulacion, na.rm = TRUE),
      Acumulacion_max = max(Acumulacion, na.rm = TRUE),
      Registros = n()
    )
  
  # Calculo de mapa polígono según escala definida para UNA
  mapa_poligono <- ranking %>%
    mutate(
      valor_escala = case_when(
        is.na(UNA) ~ NA_real_,
        UNA <= 100 ~ 1,
        UNA > 100 & UNA <= 130 ~ 2,
        UNA > 130 & UNA <= 150 ~ 3,
        UNA > 150 & UNA <= 170 ~ 4,
        UNA > 170 ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    select(Circuito_corto, UNA, valor_escala)

  # Crear un nuevo dataframe que agregue la columna Frecuencia a df_informedeldia_activos según el circuito
  df_informedeldia_activos_con_frecuencia <- df_informedeldia_activos %>%
    left_join(
      circuitos_plan %>% select(circuito_corto, Frecuencia),
      by = c("Circuito_corto" = "circuito_corto")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      UNA = round(((Acumulacion * Frecuencia) / 7) * 100, 2)
    )
  
  # Retornar una lista con múltiples dataframes
  return(list(
    ranking_principal = ranking,
    ranking_turnos = ranking_por_turno,
    detalles = detalles_circuito,
    mapa_poligono = mapa_poligono,
    datos_activos = df_informedeldia_activos_con_frecuencia
  ))
}

## Obtener el ranking de determinado dia ----
fecha_consulta <- as.Date("2025-09-08")
df_informedeldia <- historico_estado_diario %>% 
  filter(Fecha == fecha_consulta)

asd <- funcion_calcular_ranking_deldia(fecha_consulta,df_informedeldia)
ranking_por_circuito <- asd$ranking_principal
mapa_poligono <- asd$mapa_poligono
total <- asd$datos_activos

## FIN - Obtener el ranking de determinado dia ----

funcion_agregar_frecuencia

  # Obtener las frecuencias únicas presentes en el dataframe total y crear una tabla con ellas
  tabla_frecuencias <- total %>%
    distinct(Frecuencia) %>%
    arrange(Frecuencia) %>%
    mutate(
      Frec_texto = case_when(
        Frecuencia == 3 ~ "3 veces por semana",
        Frecuencia == 3.5 ~ "48 horas",
        Frecuencia == 2.33 ~ "72 horas",
        Frecuencia == 7 ~ "Diaria",
        TRUE ~ as.character(Frecuencia)
      ),
      Periodo = round(7 / Frecuencia, 2),
      UNA_minimo_dia = round(1 / Periodo, 4) * 100,
      UNA_minimo_turno = round((round(1 / Periodo, 4) * 100) / 3, 2),
      # El ciclo es la cantidad de semanas que tarda en repetirse el patrón de acumulación
      ciclo_semanal = case_when(
        Frecuencia == 3.5 ~ 2,    # 14 días (2 semanas) para repetir el patrón
        Frecuencia == 2.33 ~ 3,   # 21 días (3 semanas) para repetir el patrón
        TRUE ~ 1                  # Para otras frecuencias, se asume 1 semana
      ),
      dias_totales_ciclo = ciclo_semanal * 7,
      # Cálculo de días con 3, 2 y 1 días de acumulación en el ciclo completo
      dias_3_acum = case_when(
        Frecuencia == 3.5 ~ 0,    # Nunca hay 3 días de acumulación en el ciclo de 14 días
        Frecuencia == 3 ~ 1,      # Para frecuencia 3: 1 día con 3 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 3 de acumulación
        Frecuencia == 7 ~ 0,      # Diario, nunca se acumulan 3 días
        TRUE ~ 0
      ),
      dias_2_acum = case_when(
        Frecuencia == 3.5 ~ 7,    # En 14 días, hay 7 días con 2 de acumulación
        Frecuencia == 3 ~ 2,      # Para frecuencia 3: 2 días con 2 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 2 de acumulación
        Frecuencia == 7 ~ 0,      # Diario, nunca se acumulan 2 días
        TRUE ~ pmax(0, dias_totales_ciclo - round(Frecuencia * ciclo_semanal)) # Aproximación para otras frecuencias
      ),
      dias_1_acum = case_when(
        Frecuencia == 3.5 ~ 7,    # En 14 días, hay 7 días con 1 de acumulación
        Frecuencia == 3 ~ 3,      # Para frecuencia 3: 3 días con 1 de acumulación
        Frecuencia == 2.33 ~ 7,   # Para frecuencia 2.33: 7 días con 1 de acumulación
        Frecuencia == 7 ~ dias_totales_ciclo, # Diario, todos los días con 1 de acumulación
        TRUE ~ round(Frecuencia * ciclo_semanal) # Aproximación para otras frecuencias
      ),
      # Cálculo de UNA óptima según la fórmula solicitada
      Una_optima = round(
        (
          (dias_3_acum * UNA_minimo_dia*3) +
          (dias_2_acum * UNA_minimo_dia*2) +
          (dias_1_acum * UNA_minimo_dia*1)  
        ) / dias_totales_ciclo,
        2
      )
    )


  # Calcular el valor óptimo global (igual para todos)
  valor_optimo_global <- mean(tabla_frecuencias$Una_optima, na.rm = TRUE)

  # Agregar columna con el valor óptimo global a la tabla
  tabla_frecuencias <- tabla_frecuencias %>%
    mutate(Una_optima_global = round(valor_optimo_global, 2))

# Agrupar los datos activos (total) por la columna Frecuencia y cuantificar la cantidad de registros por cada frecuencia
resumen_frecuencia <- total %>%
  group_by(Frecuencia) %>%
  summarise(Cantidad = n()) %>%
  arrange(Frecuencia)



####### DF DEL INFORME DIARIO + LA FRECUENCIA + EL PEERÍODO ######


  # Crear una copia del dataframe historico_informe_diario
  historico_informe_diario_con_frecuencia <- historico_estado_diario

  # Anexar la información de Frecuencia y Periodo según el "cod_recorrido" (antes "Circuito")
  historico_informe_diario_con_frecuencia <- historico_informe_diario_con_frecuencia %>%
    left_join(
      datos_circuitos %>% 
        select(cod_recorrido, Frecuencia, Periodo),
      by = c("Circuito" = "cod_recorrido")
    ) %>%
    mutate(
      Frecuencia = as.numeric(Frecuencia),
      Acumulacion = as.numeric(Acumulacion),
      UNA = ((Acumulacion * Frecuencia) / 7) * 100
    )

  ####### DF PARA OBTENER LA CANTIDAD DE CONTENEDORES QUE HAY SEGÚN SU FRECUENCIA ######
  
      # Crear un nuevo dataframe que agrupe por día y por Frecuencia, contando el total de filas para cada combinación
  df_agrupado_dia_frecuencia <- historico_informe_diario_con_frecuencia %>%
    filter(is.na(Estado)) %>% 
    group_by(Fecha, Frecuencia,Periodo) %>%
    summarise(Total = n()) %>%
    ungroup()
  
#######################################################################################################
  
  
  # Crear un nuevo dataframe que agrupe por día y por Frecuencia, TENIENDO la cantidad de contenedores 
  # con equis cantidad de días de acumulación, separado por día y frecuencia.
  df_agrupado_dia_acumulacion <- historico_informe_diario_con_frecuencia %>%
    filter(is.na(Estado)) %>% 
    group_by(Fecha, Frecuencia,Periodo, Acumulacion) %>%
    summarise(Total = n()) %>%
    ungroup()
  
  # Agrego la una según la acumulación y la frecuencia
  df_agrupado_dia_acumulacion <- df_agrupado_dia_acumulacion %>% 
    mutate(una_por_acumulacion  = Acumulacion/Periodo) %>% 
    mutate(una_total_por_Acumulacion = una_por_acumulacion * Total)

  # Crear un nuevo dataframe que, para cada fecha y frecuencia, sume la cantidad de filas y el total de una_total_por_acumulacion,
  # y además agregue la información de Total de df_agrupado_dia_frecuencia cuando coinciden Fecha y Frecuencia.
  # Ahora se agrega la columna UNA_Por_Frecuencia que es una_total_por_Acumulacion / Total
  df_una_total_por_fecha_frecuencia <- df_agrupado_dia_acumulacion %>%
    group_by(Fecha, Frecuencia) %>%
    summarise(
      total_una_por_fecha_frecuencia = sum(una_total_por_Acumulacion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df_agrupado_dia_frecuencia %>% select(Fecha, Frecuencia, Periodo, Total),
      by = c("Fecha", "Frecuencia")
    ) %>%
    mutate(
      UNA_Por_Frecuencia = ifelse(Total > 0, total_una_por_fecha_frecuencia / Total, NA_real_),
      UNA_Por_Frecuencia_Normalizado = UNA_Por_Frecuencia * 100
    ) %>%
    left_join(
      tabla_frecuencias %>% select(Frecuencia, Una_optima, Una_optima_global),
      by = "Frecuencia"
    )
  
  

  # Filtrar por cada tipo de frecuencia y crear un dataframe para cada una
  df_una_frecuencia_3 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 3)
  df_una_frecuencia_3_5 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 3.5)
  df_una_frecuencia_2_33 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 2.33)
  df_una_frecuencia_7 <- df_una_total_por_fecha_frecuencia %>% filter(Frecuencia == 7)

  # Nuevo dataframe que agrupa el total por día (independientemente de la frecuencia)
  # Ahora se anexa a cada fila el valor de valor_optimo_global
  df_una_total_por_fecha <- df_agrupado_dia_acumulacion %>%
    group_by(Fecha) %>%
    summarise(
      total_una_por_fecha = sum(una_total_por_Acumulacion, na.rm = TRUE),
      total_contenedores = sum(Total, na.rm = TRUE)
    ) %>%
    mutate(
      UNA_Por_Dia = ifelse(total_contenedores > 0, total_una_por_fecha / total_contenedores, NA_real_),
      UNA_Por_Dia_Normalizado = UNA_Por_Dia * 100
    ) %>%
    mutate(
      valor_optimo_global = unique(tabla_frecuencias$Una_optima_global)
    )

  # Función para graficar Fecha vs UNA_Por_Dia_Normalizado y valor_optimo_global
  graficar_UNA_por_dia <- function(df, titulo = "UNA por Día en el tiempo") {
    library(ggplot2)
    ggplot(df, aes(x = Fecha)) +
      geom_line(aes(y = UNA_Por_Dia_Normalizado), color = "steelblue", size = 1) +
      geom_point(aes(y = UNA_Por_Dia_Normalizado), color = "darkred", size = 2) +
      geom_line(aes(y = valor_optimo_global), color = "orange", linetype = "dotted", size = 1) +
      labs(
        title = titulo,
        x = "Fecha",
        y = "UNA por Día Normalizado"
      ) +
      theme_minimal() +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
      theme(legend.position = "none")
  }
  # Ejemplo de uso:
  # graficar_UNA_por_dia(df_una_total_por_fecha, "UNA por Día en el tiempo")

# Función para graficar Fecha vs UNA_Por_Frecuencia, Una_optima y Una_optima_global dado un dataframe
graficar_UNA_por_frecuencia <- function(df, titulo = "UNA por Frecuencia en el tiempo") {
  library(ggplot2)
  ggplot(df, aes(x = Fecha)) +
    geom_line(aes(y = UNA_Por_Frecuencia_Normalizado), color = "steelblue", size = 1) +
    geom_point(aes(y = UNA_Por_Frecuencia_Normalizado), color = "darkred", size = 2) +
    geom_line(aes(y = Una_optima), color = "forestgreen", linetype = "dashed", size = 1) +
    geom_line(aes(y = Una_optima_global), color = "orange", linetype = "dotted", size = 1) +
    labs(
      title = titulo,
      x = "Fecha",
      y = "UNA por Frecuencia"
    ) +
    theme_minimal() +
    scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
    theme(legend.position = "none")
}

# Ejemplo de uso:
# graficar_UNA_por_frecuencia(df_una_frecuencia_3, "UNA por Frecuencia 3 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_3_5, "UNA por Frecuencia 3.5 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_2_33, "UNA por Frecuencia 2.33 en el tiempo")
# graficar_UNA_por_frecuencia(df_una_frecuencia_7, "UNA por Frecuencia 7 en el tiempo")


# Ordenar el ranking por UNA y actualizar Excel
ranking_ordenado <- ranking_por_circuito %>%
  arrange(desc(UNA)) %>%
  select(Circuito_corto, Ranking, UNA, turno_planificado)
# 
# # Definir las rutas de los archivos Excel
# excel_path_load <- file.path("scripts", "para_mapear", "Ranking_base.xlsx")
# excel_path_save <- file.path("scripts", "para_mapear", "Ranking_base2.xlsx")
# 
# # Cargar el archivo Excel existente
# wb <- loadWorkbook(excel_path_load)
# 
# # Escribir el ranking ordenado en la hoja 1 a partir de la celda E18
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(Circuito_corto, Ranking),
#   startCol = 5,  # Columna E
#   startRow = 18,
#   colNames = FALSE  # No escribir nombres de columnas
# )
# 
# # Escribir UNA en la columna G
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(UNA),
#   startCol = 8,  # Columna G
#   startRow = 18,
#   colNames = FALSE
# )
# 
# # Escribir turno en la columna H
# writeData(
#   wb,
#   sheet = 1,
#   x = ranking_ordenado %>% select(turno_planificado),
#   startCol = 9,  # Columna H
#   startRow = 18,
#   colNames = FALSE
# )
# 
# # Guardar el archivo Excel con el nuevo nombre
# saveWorkbook(wb, excel_path_save, overwrite = TRUE)
# 
# # Mensaje de confirmación
# cat("Ranking actualizado exitosamente en", excel_path_save, "\n")
# 
# # nolint end





### CALCULAR UNA POR TURNO ----

source("scripts/para_mapear/circuitos_planificados.R")


historico_estado_diario_porturno_inicial <- historico_estado_diario %>% 
  filter(Fecha == "2025-02-14")


Fecha <- as.Date("2025-02-14")

asd <- funcion_calcular_ranking_deldia(Fecha,historico_estado_diario_porturno_inicial)

# Situación al finalizar el turno nocturno del 14/02
total <- asd$datos_activos

total <- total %>% 
  rename(Acumulacion_fin_turno = Acumulacion)

total <- total %>% 
  mutate(Acumulacion_horas = Acumulacion_fin_turno * 24)


total2 <- total %>%
  mutate(Frecuencia = as.character(Frecuencia)) %>%
  left_join(
    tabla_frecuencias %>%
      transmute(Frecuencia = as.character(Frecuencia),
                Frec_texto, Periodo, UNA_minimo_turno, Una_optima),
    by = "Frecuencia"
  )

total2$Frecuencia <- as.numeric(total2$Frecuencia)
  


total2 <- total2 %>%
  mutate(UNA_inicio_turno = NA_real_) %>% 
  mutate(UNA_fin_turno = NA_real_)
  


### arranca 14/02

df_turno_anterior <- total2 %>% 
  mutate(Turno_informe = "Matutino") %>% 
  mutate(Turno_levantado = "Nocturno")
  
df_turno_anterior <- df_turno_anterior %>%
  dplyr::mutate(UNA_inicio_turno = (Acumulacion_horas/(7/Frecuencia)/24)*100)

df_turno_anterior <- df_turno_anterior %>%
  relocate(Fecha, Turno_informe, .before = 1)

#informe_diario_por_turno <- function(df_turno_anterior){
  
  
  
  sig_turno_vec <- function(x){
    lvls <- c("Matutino","Vespertino","Nocturno")
    xi <- factor(str_to_title(x), levels = lvls, ordered = TRUE)
    out <- lvls[(as.integer(xi) %% 3) + 1]
    ifelse(is.na(xi), NA_character_, out)
  }
  
  ant_turno_vec <- function(x){
    lvls <- c("Matutino","Vespertino","Nocturno")
    xi <- factor(str_to_title(x), levels = lvls, ordered = TRUE)
    out <- lvls[((as.integer(xi) + 1) %% 3) + 1]  # desplaza -1
    ifelse(is.na(xi), NA_character_, out)
  }
  
  ## Ultimo día del informe.
  ultimo_dia_informe <- max(df_turno_anterior$Fecha)
  # Ultimo turno
  ultimo_turno_informe <- df_turno_anterior %>%
    dplyr::distinct(Turno_informe, .keep_all = TRUE)
  
  ultimo_turno_informe <- ultimo_turno_informe$Turno_informe
  ultimo_turno_levantado <- ant_turno_vec(ultimo_turno_informe)
    
  # TODO cambiar la fecha del dia del informe.
  
  # Info del levante del día matutino.
  
  llenado_del_dia_matutino <- historico_llenado %>% 
    filter(Fecha == ultimo_dia_informe+1) %>% 
    filter(Turno_levantado == ultimo_turno_informe) %>% 
    filter(Levantado == "S")
  
  # gids levantados  
  gids_ok <- llenado_del_dia_matutino %>% filter(Levantado == "S") %>% pull(gid) %>% unique()
  
  # anexo aquellos que se levantaron y modifico. 
  # 1) turno levantado
  df_turno_matutino <- df_turno_anterior %>%
    mutate(
      Turno_levantado = if (!"Turno_levantado" %in% names(.)) NA_character_ else Turno_levantado,
      Turno_levantado = if_else(gid %in% gids_ok, sig_turno_vec(ultimo_turno_levantado), Turno_levantado),
      #Turno_informe = sig_turno_vec(Turno_informe),
      UNA_inicio_turno = df_turno_anterior$UNA_inicio_turno
    ) %>% 
    relocate(Fecha, Turno_informe, .before = 1) %>% 
    select(-Acumulacion_fin_turno)

  df_turno_matutino <- df_turno_matutino %>%
    mutate(
      Acumulacion_horas = if (!"Acumulacion_horas" %in% names(.)) NA_real_ else Acumulacion_horas,
      Acumulacion_horas = case_when(
        gid %in% gids_ok        ~ 0,                 # si está el GID → 0
        is.na(Acumulacion_horas)     ~ NA_real_,          # si es NA → no tocar
        TRUE                    ~ Acumulacion_horas + 8),   # sino → sumar 8
        UNA_fin_turno = case_when(
          gid %in% gids_ok ~ 0,
          TRUE ~ Acumulacion_horas/(7/Frecuencia)/24*100
        )
    )
  
df <- df_turno_matutino
dia <- max(df$Fecha)
turno_actual <- df %>% 
  dplyr::distinct(Turno_informe, .keep_all = TRUE)
turno_actual <- turno_actual$Turno_informe
turno_siguiente <- sig_turno_vec(turno_actual)

# turno <- turno_siguiente
actualizar_informe_diario_por_turno <- function(turno,dia,df){
  
  llenado_del_turno <- historico_llenado %>% 
    filter(Fecha == dia+1) %>% 
    filter(Turno_levantado == turno) %>% 
    filter(Levantado == "S")
  
  gids_ok <- llenado_del_turno %>% filter(Levantado == "S") %>% pull(gid) %>% unique()
  
  # anexo aquellos que se levantaron y modifico. 
  # 1) turno levantado
  df_turno_siguiente <- df %>%
    mutate(
      Turno_levantado = if (!"Turno_levantado" %in% names(.)) NA_character_ else Turno_levantado,
      Turno_levantado = if_else(gid %in% gids_ok, turno, Turno_levantado),
      Turno_informe = sig_turno_vec(Turno_informe),
      UNA_inicio_turno = df$UNA_fin_turno
     )
    # relocate(Fecha, Turno_informe, .before = 1) %>% 
    # select(-Acumulacion_fin_turno)
  
  df_turno_siguiente <- df_turno_siguiente %>%
    mutate(
      Acumulacion_horas = if (!"Acumulacion_horas" %in% names(.)) NA_real_ else Acumulacion_horas,
      Acumulacion_horas = case_when(
        gid %in% gids_ok        ~ 0,                 # si está el GID → 0
        is.na(Acumulacion_horas)     ~ NA_real_,          # si es NA → no tocar
        TRUE                    ~ Acumulacion_horas + 8),   # sino → sumar 8
      UNA_fin_turno = case_when(
        gid %in% gids_ok ~ 0,
        TRUE ~ Acumulacion_horas/(7/Frecuencia)/24*100
      )
    )
  
  return(df_turno_siguiente)
  
}
# 
# df <- df_turno_matutino
# dia <- max(df$Fecha)
# turno_actual <- df %>%
#   dplyr::distinct(Turno_informe, .keep_all = TRUE)
# turno_actual <- turno_actual$Turno_informe
# turno_siguiente <- sig_turno_vec(turno_actual)
df_vespertino <- actualizar_informe_diario_por_turno(turno_siguiente,dia,df)
  
df <- df_vespertino
dia <- max(df$Fecha)
turno_actual <- df %>%
  dplyr::distinct(Turno_informe, .keep_all = TRUE)
turno_actual <- turno_actual$Turno_informe
turno_siguiente <- sig_turno_vec(turno_actual)
df_nocturno <- actualizar_informe_diario_por_turno(turno_siguiente,dia,df)
 
  
df_total <- rbind(df_turno_matutino,df_vespertino,df_nocturno)
df_total$Fecha <- df_total$Fecha + 1
  
 
# saveRDS(estado_diario_global, file = ruta_datos) 
  
  
## Ingresa el df del día anterior
df_deldia_porturnos <- df_total




funcion_actualizar_informeDiario_porHoras <- function(df_deldia_porturnos){
  
  # Obtengo la información del último día.
  dia <- max(df_deldia_porturnos$Fecha)
  #dia_llenado <- dia+1
  
  ## Matutino, busco el último informe, que es el NOCTURNO.
  
  df_delturno <- df_deldia_porturnos %>% 
    filter(Turno_informe == "Nocturno")
  
  
  
  turno_actual <- df_delturno %>%
    dplyr::distinct(Turno_informe, .keep_all = TRUE)
  turno_actual <- turno_actual$Turno_informe
  turno_siguiente <- sig_turno_vec(turno_actual)
  
  cat(sprintf("Actualizando turno: %s\n", turno_siguiente))
  
  df_matutino <- actualizar_informe_diario_por_turno(turno_siguiente,dia,df_delturno)
  
  turno_actual <- df_matutino %>%
    dplyr::distinct(Turno_informe, .keep_all = TRUE)
  turno_actual <- turno_actual$Turno_informe
  turno_siguiente <- sig_turno_vec(turno_actual)
  
  cat(sprintf("Actualizando turno: %s\n", turno_siguiente))
  
  df_vespertino <- actualizar_informe_diario_por_turno(turno_siguiente,dia,df_matutino)
  
  turno_actual <- df_vespertino %>%
    dplyr::distinct(Turno_informe, .keep_all = TRUE)
  turno_actual <- turno_actual$Turno_informe
  turno_siguiente <- sig_turno_vec(turno_actual)
  
  cat(sprintf("Actualizando turno: %s\n", turno_siguiente))
  
  df_nocturno <- actualizar_informe_diario_por_turno(turno_siguiente,dia,df_vespertino)
  
  df_final_deldia <- rbind(df_matutino,df_vespertino,df_nocturno)
  df_final_deldia$Fecha <- df_final_deldia$Fecha +1
  
  return(df_final_deldia)
  
}


# turno <- turno_siguiente
actualizar_informe_diario_por_turno <- function(turno,dia,df){
  
  # posiciones_del_dia <- historico_ubicaciones %>% 
  #   filter(Fecha == fecha_inicio) %>%
  #   filter(!grepl("^B_0[1-7]$", Circuito_corto)) %>%
  #   filter(is.na(Estado)) %>% 
  #   select(gid)
  # 
  # gids_faltantes <- posiciones_del_dia %>%
  #   filter(!is.na(gid)) %>%
  #   mutate(gid_chr = as.character(gid)) %>%
  #   anti_join(
  #     historico_informediario_porhoras_local %>%
  #       filter(!is.na(gid)) %>%
  #       transmute(gid_chr = as.character(gid)),
  #     by = "gid_chr"
  #   ) %>%
  #   pull(gid) %>%
  #   unique()
  # 
  # info_faltantes <- historico_ubicaciones %>%
  #   filter(Fecha == fecha_inicio) %>% 
  #   mutate(gid_chr = as.character(gid)) %>%
  #   semi_join(
  #     tibble(gid_chr = as.character(gids_faltantes)),
  #     by = "gid_chr"
  #   )
  
  
  
  
  
  llenado_del_turno <- historico_llenado %>% 
    filter(Fecha == dia+1) %>% 
    filter(Turno_levantado == turno) %>% 
    filter(Levantado == "S")
  
  gids_ok <- llenado_del_turno %>% filter(Levantado == "S") %>% pull(gid) %>% unique()
  
  # anexo aquellos que se levantaron y modifico. 
  # 1) turno levantado
  df_turno_siguiente <- df %>%
    mutate(
      Turno_levantado = if (!"Turno_levantado" %in% names(.)) NA_character_ else Turno_levantado,
      Turno_levantado = if_else(gid %in% gids_ok, turno, Turno_levantado),
      Turno_informe = sig_turno_vec(Turno_informe),
      UNA_inicio_turno = df$UNA_fin_turno
    )
  # relocate(Fecha, Turno_informe, .before = 1) %>% 
  # select(-Acumulacion_fin_turno)
  
  df_turno_siguiente <- df_turno_siguiente %>%
    mutate(
      Acumulacion_horas = if (!"Acumulacion_horas" %in% names(.)) NA_real_ else Acumulacion_horas,
      Acumulacion_horas = case_when(
        gid %in% gids_ok        ~ 0,                 # si está el GID → 0
        is.na(Acumulacion_horas)     ~ NA_real_,          # si es NA → no tocar
        TRUE                    ~ Acumulacion_horas + 8),   # sino → sumar 8
      UNA_fin_turno = case_when(
        gid %in% gids_ok ~ 0,
        TRUE ~ Acumulacion_horas/(7/Frecuencia)/24*100
      )
    )
  
  return(df_turno_siguiente)
  
}



ruta_RDS_estado_diario_por_horas <- file.path(ruta_proyecto, "scripts/para_mapear/historico_estadodiario_porhoras.rds")
# saveRDS(df_total, file = ruta_RDS_estado_diario_por_horas)

source("scripts/para_mapear/circuitos_planificados.R")
  
historico_informediario_porhoras <- if (file.exists(ruta_RDS_estado_diario_por_horas)) {
  readRDS(ruta_RDS_estado_diario_por_horas)
} else {
  character(0)
}

 # fecha_fin <- as.Date("2025-02-28")
actualizar_historico_informediario_porhoras <- function(fecha_fin = NULL) {
  
  
  # Normaliza/decide fecha_fin
  if (is.null(fecha_fin)) {
    fecha_fin <- max(as.Date(historico_llenado$Fecha), na.rm = TRUE)   # o Sys.Date()
  } else {
    fecha_fin <- as.Date(fecha_fin)
  }
  
  fecha_inicio <- max(historico_informediario_porhoras$Fecha)
  #fecha_fin <- as.Date("2025-09-02")
  # Obtengo la información del último turno del día anterior.
  
  historico_informediario_porhoras_local <- historico_informediario_porhoras %>% 
    filter(Fecha == fecha_inicio) %>% 
    filter(Turno_informe == "Nocturno")
  
  posiciones_del_dia <- historico_ubicaciones %>%
    filter(Fecha == fecha_inicio) %>%
    filter(!grepl("^B_0[1-7]$", Circuito_corto)) %>%
    filter(is.na(Estado)) %>%
    select(gid)

  gids_faltantes <- posiciones_del_dia %>%
    filter(!is.na(gid)) %>%
    mutate(gid_chr = as.character(gid)) %>%
    anti_join(
      historico_informediario_porhoras_local %>%
        filter(!is.na(gid)) %>%
        transmute(gid_chr = as.character(gid)),
      by = "gid_chr"
    ) %>%
    pull(gid) %>%
    unique()

  info_faltantes <- historico_ubicaciones %>%
    filter(Fecha == fecha_inicio) %>%
    mutate(gid_chr = as.character(gid)) %>%
    semi_join(
      tibble(gid_chr = as.character(gids_faltantes)),
      by = "gid_chr"
    )
  
  # Normalizo tipos y espacios para evitar falsos positivos
  info_filtrado <- info_faltantes %>%
    mutate(
      cc_key = str_squish(tolower(as.character(Circuito_corto))),
      pos_key = str_squish(tolower(as.character(Posicion)))
    ) %>%
    anti_join(
      historico_informediario_porhoras_local %>%
        transmute(
          cc_key = str_squish(tolower(as.character(Circuito_corto))),
          pos_key = str_squish(tolower(as.character(Posicion)))
        ) %>%
        distinct(),
      by = c("cc_key", "pos_key")
    ) %>%
    select(-cc_key, -pos_key)
  
  
  
  
  fechas <- seq.Date(fecha_inicio, fecha_fin, by = "day")

  
  # --- Lógica que usa fecha_fin ---
  # Ejemplo: devolver filas con esa fecha
  
  if(length(fechas) > 0) {
    
    print(paste("Procesando desde", fecha_inicio+1, "hasta", fecha_fin))
    
    print(paste("Se encontraron", length(fechas), "días nuevos para procesar"))
    # Lista para almacenar informes diarios de fechas nuevas
    lista_cambios <- list()
    
    for (f in seq.Date(fecha_inicio, fecha_fin, by = "day")) {
      
      # f <- as.Date("2025-02-16")
      fecha <- as.Date(f, origin = "1970-01-01")
      # tu lógica con la fecha f
      cat(sprintf("Procesando fecha: %s\n", fecha))
      
      informe_del_dia <- funcion_actualizar_informeDiario_porHoras(historico_informediario_porhoras_local)
      
      historico_informediario_porhoras_local <- informe_del_dia
      
      # Almacenar informe si tiene contenido
      if(nrow(informe_del_dia) > 0) {
        lista_cambios[[length(lista_cambios) + 1]] <- informe_del_dia
        print(paste("✓ Día", fecha, "agregado con", nrow(informe_del_dia), "registros"))
      } else {
        print(paste("✗ Día", fecha, "sin registros"))
      }
      

    
    
    }
    
    # Procesar datos nuevos si existen
    if(length(lista_cambios) > 0) {
      datos_nuevos <- bind_rows(lista_cambios) 
      
      lvls <- c("Matutino","Vespertino","Nocturno")
      
      # Crear/forzar factor con ese orden
      datos_nuevos$Turno_informe <- factor(datos_nuevos$Turno_informe, levels = lvls, ordered = TRUE)
      datos_nuevos <- datos_nuevos %>% 
        arrange(desc(Fecha),Circuito_corto,Posicion,desc(Turno_informe))
      
      
      return(datos_nuevos)
    }
  
 
  
  
  }
  
}










prueba <- actualizar_historico_informediario_porhoras()

prueba_agru <- prueba %>% 
  group_by(Fecha) %>% 
  summarise(total = n())

historico_estado_diario_agru <- historico_estado_diario %>% 
  group_by(Fecha) %>% 
  summarise(total = n())


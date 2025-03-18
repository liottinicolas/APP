
## Cargar los datos


# ===================================================================
# Función para crear los circuitos con sus correspondientes turnos planificados.
# 
#
# Entrada: Ninguno
# Salida: Dataframe separada por circuito, con el turno planificado.
# ===================================================================

turnos_planificados_por_circuito <- function(){
  municipios <- c("A", "B", "C", "CH", "D", "E", "F", "G")

  municipios_numeros <- list(
    A = c(101:114,116,118:119,201:206),
    B = 101:103,
    C = 101:116,
    CH = 101:123,
    D = 101:121,
    E = c(101:116,136),
    F = 101:115,
    G = c(101,103:116)
  )
  
  # Función para crear un dataframe para cada municipio
  crear_df_municipio <- function(municipio, numeros) {
    nombre_circuito <- paste0(municipio, "_DU_RM_CL_", numeros)
    data.frame(
      cod_recorrido = nombre_circuito,
      municipio = municipio,
      numero_circuito = numeros,
      turno_planificado = "M",
      stringsAsFactors = FALSE
    )
  }
  
  # Crear un dataframe para cada municipio y combinarlos
  df_list <- lapply(names(municipios_numeros), function(municipio) {
    crear_df_municipio(municipio, municipios_numeros[[municipio]])
  })
  
  # Combinar todos los dataframes en uno solo
  df_final <- do.call(rbind, df_list)
  
  df_final <- df_final %>%
    mutate(turno_planificado = case_when(
      cod_recorrido == "A_DU_RM_CL_106" ~ "N",
      cod_recorrido == "A_DU_RM_CL_119" ~ "V",
      municipio == "B" ~ "N",
      cod_recorrido == "C_DU_RM_CL_101" ~ "N",
      cod_recorrido == "C_DU_RM_CL_102" ~ "N",
      cod_recorrido == "C_DU_RM_CL_107" ~ "N",
      cod_recorrido == "C_DU_RM_CL_108" ~ "N",
      cod_recorrido == "C_DU_RM_CL_109" ~ "N",
      cod_recorrido == "C_DU_RM_CL_110" ~ "N",
      cod_recorrido == "C_DU_RM_CL_111" ~ "N",
      cod_recorrido == "C_DU_RM_CL_112" ~ "N",
      cod_recorrido == "C_DU_RM_CL_113" ~ "N",
      cod_recorrido == "C_DU_RM_CL_114" ~ "N",
      cod_recorrido == "C_DU_RM_CL_115" ~ "N",
      cod_recorrido == "C_DU_RM_CL_116" ~ "N",
      municipio == "CH" ~ "N",
      cod_recorrido == "D_DU_RM_CL_101" ~ "N",
      cod_recorrido == "D_DU_RM_CL_103" ~ "N",
      municipio == "E" ~ "N",
      cod_recorrido == "E_DU_RM_CL_136" ~ "M",
      cod_recorrido == "E_DU_RM_CL_102" ~ "V",
      cod_recorrido == "E_DU_RM_CL_103" ~ "V",
      cod_recorrido == "E_DU_RM_CL_106" ~ "V",
      cod_recorrido == "E_DU_RM_CL_107" ~ "V",
      cod_recorrido == "F_DU_RM_CL_101" ~ "V",
      cod_recorrido == "F_DU_RM_CL_102" ~ "V",
      cod_recorrido == "F_DU_RM_CL_103" ~ "V",
      cod_recorrido == "F_DU_RM_CL_104" ~ "V",
      municipio == "G" ~ "V",
      TRUE ~ as.character(turno_planificado)
    )
    )
  
  df_final <- df_final %>% 
    mutate(circuito_corto = ifelse(
      # Si tiene 3 digitos
      substring(cod_recorrido,nchar(cod_recorrido)-3,nchar(cod_recorrido)-3) == "_",
      #valor verdadero
      substring(cod_recorrido,nchar(cod_recorrido)-2,nchar(cod_recorrido)),
      substring(cod_recorrido,nchar(cod_recorrido)-1,nchar(cod_recorrido))
    )) %>%
    mutate(circuito_corto = paste0(municipio,"_",circuito_corto))
  
  df_final <- df_final %>% 
    select(cod_recorrido,circuito_corto,municipio,numero_circuito,turno_planificado)
  
  df_final$Fecha_inicio <- as.Date("2024-10-10")
  df_final$FechaFin <- as.Date(NA)
  
  
  
  return(df_final)
}

prueba <- turnos_planificados_por_circuito()

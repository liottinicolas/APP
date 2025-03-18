# 
# ultimo_dia_informe <- fecha_mas_alta_estado_diario()
# dia_consulta <- day(ultimo_dia_informe)
# mes_consulta <- month(ultimo_dia_informe)
# anio_consulta <- year(ultimo_dia_informe)
# df_historico_llenado <- historico_llenado
# df_historico_incidencias <- historico_incidencias

### Le agrego la direccion a el estadoDiario.
unir_incidencias_por_gid <- function(dia_consulta,
                                           mes_consulta,
                                           anio_consulta,
                                           df_historico_llenado,
                                           df_historico_incidencias){
  
  ### Busco la información del dia anterior
  fecha_informe_consulta <- make_date(year = anio_consulta, month = mes_consulta, day = dia_consulta)
  # Fecha datos, es para el día que se consulta. 1 dia menos que el dia del informe, pero el día que sucedió la incidencia.
  fecha_datos <- fecha_informe_consulta - 1
  contador <- 0
  
  # Filtro en el llenado diario
  # Aquellos que no fueron levantados. Tanto los que están con NA como los que no se levantó con su correspondiente incidencia.
  llenado_diario <- df_historico_llenado %>% 
    filter(dia == fecha_datos) %>% 
    filter(levantado == "N" | is.na(levantado))
  
  # filtro las indicdencias del día.
  incidencias_diarias <- df_historico_incidencias %>% 
    filter(Dia == fecha_datos)

  # De los que no fueron levantados en el día.
  # Filtro aquellos que no tienen incidencias ingresadas desde la APP.
  llenado_NA <- llenado_diario %>% 
    filter(is.na(levantado)) %>% 
    filter(cod_motivo_inactiva == 0)
  
  llenado_NO <- llenado_diario %>% 
    filter(levantado == "N") %>% 
    filter(cod_motivo_inactiva == 0)
  
  
  ## separo las incidencias por GID, VIAJE Y DESCRIPCION
  mini_incidencias <- incidencias_diarias %>% 
    select(gid,id_viaje,cod_inci,Descripcion)
  
  df_resultado <- llenado_NA %>%
    left_join(mini_incidencias, by = c("gid" ="gid", "id_viaje"="id_viaje"))
  
  
  mini_incidencias <- incidencias_diarias %>% 
    select(gid, id_viaje,cod_inci ,Descripcion)
  
  # Realizamos el join y actualizamos el campo desc_motivo directamente
  llenado_NA_actualizado <- llenado_NA %>%
    left_join(mini_incidencias, by = c("gid", "id_viaje")) %>%
    mutate(desc_motivo = ifelse(is.na(desc_motivo) | desc_motivo == "", Descripcion, desc_motivo)) %>%
    select(-Descripcion)
  
  # Elimina las advertencias de relación de muchos-a-muchos configurando "relationship"
  llenado_NA_actualizado <- llenado_NA_actualizado %>%
    distinct() # Elimina duplicados después de la unión
  
  llenado_total <-bind_rows(llenado_NA_actualizado,llenado_NO)
  
  # Realizar el join para obtener la información de 'cod_inci' desde 'df_incidencias'
  llenado_total <- llenado_total %>%
    left_join(df_incidencias, by = c("desc_motivo" = "descripcion")) %>%
    mutate(cod_inci = ifelse(is.na(cod_inci), motivo_no_levante, cod_inci)) %>%
    select(-motivo_no_levante, -accion_requerida, -responsable)
  
  
  llenado_total <- llenado_total %>%
    left_join(df_incidencias %>% select(motivo_no_levante, accion_requerida, responsable), 
              by = c("cod_inci" = "motivo_no_levante"))
  
  llenado_total <- llenado_total %>%
  mutate(Municipio = case_when(
    substring(nomenclatura_circuito,1,2) == "CH" ~ "CH",
    substring(nomenclatura_circuito,1,1) == "A" ~ "A",
    substring(nomenclatura_circuito,1,1) == "B" ~ "B",
    substring(nomenclatura_circuito,1,1) == "C" ~ "C",
    substring(nomenclatura_circuito,1,1) == "D" ~ "D",
    substring(nomenclatura_circuito,1,1) == "E" ~ "E",
    substring(nomenclatura_circuito,1,1) == "F" ~ "F",
    substring(nomenclatura_circuito,1,1) == "G" ~ "G"))
  
  llenado_total <- llenado_total %>%
  mutate(Circuito_corto = ifelse(
    # Si tiene 3 digitos
    substring(nomenclatura_circuito,nchar(nomenclatura_circuito)-3,nchar(nomenclatura_circuito)-3) == "_",
    #valor verdadero
    substring(nomenclatura_circuito,nchar(nomenclatura_circuito)-2,nchar(nomenclatura_circuito)),
    substring(nomenclatura_circuito,nchar(nomenclatura_circuito)-1,nchar(nomenclatura_circuito))
  )) %>%
    mutate(Circuito_corto = paste0(Municipio,"_",Circuito_corto))
  
  
  
  llenado_total <- llenado_total %>% 
    select(dia,gid,nomenclatura_circuito,Municipio,Circuito_corto,posicion,ubicacion,levantado,desc_turno,fecha_pasaje,id_viaje,the_geom,cod_inci,desc_motivo,accion_requerida,responsable)
  
  llenado_total <- llenado_total %>% 
    rename(Dia_incidencia = dia,
           Nomenclatura = nomenclatura_circuito,
           Posicion = posicion,
           Direccion = ubicacion,
           Levantado = levantado,
           Turno_viaje = desc_turno,
           Fecha_hora_levante = fecha_pasaje,
           Codigo_incidencia = cod_inci,
           Descripcion = desc_motivo,
           Accion_requerida = accion_requerida,
           Responsable= responsable
        )
  
return(llenado_total)

}
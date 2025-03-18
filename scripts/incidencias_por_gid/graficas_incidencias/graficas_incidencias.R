#############################################

# FUNCIONES #

# Dada una fecha de fin e inicio, me devuelve los pendientes del dia, con la diferencia de días de atraso
# Si la diferencia es 0, es porque la incidencia es del mismo dia.
funcion_Calcular_pendientes_por_dia <- function(db_historico_incidencias,
                                                db_informe_diario,
                                                responsable_incidencia,
                                                fecha_inicio_general,
                                                fecha_fin_general,
                                                lista_incidencias){
  
  
  # Aplicar los filtros base: "Responsable == 'Grua'", rango de fechas, y checkbox
  datos_filtrados <- db_historico_incidencias %>%
    filter(Responsable == responsable_incidencia) %>%
    filter(Dia_incidencia >= fecha_inicio_general) %>%
    filter(Dia_incidencia <= fecha_fin_general) %>%
    filter(Descripcion %in% lista_incidencias)  # Filtra según los valores seleccionados en el checkbox
  
  # El dia_informe_inicio es una variable, que va a traer la información del estado diario, al dia siguiente de la incidencia (que es el mismo dia)
  datos_filtrados <- datos_filtrados %>% 
    mutate(Fecha_estado_diario_dia_incidencia = Dia_incidencia + 1)
  
  ## Le agrego la acumulación del día que tiene seleccionada la fecha inicio.
  datos_filtrados <- datos_filtrados %>%
    left_join(db_informe_diario %>% select(gid, Dia_informe, Acumulacion),
              by = c("gid" = "gid", "Fecha_estado_diario_dia_incidencia" = "Dia_informe")) %>% 
    rename(Acumulacion_dia_incidencia = Acumulacion)
  
  informe_del_dia_para_incidencias <- db_informe_diario %>% 
    filter(Dia_informe == fecha_fin_general + 1)
  
  ## Le agrego la acumulación del día que tiene seleccionada la fecha fin.
  datos_filtrados <- datos_filtrados %>%
    left_join(informe_del_dia_para_incidencias %>% select(gid, Dia_informe, Acumulacion),
              by = c("gid" = "gid")) %>% 
    rename(Acumulacion_actual = Acumulacion)
  
  datos_filtrados <- datos_filtrados %>% 
    mutate(diferencia_dias = Dia_informe - Fecha_estado_diario_dia_incidencia) %>% 
    filter(!is.na(diferencia_dias))
  
  
  ### CUENTA LAS INCIDENCIAS ENTRE DOS FECHAS.
  conteo_incidencias <- datos_filtrados %>%
    group_by(gid, Descripcion) %>%  # Agrupa por 'gid' y 'Descripcion'
    summarise(Veces_inc = n(), .groups = 'drop')  # Cuenta las filas en cada grupo y añade una columna llamada 'conteo'
  
  
  ## Debo filtrar aquellos que la diferencia entre:
  # Dias de acumulacion del dia incidencia + dif_fechas = Dias de acumulacion actual + dif fechas.
  
  datos_filtrados <- datos_filtrados %>% 
    filter(Acumulacion_actual - Acumulacion_dia_incidencia == diferencia_dias)
  
  # # Elimino gid repetido y me quedo con el mpas anterior, los que pasaron antes no me interesa.
  # df_filtrado <- datos_filtrados %>%
  #   group_by(gid) %>%               # Agrupa por id
  #   slice_min(order_by = Dia_incidencia) %>% # Selecciona la fila con la fecha más baja
  #   ungroup()   
  
  
  
  df_filtrado <- datos_filtrados %>%
    group_by(gid,Dia_incidencia) %>%
    # Dentro de cada grupo (mismo gid):
    # 1. Revisar si hay algún Levantado con datos
    # 2. Si hay alguno con datos, nos quedamos con las filas que tengan Levantado vacío
    # 3. Si todos están vacíos, nos quedamos con la primera (las demás se eliminan)
    filter(
      if (n() == 1) {
        # Si no hay duplicados, conservar la fila tal cual
        TRUE
      } else {
        # Hay duplicados
        if (any(Levantado != "" & !is.na(Levantado))) {
          # Si existe al menos uno con datos en Levantado, nos quedamos con los vacíos
          Levantado != "" & !is.na(Levantado)
        } else {
          # Si todos están vacíos en Levantado, conservar solo la primera fila
          row_number() == 1
        }
      }
    ) %>%
    ungroup()
  
  # datos_filtrados <- datos_filtrados %>% 
  #   distinct(gid)
  
  df_final <- df_filtrado %>%
    group_by(gid) %>%
    slice_max(order_by = Dia_incidencia, n = 1) %>%
    ungroup()
  
  return (df_final)
}




funcion_calcular_historico_y_diario_de_incidencias <- function(db_historico_incidencias,
                                                               db_informe_diario,
                                                               responsable_incidencia,
                                                               fecha_inicio_general,
                                                               fecha_fin_general,                                                               
                                                               lista_incidencias){
  
  historico_tabla_paragrafico_incidencias <- data.frame()
  fecha_inicio_INICIAL <- fecha_inicio_general
  
  while (fecha_inicio_general <= fecha_fin_general) {
    
    aux_fecha <- fecha_inicio_general
    
    ## Filtro los días y la incidencia que quiero, los sumo y hago el df con la fecha
    
    ## Calculo incidencias pendientes al dia de la fecha.
    incidencias_pendientes_al_diadelafecha <- funcion_Calcular_pendientes_por_dia(historico_incidencias_completas,historico_estado_diario_con_direccion,responsable_incidencia,
                                                                                  fecha_inicio_INICIAL,aux_fecha,lista_incidencias)
    
    # Número de total de incidencias pendientes al dia de la fecha
    total_incidencias_pendientes_del_dia <- nrow(incidencias_pendientes_al_diadelafecha)
    
    # Incidencias que se generaron el día anterior.
    incidencias_generadas_mismo_dia <- incidencias_pendientes_al_diadelafecha %>% 
      filter(diferencia_dias == 0)
    
    total_incidencias_generadas_mismo_dia <- nrow(incidencias_generadas_mismo_dia)
    
    # Inicializar el dataframe vacío
    tabla_para_grafico <- tibble(
      Fecha = as.Date(character()),
      Generados = integer(),
      Pendientes = integer()
      
      
    )
    
    tabla_para_grafico <- bind_rows(tabla_para_grafico, tibble(
      Fecha = aux_fecha,
      
      
      
      
      Generados = total_incidencias_generadas_mismo_dia,
      Pendientes = total_incidencias_pendientes_del_dia
      
    ))
    
    
    
    
    
    historico_tabla_paragrafico_incidencias <-  rbind(historico_tabla_paragrafico_incidencias, tabla_para_grafico)
    
    fecha_inicio_general <- fecha_inicio_general + 1
    
  }
  
  return(historico_tabla_paragrafico_incidencias)
  
}

#############################################

#############################################
#############################################              GRUA  
#############################################

# Definir la ruta donde se guardará el excel del histórico
#archivoXLSX_historico_pendientes_generados_pluma <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
archivords_historico_pendientes_generados_grua <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","historicos_pendientes_generados_GRUA.rds")
#archivords_historico_levantes <- paste0(ruta_proyecto, "/scripts/incidencias_por_gid/historico_levantes_completos.rds")



# Cargar el histórico anterior si existe, de lo contrario usar solo los datos nuevos
historico_pendientes_generados_grua <- if (file.exists(archivords_historico_pendientes_generados_grua)) {
  # Cargar el histórico previamente guardado
  readRDS(archivords_historico_pendientes_generados_grua)
  #read_excel(archivoXLSX_historico_pendientes_generados_grua, sheet = "Hoja1")
} else {
  character(0)
}



## Si existe el historial.
## No es la primera vez, entonces hay que revisar si se agrega o no
if(length(historico_pendientes_generados_grua) > 0){
  
  responsable_incidencia <- "Grua"
  fecha_inicio_general <- as.Date("2024-10-10")
  fecha_fin_general <- max(historico_incidencias_completas$Dia_incidencia)
  lista_incidencias <- c("Sobrepeso")
  
  
  # Busco cual fue la última fecha que se tiene datos.
  ultima_fecha_ingresada <- as.Date(max(historico_pendientes_generados_grua$Fecha)) +1
  
  
  while (ultima_fecha_ingresada <= fecha_fin_general) {
    
    aux_fecha <- ultima_fecha_ingresada
    
    # Obtengo el df con los pendientes desde el día 0 hasta el día en cuestión
    grua_ultimo_dia_pendientes <- funcion_Calcular_pendientes_por_dia(historico_incidencias_completas,historico_estado_diario_con_direccion,responsable_incidencia,
                                                                      fecha_inicio_general,aux_fecha,lista_incidencias)
    
    pendientes <- nrow(grua_ultimo_dia_pendientes)
    
    sobrepesos_generadoshoy <- grua_ultimo_dia_pendientes %>% 
      filter(diferencia_dias == 0)
    sobrepesos_generadoshoy <- nrow(sobrepesos_generadoshoy)
    
    historico_pendientes_generados_grua <- rbind(historico_pendientes_generados_grua, data.frame(Fecha = ultima_fecha_ingresada, Generados = sobrepesos_generadoshoy,Pendientes = pendientes))
    
    ultima_fecha_ingresada <- ultima_fecha_ingresada + 1
    
    
  }
  
  saveRDS(historico_pendientes_generados_grua, file = archivords_historico_pendientes_generados_grua)
  
  # Limpiar variables innecesarias
  rm(responsable_incidencia, fecha_fin_general, lista_incidencias,
     historico_pendientes_generados_grua, archivords_historico_pendientes_generados_grua)
  
  #historico_pendientes_generados_grua$Fecha <- as.Date(historico_pendientes_generados_grua$Fecha)
  # 
  # wb <- loadWorkbook(archivoXLSX_historico_pendientes_generados_grua)
  # removeTable(wb, sheet = "Hoja1", table = "tabla_sobrepeso")
  # writeDataTable(wb, sheet = "Hoja1", x = historico_pendientes_generados_grua, tableName = "tabla_sobrepeso", tableStyle = "TableStyleMedium2")
  # 
  # ## Guardar en:
  # archivoXLSX_guardar_primero_pluma <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
  # saveWorkbook(wb, file = archivoXLSX_guardar_primero_pluma, overwrite = TRUE)
  # 
  # 


  
} else {

  # Primera vez
  
  responsable_incidencia <- "Grua"
  fecha_inicio_general <- as.Date("2024-10-10")
  fecha_fin_general <- max(historico_incidencias_completas$Dia_incidencia)
  lista_incidencias <- c("Sobrepeso")
  
  historico_pendientes_generados_grua <- funcion_calcular_historico_y_diario_de_incidencias(historico_incidencias_completas,
                                                               historico_estado_diario_con_direccion,
                                                               responsable_incidencia,
                                                               fecha_inicio_general,
                                                               fecha_fin_general,
                                                               lista_incidencias)
  # 
  # # Ruta al archivo base
  # archivoXLSX_historico_pendientes_generados_grua_BASE <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor_planillabase.xlsx")
  # 
  # # Leer archivo base
  # archivo <- read_excel(archivoXLSX_historico_pendientes_generados_grua_BASE, sheet = "Hoja1")
  # wb <- loadWorkbook(archivoXLSX_historico_pendientes_generados_grua_BASE)
  # 
  # # Remover la tabla anterior
  # removeTable(wb, sheet = "Hoja1", table = "tabla_sobrepeso")
  # 
  # # Escribir los datos como tabla
  # writeDataTable(wb, sheet = "Hoja1", x = historico_pendientes_generados_grua, tableName = "tabla_sobrepeso", tableStyle = "TableStyleMedium2")
  # 
  # # Crear un estilo para formato de fecha
  # date_style <- createStyle(numFmt = "dd/mm/yyyy") # Cambia aquí el formato si es necesario
  # 
  # # Aplicar el estilo a la columna de fecha
  # # Suponiendo que la columna de fecha es la primera columna
  # num_filas <- nrow(historico_pendientes_generados_grua)
  # addStyle(wb, sheet = "Hoja1", style = date_style, rows = 2:(num_filas + 1), cols = 1, gridExpand = TRUE)
  # 
  # # Guardar el archivo actualizado
  # archivoXLSX_guardar_primero_grua <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
  # saveWorkbook(wb, file = archivoXLSX_guardar_primero_grua, overwrite = TRUE)
  
  
  saveRDS(historico_pendientes_generados_grua, file = archivords_historico_pendientes_generados_grua)
  
  # Limpiar variables innecesarias
  rm(responsable_incidencia, fecha_fin_general, lista_incidencias,
     historico_pendientes_generados_grua, archivords_historico_pendientes_generados_grua)
  
}

### GRÁFICA.

# fecha_filtro_grafica <- fecha_inicio_general + 5
# 
# grua_grafico <- historico_pendientes_generados_grua %>% 
#   filter(Fecha >= fecha_filtro)
# 
# grafica_base <- ggplot(grua_grafico, aes(x = Fecha)) +
#   geom_line(aes(y = Generados, color = "Generados por día"), size = 1) +
#   geom_line(aes(y = Pendientes, color = "Acumulados"), size = 1) +
#   scale_color_manual(values = c("Generados por día" = "blue", "Acumulados" = "orange")) +
#   labs(
#     title = "Contenedores con sobrepesos",
#     x = "Fecha",
#     y = "Cantidad de contenedores",
#     color = NULL
#   ) +
#   scale_x_date(
#     date_breaks = "3 days",  # Etiquetas cada 7 días
#     date_labels = "%d/%m/%Y"  # Formato de fecha (día/mes/año)
#   ) +
#   guides(x = guide_axis(angle = 45)) +  # Ajusta automáticamente las etiquetas en ángulo para evitar solapamientos
#   theme_minimal() +
#   theme(
#     legend.position = "bottom"
#   )
# 
# # Convertir la gráfica en interactiva con plotly
# grafica_dinamica <- ggplotly(grafica_base)
# 
# # Mostrar la gráfica
# grafica_dinamica
# 
# 



#############################################
#############################################              PLUMA  
#############################################



# Definir la ruta donde se guardará el excel del histórico
#archivoXLSX_historico_pendientes_generados_pluma <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
archivords_historico_pendientes_generados_pluma <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","historicos_pendientes_generados_PLUMA.rds")
#archivords_historico_levantes <- paste0(ruta_proyecto, "/scripts/incidencias_por_gid/historico_levantes_completos.rds")



# Cargar el histórico anterior si existe, de lo contrario usar solo los datos nuevos
historico_pendientes_generados_pluma <- if (file.exists(archivords_historico_pendientes_generados_pluma)) {
  # Cargar el histórico previamente guardado
  readRDS(archivords_historico_pendientes_generados_pluma)
  #read_excel(archivoXLSX_historico_pendientes_generados_pluma, sheet = "Hoja1")
} else {
  character(0)
}



## Si existe el historial.
## No es la primera vez, entonces hay que revisar si se agrega o no
if(length(historico_pendientes_generados_pluma) > 0){

  responsable_incidencia <- "Pluma"
  fecha_inicio_general <- as.Date("2024-10-10")
  fecha_fin_general <- max(historico_incidencias_completas$Dia_incidencia)
  lista_incidencias <- c("Buzonera Girada","Contenedor Cruzado","Contenedor Fuera de Alcance")
  
  
  # Busco cual fue la última fecha que se tiene datos.
  ultima_fecha_ingresada <- as.Date(max(historico_pendientes_generados_pluma$Fecha)) +1
  
  
  while (ultima_fecha_ingresada <= fecha_fin_general) {
    
    aux_fecha <- ultima_fecha_ingresada
    
    # Obtengo el df con los pendientes desde el día 0 hasta el día en cuestión
    pluma_ultimo_dia_pendientes <- funcion_Calcular_pendientes_por_dia(historico_incidencias_completas,historico_estado_diario_con_direccion,responsable_incidencia,
                                                                      fecha_inicio_general,aux_fecha,lista_incidencias)
    
    pendientes <- nrow(pluma_ultimo_dia_pendientes)
    
    contenedoresIncidencias_generadoshoy <- pluma_ultimo_dia_pendientes %>% 
      filter(diferencia_dias == 0)
    contenedoresIncidencias_generadoshoy <- nrow(contenedoresIncidencias_generadoshoy)
    
    historico_pendientes_generados_pluma <- rbind(historico_pendientes_generados_pluma, data.frame(Fecha = ultima_fecha_ingresada, Generados = contenedoresIncidencias_generadoshoy,Pendientes = pendientes))
    
    ultima_fecha_ingresada <- ultima_fecha_ingresada + 1
    
    
  }
  
  saveRDS(historico_pendientes_generados_pluma, file = archivords_historico_pendientes_generados_pluma)
  
  # Limpiar variables innecesarias
  rm(responsable_incidencia, fecha_fin_general, lista_incidencias,
     historico_pendientes_generados_pluma, archivords_historico_pendientes_generados_pluma)
  
  #historico_pendientes_generados_pluma$Fecha <- as.Date(historico_pendientes_generados_pluma$Fecha)
  # 
  # wb <- loadWorkbook(archivoXLSX_historico_pendientes_generados_pluma)
  # removeTable(wb, sheet = "Hoja1", table = "tabla_sobrepeso")
  # writeDataTable(wb, sheet = "Hoja1", x = historico_pendientes_generados_pluma, tableName = "tabla_sobrepeso", tableStyle = "TableStyleMedium2")
  # 
  # ## Guardar en:
  # archivoXLSX_guardar_primero_pluma <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
  # saveWorkbook(wb, file = archivoXLSX_guardar_primero_pluma, overwrite = TRUE)
  # 
  # 
  
  
  
} else {
  
  # Primera vez
  
  responsable_incidencia <- "Pluma"
  fecha_inicio_general <- as.Date("2024-10-10")
  fecha_fin_general <- max(historico_incidencias_completas$Dia_incidencia)
  lista_incidencias <- c("Buzonera Girada","Contenedor Cruzado","Contenedor Fuera de Alcance")
  
  historico_pendientes_generados_pluma <- funcion_calcular_historico_y_diario_de_incidencias(historico_incidencias_completas,
                                                                                            historico_estado_diario_con_direccion,
                                                                                            responsable_incidencia,
                                                                                            fecha_inicio_general,
                                                                                            fecha_fin_general,
                                                                                            lista_incidencias)
  # 
  # # Ruta al archivo base
  # archivoXLSX_historico_pendientes_generados_pluma_BASE <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor_planillabase.xlsx")
  # 
  # # Leer archivo base
  # archivo <- read_excel(archivoXLSX_historico_pendientes_generados_pluma_BASE, sheet = "Hoja1")
  # wb <- loadWorkbook(archivoXLSX_historico_pendientes_generados_pluma_BASE)
  # 
  # # Remover la tabla anterior
  # removeTable(wb, sheet = "Hoja1", table = "tabla_sobrepeso")
  # 
  # # Escribir los datos como tabla
  # writeDataTable(wb, sheet = "Hoja1", x = historico_pendientes_generados_pluma, tableName = "tabla_sobrepeso", tableStyle = "TableStyleMedium2")
  # 
  # # Crear un estilo para formato de fecha
  # date_style <- createStyle(numFmt = "dd/mm/yyyy") # Cambia aquí el formato si es necesario
  # 
  # # Aplicar el estilo a la columna de fecha
  # # Suponiendo que la columna de fecha es la primera columna
  # num_filas <- nrow(historico_pendientes_generados_pluma)
  # addStyle(wb, sheet = "Hoja1", style = date_style, rows = 2:(num_filas + 1), cols = 1, gridExpand = TRUE)
  # 
  # # Guardar el archivo actualizado
  # archivoXLSX_guardar_primero_grua <- file.path(ruta_proyecto, "scripts", "incidencias_por_gid", "graficas_incidencias","Sobrepesos por contenedor.xlsx")
  # saveWorkbook(wb, file = archivoXLSX_guardar_primero_grua, overwrite = TRUE)
  
  
  saveRDS(historico_pendientes_generados_pluma, file = archivords_historico_pendientes_generados_pluma)
  
  # Limpiar variables innecesarias
  rm(responsable_incidencia, fecha_fin_general, lista_incidencias,
     historico_pendientes_generados_pluma, archivords_historico_pendientes_generados_pluma)
  
}

### GRÁFICA.

# fecha_filtro_grafica <- fecha_inicio_general + 5
# 
# grua_grafico <- historico_pendientes_generados_pluma %>% 
#   filter(Fecha >= fecha_filtro)
# 
# grafica_base <- ggplot(grua_grafico, aes(x = Fecha)) +
#   geom_line(aes(y = Generados, color = "Generados por día"), size = 1) +
#   geom_line(aes(y = Pendientes, color = "Acumulados"), size = 1) +
#   scale_color_manual(values = c("Generados por día" = "blue", "Acumulados" = "orange")) +
#   labs(
#     title = "Contenedores con sobrepesos",
#     x = "Fecha",
#     y = "Cantidad de contenedores",
#     color = NULL
#   ) +
#   scale_x_date(
#     date_breaks = "3 days",  # Etiquetas cada 7 días
#     date_labels = "%d/%m/%Y"  # Formato de fecha (día/mes/año)
#   ) +
#   guides(x = guide_axis(angle = 45)) +  # Ajusta automáticamente las etiquetas en ángulo para evitar solapamientos
#   theme_minimal() +
#   theme(
#     legend.position = "bottom"
#   )
# 
# # Convertir la gráfica en interactiva con plotly
# grafica_dinamica <- ggplotly(grafica_base)
# 
# # Mostrar la gráfica
# grafica_dinamica
# 
# 












 





  

  
  











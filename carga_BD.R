
######################## CARGA DIRECTA DE LAS CONSULTAS EXPORTABLES ################################

# Cargo paquetes y funciones básicas.
source("global.R")

# funciones para actualizar rds.
source("funciones_carga_datos.R")

# mas fuciones
source("funciones_utiles.R")

# Web
 source("funciones_para_web.R")

# Actualizacion de la bd
source("carga_datos.R")



ubicaciones_existentes <- funcion_listar_ubicaciones_unicas_con_thegeom_y_sin_thegeom()
estado_diario_global <- funcion_agregar_the_geom_a_faltantes(historico_estado_diario,ubicaciones_existentes$ubicaciones_con_thegeom)



ultima_fecha_registro <- max(historico_estado_diario$Fecha, na.rm = TRUE)
fecha_informe_diario <- max(historico_estado_diario$Fecha, na.rm = TRUE) + 1


# Limpieza
rm(ruta_carpeta_archivos,ruta_funciones,ruta_RDS_datos,ruta_RDS_planillas_procesadas)
rm(actualizar_planillas_RDS,eliminar_ultimo_dia_llenado,funcion_actualizar_incidencias_10334,funcion_actualizar_llenado_10484,
   funcion_actualizar_ubicaciones_10393,funcion_actualizar_viajesEnUnPeriodo_10334,funcion_agregar_gid_incidencias)



# repes <- imprimir_repetidos(historico_estado_diario)


inicio <- as.Date("2025-02-20")
fin <- max(historico_estado_diario$Fecha)
fecha_consulta <- fin
# incidencias_por_gid <- historico_completo_llenado_incidencias
# responsable <- "Pluma"
# estado_diario <- historico_estado_diario

prueba_grua <- funcion_mostrar_responsables_por_incidencias(historico_completo_llenado_incidencias,historico_estado_diario,inicio,fin,"Grua")
prueba_pluma <- funcion_mostrar_responsables_por_incidencias(historico_completo_llenado_incidencias,historico_estado_diario,inicio,fin,"Pluma")

funcion_exportar_incidencias_grua_o_pluma(prueba_grua,"Grua")
funcion_exportar_incidencias_grua_o_pluma(prueba_pluma,"Pluma")



############## para probar ####################

ini <- as.Date("2025-03-01")
fin <- as.Date("2025-03-26")
prueba_grua_dia <- funcion_mostrar_responsables_por_incidencias(historico_completo_llenado_incidencias,historico_estado_diario,ini,fin,"Grua")


  


# gid_buscado <- 134160
funcion_imprimir_datosporgid <-function(gid_buscado){
  
  imprimir <- historico_completo_llenado_incidencias %>% 
    filter(gid == gid_buscado) %>% 
    select(-Id_Motivo_no_levante,-Accion_requerida,-Responsable,-Circuito,-DB,-Numero_caja)
  
  wb <- createWorkbook()
  
  # Añadir una hoja
  addWorksheet(wb, "Datos")
  
  # Escribir el data frame como tabla con formato en la hoja "Datos"
  writeDataTable(wb, sheet = "Datos", x = imprimir, tableStyle = "TableStyleLight9")
  
  # Ajustar automáticamente el ancho de todas las columnas (1 a ncol(df_filtrado))
  setColWidths(wb, sheet = "Datos", cols = 1:ncol(imprimir), widths = "auto")
  
  # Crear un estilo para fechas (solo fecha)
  dateStyle <- createStyle(numFmt = "dd/mm/yyyy")
  
  # Suponiendo que la columna Fecha es la segunda columna, se aplica estilo a todas las celdas de esa columna
  addStyle(wb, sheet = "Datos", style = dateStyle, 
           cols = 2, rows = 2:(nrow(imprimir) + 1), gridExpand = TRUE)
  
  nombre_archivo <- paste0("datos_gid_", gid_buscado, ".xlsx")

  
  saveWorkbook(wb, file = nombre_archivo, overwrite = TRUE)
  
}  

funcion_obtener_datosporgid <-function(gid_buscado){
  
  imprimir <- historico_completo_llenado_incidencias %>% 
    filter(gid == gid_buscado) %>% 
    select(-Id_Motivo_no_levante,-Accion_requerida,-Responsable,-Circuito,-DB,-Numero_caja)
  
return(imprimir)
  
}  

funcion_imprimir_datosporgid(134160)
asd_177075 <- funcion_obtener_datosporgid(177075)
asd_181129 <- funcion_obtener_datosporgid(181129)





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


# 
# funcion_arreglar_ubicaciones_paragid <- function(){
#   
#   df <- historico_ubicaciones
#   
#   #### ACA DIRECCIONES QUE VEO QUE HAY QUE ARREGLAR, LAS CORRIJO
#   
#   # Actualizar la columna "Calle" con mutate y if_else
#   historico_ubicacionparthegeom <- df %>% 
#     mutate(Calle = if_else(gid == 180871 & Fecha == as.Date("2025-03-18"), 
#                            "JUAN RODRIGUEZ CORREA", 
#                            Calle)) %>% 
#     mutate(Calle = if_else(gid == 117165 & Fecha == as.Date("2025-03-13"), 
#                            "ARQ JUAN G GIURIA", 
#                            Calle))
#   
#   ##############################################################  
#   
#   ubicaciones_totales <- historico_ubicacionparthegeom %>% 
#     mutate(Direccion = ifelse(
#       is.na(historico_ubicacionparthegeom$Numero),
#       historico_ubicacionparthegeom$Calle,
#       paste(historico_ubicacionparthegeom$Calle, historico_ubicacionparthegeom$Numero)))
#   
#   return(ubicaciones_totales)
# }











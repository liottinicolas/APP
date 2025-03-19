
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




historico_sel <- historico_estado_diario %>%
  select(gid, Fecha_informe, Acumulacion) %>%
  rename(Fecha_informe_dia_anterior_incidencia = Fecha_informe,
         Acumulacion_dia_anterior_incidencia = Acumulacion)

# prueba_grua <- prueba_grua %>% 
#   mutate(Fecha_informe_dia_anterior_incidencia = Fecha_incidencia - 1)

prueba_grua <- prueba_grua %>%
  left_join(
    historico_sel,
    by = c("gid", "Fecha_incidencia" = "Fecha_informe_dia_anterior_incidencia")
  )

coincidencias <- prueba_grua %>%
  inner_join(
    historico_estado_diario %>% select(gid, Fecha_informe, Acumulacion),
    by = c("gid", "Fecha_incidencia" = "Fecha_informe")
  )



################################################################

ubicaciones_totales <- historico_ubicaciones %>% 
  mutate(Direccion = ifelse(
    is.na(historico_ubicaciones$Numero),
    historico_ubicaciones$Calle,
    paste(historico_ubicaciones$Calle, historico_ubicaciones$Numero)))

# df_filtrado <- ubicaciones_totales %>% 
#   distinct(gid, Direccion, .keep_all = TRUE)


############ UBICACION

ubicaciones_cambios_direccion_unica <- ubicaciones_totales %>%
  group_by(gid, Direccion) %>%              # Agrupamos por gid y dirección
  slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%  # Nos quedamos con la fila con la fecha mínima
  ungroup() %>% 
  select(gid,Fecha,Estado,Municipio,Circuito_corto,Posicion,Calle,Numero,Direccion,Observaciones) %>% 
  group_by(gid) %>%
  mutate(n_dir = n_distinct(Direccion)) %>%  # Cuenta las direcciones únicas por gid
  ungroup() %>% 
  filter(!(Circuito_corto %in% paste0("B_0", 1:7)))

total_gid_ubicaciones_cambios_direccion_unica <- ubicaciones_cambios_direccion_unica %>% 
  group_by(gid) %>% 
  summarise(total = n())

## Total gid con direcciones distintas = 10996
## Total gids = 10970

#### LLENADO.

unique_gids <- historico_llenado %>% 
  distinct(gid)

## TOTAL 40
gid_que_no_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
  anti_join(unique_gids)

## TOTAL 10956
gid_que_si_estan_ubicaciones <- ubicaciones_cambios_direccion_unica %>% 
  anti_join(gid_que_no_estan_ubicaciones)


direcciones_de_llenado <- historico_llenado %>%
  group_by(gid, Direccion) %>%              # Agrupamos por gid y dirección
  slice_min(order_by = Fecha, n = 1, with_ties = FALSE) %>%  # Nos quedamos con la fila con la fecha mínima
  ungroup() %>% 
  select(gid,Fecha,Municipio,Circuito_corto,Posicion,Direccion,the_geom)

# df_filtrado_llenado <- historico_llenado %>% 
#   distinct(gid, Direccion, .keep_all = TRUE)


### Bloque 1 #####

### Aquellos que coincida el gid, direccion, le agrego the_geom.
ubicaciones_por_direccion_gid_iguales <- gid_que_si_estan_ubicaciones %>%
  left_join(
    direcciones_de_llenado %>% select(gid, Direccion, the_geom),
    by = c("gid", "Direccion")
  ) 

## Filtro los que no tienen direccion
NA_de_ubicaciones_por_direccion_gid_iguales <- ubicaciones_por_direccion_gid_iguales %>% 
  filter(is.na(the_geom))

bloque_1_direcciones <- ubicaciones_por_direccion_gid_iguales %>% 
  anti_join(NA_de_ubicaciones_por_direccion_gid_iguales, by="gid")

## los que faltan????



## BLOQUE 2  ############################

### Aquellos que coincida el gid y la calle, le agrego the_geom.

ubicaciones_por_calle_y_gid_iguales <- NA_de_ubicaciones_por_direccion_gid_iguales %>%
  left_join(
    direcciones_de_llenado %>% select(gid, Direccion, the_geom),
    by = c("gid", "Calle" = "Direccion")
  ) %>% 
  select(-the_geom.x) %>% 
  rename(the_geom = the_geom.y)

NA_ubicaciones_por_calle_y_gid_iguales <- ubicaciones_por_calle_y_gid_iguales %>% 
  filter(is.na(the_geom))

bloque_2_calles <- ubicaciones_por_calle_y_gid_iguales %>% 
  anti_join(NA_ubicaciones_por_calle_y_gid_iguales, by="gid")

### BLOQUE 3

the_geom_ubicaciones <- bind_rows(bloque_1_direcciones,bloque_2_calles)
###################################

##3 Ubicaciones que por gid y direccion, gid y calle no se ubicaron
ubicaciones_diferente_direccion_y_calle <- NA_ubicaciones_por_calle_y_gid_iguales 

# direcciones que solo tuvieron 1 direccion en la historia.
ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle %>% 
  filter(n_dir == 1) %>% 
  select(-the_geom)

# Filtro solo los gids
unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
  distinct(gid)

### Filtro los que no tienen cambio de direccion, es decir para un solo gid no cambiaron
# No los encontré antes por que el gid y la dirección no coincidian (era dificil el nombre)

# Filtro esas direcciones sin repetir
the_geom_faltantes <- historico_llenado %>% 
  inner_join(unique_gids_de_ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion , by="gid") %>% 
  distinct(gid,Direccion,the_geom)

ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final <- ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion %>% 
  left_join(
    the_geom_faltantes %>% select(gid,the_geom), by="gid"
  )

### 10946
the_geom_ubicaciones <- bind_rows(the_geom_ubicaciones,ubicaciones_diferente_direccion_y_calle_sin_cambios_de_direccion_final)
###################################################################################################



ubicaciones_diferente_direccion_y_calle_con_cambios_de_direccion <- ubicaciones_diferente_direccion_y_calle %>% 
  filter(n_dir > 1) %>% 
  select(-the_geom)

the_geom_faltantes_176724 <- historico_llenado %>% 
  filter(gid == 176724) %>% 
  distinct(Direccion, .keep_all = TRUE)









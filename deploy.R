
  source("carga_BD.R")
  
  # Configuración del certificado SSL
  options(RCurlOptions = list(
    capath = "cacert.pem",
    ssl.verifypeer = TRUE
  ))
  
  # Código para desplegar la aplicación
  # Configura la cuenta (descomenta y rellena con tus datos)
  rsconnect::setAccountInfo(name = "datoseindicadores",
                            token = "C9EC0DCFA432ECC14F2438D893828344",
                            secret = "b6YjR35qtht+AisTyTGAL5T8bZRzg1D+2go+6pPC")
  
  # Despliega la aplicación (descomenta esta línea para desplegar)
  
  rsconnect::deployApp(appDir = ".",
                       appFiles = c("app.R", "config.R", "logging.R","global.R",
                                    "carga_informacion_web.R",
                                    "funciones_para_web.R",
                                    "funciones_utiles.R",
                                    "scripts/estado_diario/historico_estado_diario.rds",
                                    "scripts/llenado_completo/historico_llenado_completo.rds",
                                    "scripts/db/10393_ubicaciones/ubicaciones_con_thegheom.rds",
                                    "scripts/db/10393_ubicaciones/historico_ubicaciones.rds",
                                    "scripts/para_mapear/circuitos_planificados.rds",
                                    "modules/estado_diario.R",
                                    "modules/busqueda_gid.R",
                                    "modules/incidencias_spp.R",
                                    "modules/incidencias_grua.R",
                                    "modules/incidencias_pluma.R",
                                    "modules/incidencias_mantenimiento.R",
                                    "modules/incidencias_operativa.R",
                                    "modules/condicion_contenedor.R"),
                       appName = "Incidencias_2025")
  
  
  # asd <- historico_llenado %>% 
  #   filter(Circuito == "C_DU_RM_CL_102")
  # 
  # # Suponiendo que tu dataframe se llama 'asd'
  # asd_completo <- asd %>%
  #   mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>% 
  #   group_by(Circuito, Posicion) %>%
  #   complete(
  #     Fecha = seq.Date(min(Fecha), max(Fecha), by = "day")
  #   ) %>%
  #   ungroup()
  # 
  # funcion_imprimir_datosporgid("179772")
  # funcion_imprimir_datosporgid("180687")
  # 
  # 
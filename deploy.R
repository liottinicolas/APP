source("global.R")
source("carga_BD.R")
source("funciones.R")


# Código para desplegar la aplicación
# Configura la cuenta (descomenta y rellena con tus datos)
rsconnect::setAccountInfo(name='datoseindicadores',
                          token='C9EC0DCFA432ECC14F2438D893828344',
                          secret='b6YjR35qtht+AisTyTGAL5T8bZRzg1D+2go+6pPC')

# Despliega la aplicación (descomenta esta línea para desplegar)
#rsconnect::deployApp()

rsconnect::deployApp(appDir = ".",
                     appFiles = c("app.R","global.R","funciones.R",
                                  "/scripts/informe_del_dia/historico_informe_diario.rds", 
                                  "/scripts/incidencias_por_gid/historico_incidencias_completas.rds",
                                  "/scripts/incidencias_por_gid/historico_levantes_completos.rds",
                                  "/scripts/incidencias_por_gid/historico_no_levantes_completos.rds",
                                  "/scripts/incidencias_por_gid/graficas_incidencias/historicos_pendientes_generados_GRUA.rds",
                                  "/scripts/incidencias_por_gid/graficas_incidencias/historicos_pendientes_generados_PLUMA.rds",
                                  "modules/estado_diario.R",
                                  "modules/busqueda_gid.R",
                                  "modules/incidencias_spp.R",
                                  "modules/incidencias_grua.R",
                                  "modules/incidencias_pluma.R",
                                  "modules/incidencias_mantenimiento.R",
                                  "modules/incidencias_operativa.R",
                                  "modules/historico_incidencias.R",
                                  "modules/condicion_contenedor.R"),
                     appName = "Incidencias")



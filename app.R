# nolint start: line_length_linter, object_name_linter

source("config.R")
source("logging.R")
source("global.R")
source("carga_informacion_web.R")
source("funciones_para_web.R")
source("funciones_utiles.R")

#source("carga_BD.R")

# Cargar módulos
source("modules/estado_diario.R")
source("modules/busqueda_gid.R")
source("modules/incidencias_spp.R")
source("modules/incidencias_grua.R")
source("modules/incidencias_pluma.R")
source("modules/incidencias_mantenimiento.R")
source("modules/incidencias_operativa.R")
source("modules/condicion_contenedor.R")
source("modules/reportes_incidencias.R")
source("modules/incidencias_diarias.R")

# ---- UI ----  
ui <- dashboardPage(
  
  skin = "blue",  # Cambia el color principal del tema
  
  dashboardHeader(title = "Incidencias"),
      
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estado diario", tabName = "estado_diario", icon = icon("globe")),
      menuItem("Busqueda por GID", tabName = "busqueda_gid", icon = icon("search")),
      menuItem("Incidencias", tabName = "incidencias", icon = icon("exclamation-triangle"),
               menuSubItem("SPP", tabName = "incidencias_spp", icon = icon("calendar")),
               menuSubItem("Grua", tabName = "incidencias_grua", icon = icon("piggy-bank")),
               menuSubItem("Pluma", tabName = "incidencias_pluma", icon = icon("leaf")),
               menuSubItem("Mantenimiento", tabName = "incidencias_mantenimiento", icon = icon("wrench")),
               menuSubItem("Operativa", tabName = "incidencias_operativa", icon = icon("road")),
               menuSubItem("Reportes", tabName = "reportes_incidencias", icon = icon("file-alt")),
               menuSubItem("Incidencias diarias", tabName = "incidencias_diarias", icon = icon("calendar"))
               ),
      menuItem("Condicion contenedor", tabName = "condicion_contenedor", icon = icon("signal"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "estado_diario", estadoDiarioUI("estadoDiario")),
      tabItem(tabName = "busqueda_gid", busquedaGidUI("busquedaGid")),
       tabItem(tabName = "incidencias_spp", incidenciasSPPUI("incidenciasSPP")),
       tabItem(tabName = "incidencias_grua", incidenciasGruaUI("incidenciasGRUA")),
       tabItem(tabName = "incidencias_pluma", incidenciasPlumaUI("incidenciasPLUMA")),
       tabItem(tabName = "incidencias_mantenimiento", incidenciasMantenimientoUI("incidenciasMantenimiento")),
       tabItem(tabName = "incidencias_operativa", incidenciasOperativaUI("incidenciasOperativa")),
       tabItem(tabName = "condicion_contenedor", condicionContenedorUI("condicionContenedor")),
       tabItem(tabName = "reportes_incidencias", reportesIncidenciasUI("reportesIncidencias")),
       tabItem(tabName = "incidencias_diarias", incidenciasGeneralUI("incidenciasGeneral"))
     )
    
    
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  callModule(estadoDiarioServer, "estadoDiario")
  callModule(busquedaGidServer, "busquedaGid")
  callModule(incidenciasSPPServer, "incidenciasSPP")
  callModule(incidenciasGruaServer, "incidenciasGRUA")
  callModule(incidenciasPlumaServer, "incidenciasPLUMA")
  callModule(incidenciasMantenimientoServer, "incidenciasMantenimiento")
  callModule(incidenciasOperativaServer, "incidenciasOperativa")
  callModule(condicionContenedorServer, "condicionContenedor")
  callModule(reportesIncidenciasServer, "reportesIncidencias")
  callModule(incidenciasGeneralServer, "incidenciasGeneral")
}
  
shinyApp(ui, server)
# 
# # funcion_imprimir_datosporgid(105531)
# 
# # Definir la fecha a filtrar (puedes cambiar esta variable según necesites)
# fecha_filtro <- as.Date("2025-05-13")  # Ejemplo de fecha, cámbiala según necesites
# 
# # Filtrar los contenedores levantados por día y agrupar por circuito
# contenedores_levantados <- historico_estado_diario %>%
#   filter(Acumulacion == 1) %>% 
#   filter(Fecha == fecha_filtro) %>%  # Filtrar por la fecha especificada
#   group_by(Circuito, Id_viaje) %>%
#   summarise(
#     cantidad_levantados = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(cantidad_levantados))
# 
# # Mostrar el resultado
# print(contenedores_levantados)
# 
# cont <- contenedores_levantados %>%
#   left_join(historico_viajes, by = c("Id_viaje", "Circuito")) %>% 
#   filter(Peso_neto <= 0)
# # nolint end
# 
# contenedores_levantados <- web_historico_estado_diario %>%
#   filter(Fecha == "2025-05-19") %>% 
#     filter(Acumulacion == 1) %>%
#     filter(is.na(Estado)) %>%
#     left_join(
#       web_historico_completo_llenado_incidencias %>% 
#         select(gid, Fecha, Porcentaje_llenado),
#       by = c("gid", "Fecha")
#     ) %>%
#     filter(!is.na(Porcentaje_llenado))
# 
# contenedores_1 <- web_historico_estado_diario %>%
#   filter(Fecha == "2025-05-19") %>% 
#   filter(Acumulacion == 1) %>%
#   filter(is.na(Estado)) %>%
#   left_join(
#     web_historico_completo_llenado_incidencias %>% 
#       select(gid, Fecha, Porcentaje_llenado),
#     by = c("gid", "Fecha")
#   ) %>%


# 
# asd <- historico_estado_diario %>%
#   filter(Fecha == "2025-05-21") %>%
#   filter(is.na(Estado)) %>% 
#   filter(!is.na(Acumulacion))


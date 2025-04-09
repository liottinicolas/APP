# nolint start: line_length_linter, object_name_linter


library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

source("config.R")
source("logging.R")
source("global.R")
source("carga_informacion_web.R")
source("funciones_para_web.R")
source("funciones_utiles.R")

# source("carga_BD.R")

# Cargar módulos
source("modules/estado_diario.R")
source("modules/busqueda_gid.R")
source("modules/incidencias_spp.R")
source("modules/incidencias_grua.R")
source("modules/incidencias_pluma.R")
source("modules/incidencias_mantenimiento.R")
source("modules/incidencias_operativa.R")
# source("modules/historico_incidencias.R")
source("modules/condicion_contenedor.R")

# ---- UI ----  
ui <- dashboardPage(
  
  skin = "green",  # Cambia el color principal del tema
  
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
               menuSubItem("Operativa", tabName = "incidencias_operativa", icon = icon("road"))
               ),
      #menuItem("Historicos incidencias", tabName = "historico_incidencias", icon = icon("tasks")),
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
    #   tabItem(tabName = "historico_incidencias", historicoIncidenciasUI("historicoIncidencias")),
       tabItem(tabName = "condicion_contenedor", condicionContenedorUI("condicionContenedor"))
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
  # callModule(historicoIncidenciasServer, "historicoIncidencias")
  callModule(condicionContenedorServer, "condicionContenedor")
}
  
shinyApp(ui, server)



# nolint end
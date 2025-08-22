library(shiny)
library(shinydashboard)

# Cargar módulos necesarios
source("config.R")
source("logging.R")
source("global.R")
source("carga_informacion_web.R")
source("funciones_para_web.R")
source("funciones_utiles.R")
source("modules/estado_diario.R")
source("modules/busqueda_gid.R")
source("modules/reportes_incidencias.R")

# ---- UI ----  
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Incidencias"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estado diario", tabName = "estado_diario", icon = icon("globe")),
      menuItem("Busqueda por GID", tabName = "busqueda_gid", icon = icon("search")),
      menuItem("Reportes", tabName = "reportes_incidencias", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "estado_diario", estadoDiarioUI("estadoDiario")),
      tabItem(tabName = "busqueda_gid", busquedaGidUI("busquedaGid")),
      tabItem(tabName = "reportes_incidencias", reportesIncidenciasUI("reportesIncidencias"))
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  callModule(estadoDiarioServer, "estadoDiario")
  callModule(busquedaGidServer, "busquedaGid")
  callModule(reportesIncidenciasServer, "reportesIncidencias")
}

shinyApp(ui, server)

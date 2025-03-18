incidenciasUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          ns("fecha_incidencias"),
          "Rango de fechas:",
          start = Sys.Date() - 7,
          end = Sys.Date(),
          min = Sys.Date() - 60,
          max = Sys.Date()
        ),
        actionButton(ns("btn_buscar"), "Buscar")
      ),
      mainPanel(DTOutput(ns("tabla_incidencias")))
    )
  )
}

incidenciasServer <- function(input, output, session) {
  ns <- session$ns
  
  incidencias_filtradas <- reactive({
    # Simulación de datos para este ejemplo
    data.frame(
      Dia = Sys.Date() - sample(1:7, 10, replace = TRUE),
      gid = 1:10,
      Municipio = "Montevideo",
      Circuito = paste("Circuito", 1:10),
      Direccion = paste("Dirección", 1:10),
      Descripcion = paste("Incidencia", 1:10)
    )
  })
  
  output$tabla_incidencias <- renderDT({
    datatable(incidencias_filtradas())
  })
}

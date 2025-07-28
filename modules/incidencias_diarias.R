# nolint start: line_length_linter, object_name_linter

#' Módulo de Incidencias Diarias
#' 
#' Este módulo permite visualizar y analizar incidencias diarias.

incidenciasGeneralUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        h3("Incidencias Diarias", style = "text-align: center; color: #2c3e50; margin-bottom: 30px; font-weight: bold;")
      )
    ),
    # Filtros
    div(
      class = "well",
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
      fluidRow(
        column(
          width = 4,
          dateInput(
            ns("filtro_fecha"),
            "Selecciona una fecha:",
            value = max(web_historico_completo_llenado_incidencias$Fecha),
            min = as.Date("2024-10-10"),
            max = max(web_historico_completo_llenado_incidencias$Fecha),
            width = '100%'
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("filtro_municipio"),
            "Municipio:",
            choices = NULL,
            multiple = TRUE,
            width = '100%'
          )
        ),
        column(
          width = 4,
          selectInput(
            ns("filtro_circuito"),
            "Circuito:",
            choices = NULL,
            multiple = TRUE,
            width = '100%'
          )
        )
      )
    ),
    # Tabla
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_incidencias"))
        )
      )
    )
  )
}

incidenciasGeneralServer <- function(input, output, session) {
  ns <- session$ns
  
  # Reactiva para obtener los municipios únicos
  municipios_unicos <- reactive({
    req(web_historico_completo_llenado_incidencias)
    sort(unique(web_historico_completo_llenado_incidencias$Municipio))
  })
  
  # Actualizar las opciones del select de municipios
  observe({
    updateSelectInput(
      session,
      "filtro_municipio",
      choices = municipios_unicos(),
      selected = NULL
    )
  })
  
  # Reactiva para obtener los circuitos según el municipio seleccionado
  circuitos_filtrados <- reactive({
    req(input$filtro_municipio)
    req(web_historico_completo_llenado_incidencias)
    
    if (length(input$filtro_municipio) > 0) {
      circuitos <- web_historico_completo_llenado_incidencias %>%
        filter(Municipio %in% input$filtro_municipio) %>%
        pull(Circuito_corto) %>%
        unique() %>%
        sort()
    } else {
      circuitos <- sort(unique(web_historico_completo_llenado_incidencias$Circuito_corto))
    }
    return(circuitos)
  })
  
  # Actualizar las opciones del select de circuitos
  observe({
    updateSelectInput(
      session,
      "filtro_circuito",
      choices = circuitos_filtrados(),
      selected = NULL
    )
  })
  
  # Reactiva para los datos filtrados por fecha
  datos_filtrados <- reactive({
    req(input$filtro_fecha)
    
    # Convertir fecha al formato numérico (YYMMDD)
    fecha_filtro <- as.Date(input$filtro_fecha)
    
    # Filtrar datos por fecha y Levantado distinto a S (incluyendo NA)
    datos_filtrados <- web_historico_completo_llenado_incidencias %>%
      filter(Fecha == fecha_filtro) %>%
      filter(Levantado != "S" | is.na(Levantado))
    
    # Aplicar filtros de municipio y circuito
    if (length(input$filtro_municipio) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(Municipio %in% input$filtro_municipio)
    }
    
    if (length(input$filtro_circuito) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(Circuito_corto %in% input$filtro_circuito)
    }
    
    return(datos_filtrados)
  })
  
  # Tabla de incidencias
  output$tabla_incidencias <- renderDT({
    df <- datos_filtrados()
    
    # Seleccionar columnas específicas
    df <- df %>%
      select(gid, Municipio, Circuito_corto, Posicion, Direccion, Observaciones, Turno, Id_viaje, Incidencia)
    
    # Configuración de anchos de columnas
    columnDefs <- list(
      list(width = '8%', targets = 0, className = 'dt-center'),    # gid
      list(width = '8%', targets = 1, className = 'dt-center'),    # Municipio
      list(width = '8%', targets = 2, className = 'dt-center'),    # Circuito_corto
      list(width = '6%', targets = 3, className = 'dt-center'),    # Posicion
      list(width = '25%', targets = 4),                           # Direccion
      list(width = '20%', targets = 5),                           # Observaciones
      list(width = '8%', targets = 6, className = 'dt-center'),    # Turno
      list(width = '8%', targets = 7, className = 'dt-center'),    # Id_viaje
      list(width = '9%', targets = 8, className = 'dt-center')     # Incidencia
    )
    
    datatable(df,
              filter = "top",
              selection = "single",
              rownames = FALSE,
              options = list(
                pageLength = 50,
                scrollX = TRUE,
                columnDefs = columnDefs
              ))
  })
}

# nolint end 
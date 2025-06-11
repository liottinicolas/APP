# nolint start: line_length_linter, object_name_linter

incidenciasSPPUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
        dateRangeInput(
          inputId = ns("fecha_spp"),
          label = "Seleccione un rango de fechas",
          start = ultima_fecha_registro,
          end = ultima_fecha_registro,
          min = inicio,
          max = ultima_fecha_registro
        ),
        checkboxGroupInput(
          inputId = ns("checkbox_spp"),
          label = "Seleccione opciones",
          choices = cargar_opciones_responsable("Seguimiento SPP"),
          selected = cargar_opciones_responsable("Seguimiento SPP")
        ),
        actionButton(ns("eliminar_duplicados"), "Eliminar Duplicados", 
                    class = "btn-primary", style = "margin-bottom: 15px;"),
        downloadButton(ns("descargar_csv_spp"), "Descargar CSV")
      ),
    
      
        column(
          width = 10,
          div(
            style = "width: 95%; margin: 0 auto;",
            DTOutput(ns("tabla_spp"))
          )
        )
      
    )
  )
}

incidenciasSPPServer <- function(input, output, session) {
  ns <- session$ns
  
  # Variable reactiva para controlar si se deben eliminar duplicados
  eliminar_duplicados <- reactiveVal(FALSE)
  
  # Observador para el botón de eliminar duplicados
  observeEvent(input$eliminar_duplicados, {
    eliminar_duplicados(!eliminar_duplicados())
  })
  
  filtrado_reactivo_spp <- reactive({
    req(input$fecha_spp)
    
    datos_filtrados <- web_historico_completo_llenado_incidencias %>%
      filter(Responsable == "Seguimiento SPP") %>% 
      filter(Fecha >= input$fecha_spp[1]) %>% 
      filter(Fecha <= input$fecha_spp[2]) %>% 
      filter(gid > 0) %>% 
      filter(Incidencia %in% input$checkbox_spp)
    
    # Si el botón está activado, eliminar duplicados y contar repeticiones
    if (eliminar_duplicados()) {
      # Primero contar las repeticiones
      conteo_repeticiones <- datos_filtrados %>%
        group_by(gid, Incidencia) %>%
        summarise(Repeticiones = n(), .groups = 'drop')
      
      # Luego obtener el registro más reciente
      datos_filtrados <- datos_filtrados %>%
        group_by(gid) %>%
        slice_max(Fecha, n = 1) %>%
        ungroup() %>%
        # Unir con el conteo de repeticiones
        left_join(conteo_repeticiones, by = c("gid", "Incidencia")) %>%
        # Unir con el estado diario para obtener Acumulacion y Estado
        left_join(
          web_historico_estado_diario %>% 
            select(gid, Fecha, Acumulacion, Estado),
          by = c("gid", "Fecha")
        )
    }
    
    return(datos_filtrados)
  })
  
  ### Mensaje de error si la fecha de la pluma está mal ingresada
  output$error_spp <- renderText({
    # Si la fecha de inicio es mayor que la fecha de fin, muestra un mensaje de error
    if (!is.null(input$fecha_spp) && input$fecha_spp[1] > input$fecha_spp[2]) {
      return("Error: La fecha de inicio no puede ser mayor que la fecha de fin.")
    }
    NULL
  })
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_spp <- renderDT({
    datos <- filtrado_reactivo_spp()
    
    # Seleccionar columnas según si el botón está activado
    if (eliminar_duplicados()) {
      datos <- datos %>% 
        select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia, 
               Repeticiones, Acumulacion, Estado) %>% 
        rename(Circuito = Circuito_corto)
      
      column_defs <- list(
        list(width = '10%', targets = 0, className = 'dt-center'),  # Dia
        list(width = '5%', targets = 1, className = 'dt-center'),  # gid
        list(width = '5%', targets = 2, className = 'dt-center'),  # municipio
        list(width = '5%', targets = 3, className = 'dt-center'), # Circuito
        list(width = '5%', targets = 4, className = 'dt-center'), #Posicion
        list(width = '20%', targets = 5), # Direccion
        list(width = '15%', targets = 6), # Descripcion
        list(width = '5%', targets = 7, className = 'dt-center'), # Repeticiones
        list(width = '5%', targets = 8, className = 'dt-center'), # Acumulacion
        list(width = '5%', targets = 9, className = 'dt-center')  # Estado
      )
    } else {
      datos <- datos %>% 
        select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
        rename(Circuito = Circuito_corto)
      
      column_defs <- list(
        list(width = '10%', targets = 0, className = 'dt-center'),  # Dia
        list(width = '5%', targets = 1, className = 'dt-center'),  # gid
        list(width = '5%', targets = 2, className = 'dt-center'),  # municipio
        list(width = '5%', targets = 3, className = 'dt-center'), # Circuito
        list(width = '5%', targets = 4, className = 'dt-center'), #Posicion
        list(width = '20%', targets = 5), # Direccion
        list(width = '15%', targets = 6) # Descripcion
      )
    }
    
    datatable(
      datos,
      filter = "top",         # Agrega filtros en la parte superior de cada columna
      options = list(
        lengthMenu = c(10, 25, 50, 100),
        pageLength = 100,
        autoWidth = TRUE,
        columnDefs = column_defs
      ),
      rownames = FALSE
    )
  })
  
  output$descargar_csv_spp <- downloadHandler(
    filename = function() {
      paste("incidencias_spp_", input$fecha_spp[1], "_",input$fecha_spp[2], ".csv", sep = "")
    },
    content = function(file) {
      datos <- filtrado_reactivo_spp()
      
      if (eliminar_duplicados()) {
        datos <- datos %>% 
          select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia, 
                 Repeticiones, Acumulacion, Estado) %>% 
          rename(Circuito = Circuito_corto)
      } else {
        datos <- datos %>% 
          select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
          rename(Circuito = Circuito_corto)
      }
      
      write.csv(datos, file, row.names = FALSE)
    }
  )
}
# nolint end

incidenciasOperativaUI <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
      
      dateRangeInput(
        inputId = ns("fecha_Operativa"),
        label = "Seleccione un rango de fechas",
        start = ultima_fecha_registro,
        end = ultima_fecha_registro,
        min = inicio,
        max = ultima_fecha_registro
      ),
      
      
      # Agregar los botones para seleccionar y deseleccionar
      actionButton(ns("seleccionar_todo_operativa"), "Seleccionar Todo"),
      actionButton(ns("deseleccionar_todo_operativa"), "Deseleccionar Todo"),
      
      checkboxGroupInput(
        inputId = ns("checkbox_Operativa"),
        label = "Seleccione opciones",
        choices = cargar_opciones_responsable("Operativa"),
        selected = cargar_opciones_responsable("Operativa")
      ),
      
      # Agregar el botón de descarga
      downloadButton(ns("descargar_csv_operativa"), "Descargar CSV")
      
      ),

      column(
        width = 10,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_Operativa"))
        )
      )
      
    )

      
      
  )
  
}

incidenciasOperativaServer <- function(input, output, session) {
  ns <- session$ns
  
  filtrado_reactivo_Operativa <- reactive({
    req(input$fecha_Operativa)
    web_historico_completo_llenado_incidencias %>%
      filter(Responsable == "Operativa") %>% 
      filter(Fecha >= input$fecha_Operativa[1]) %>% 
      filter(Fecha <= input$fecha_Operativa[2]) %>% 
      filter(Incidencia %in% input$checkbox_Operativa)  # Filtra según los valores seleccionados en el checkbox
    
  })
  
  
  observeEvent(input$seleccionar_todo_operativa, {
    updateCheckboxGroupInput(session, "checkbox_Operativa", selected = cargar_opciones_responsable("Operativa"))
  })
  
  observeEvent(input$deseleccionar_todo_operativa, {
    updateCheckboxGroupInput(session, "checkbox_Operativa", selected = character(0))
  })
  
  
  ### Mensaje de error si la fecha de la Operativa está mal ingresada
  output$error_Operativa <- renderText({
    # Si la fecha de inicio es mayor que la fecha de fin, muestra un mensaje de error
    if (!is.null(input$fecha_Operativa) && input$fecha_Operativa[1] > input$fecha_Operativa[2]) {
      return("Error: La fecha de inicio no puede ser mayor que la fecha de fin.")
    }
    NULL
  })
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_Operativa <- renderDT({
    filtrado_reactivo_Operativa() %>% 
      select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
      rename(Circuito = Circuito_corto) %>% 
      datatable(
        filter = "top",         # Agrega filtros en la parte superior de cada columna
        options = list(lengthMenu = c(10, 25, 50, 100),
                       pageLength = 100,
                       columnDefs = list(
                         list(width = '10%', targets = 0,className = 'dt-center'),  # Dia
                         list(width = '5%', targets = 1,className = 'dt-center'),  # gid
                         list(width = '5%', targets = 2,className = 'dt-center'),  # municipio
                         list(width = '5%', targets = 3,className = 'dt-center'), # Circuito
                         list(width = '5%', targets = 4, className = 'dt-center'), #Posicion
                         list(width = '20%',targets = 5), # Direccion
                         list(width = '15%', targets = 6) # Descripcion
                       )),
        rownames = FALSE
      )
  })
  
  
  
  # 
  # output$descargar_csv_operativa <- downloadHandler(
  #   filename = function() {
  #     paste("incidencias_mantenimiento_", input$fecha_Operativa[1], "_",input$fecha_Operativa[2], ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(filtrado_reactivo_Operativa(), file, row.names = FALSE)
  #   }
  # )
  
  output$descargar_csv_operativa <- downloadHandler(
    filename = function() {
      paste("incidencias_operativa_", input$fecha_Operativa[1], "_",input$fecha_Operativa[2], ".csv", sep = "")
    },
    content = function(file) {
      
      datos <- filtrado_reactivo_Operativa() %>% 
        select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
        rename(Circuito = Circuito_corto)
      
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
  
  
 
  
  
}

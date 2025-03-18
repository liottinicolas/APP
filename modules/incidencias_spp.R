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
  
  filtrado_reactivo_spp <- reactive({
    req(input$fecha_spp)
    historico_incidencias_por_gid %>%
      filter(Responsable == "Seguimiento SPP") %>% 
      filter(Fecha_incidencia >= input$fecha_spp[1]) %>% 
      filter(Fecha_incidencia <= input$fecha_spp[2]) %>% 
      filter(Incidencia %in% input$checkbox_spp)  # Filtra según los valores seleccionados en el checkbox
    
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
    filtrado_reactivo_spp() %>% 
      select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion ,Direccion, Incidencia) %>% 
      rename(Fecha = Fecha_incidencia,
             Circuito = Circuito_corto) %>% 
      datatable(
        filter = "top",         # Agrega filtros en la parte superior de cada columna
        options = list(lengthMenu = c(10, 25, 50, 100),
                       pageLength = 100,
                       autoWidth = TRUE,
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
  
  # output$descargar_csv_spp <- downloadHandler(
  #   filename = function() {
  #     paste("incidencias_spp_", input$fecha_spp[1], "_",input$fecha_spp[2], ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(filtrado_reactivo_spp(), file, row.names = FALSE)
  #   }
  # ) 
  # 
  output$descargar_csv_spp <- downloadHandler(
    filename = function() {
      paste("incidencias_spp_", input$fecha_spp[1], "_",input$fecha_spp[2], ".csv", sep = "")
    },
    content = function(file) {
      
      datos <- filtrado_reactivo_spp() %>% 
        select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
        rename(Circuito = Circuito_corto)
      
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
  
}

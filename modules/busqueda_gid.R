# nolint start: line_length_linter, object_name_linter

busquedaGidUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Panel que ocupa todo el ancho de la página
    fluidRow(
      column(
        width = 3,
        sidebarPanel(
          width = NULL,
          textInput(ns("txt_busqueda_gid"), "Ingrese GID para buscar"),
          dateRangeInput(
            inputId = ns("fecha_busqueda_gid"),
            label = "Seleccione un rango de fechas",
            start = ultima_fecha_registro - 15,
            end = ultima_fecha_registro,
            min = ultima_fecha_registro-50,
            max = ultima_fecha_registro
          ),
          actionButton(inputId = ns("btn_buscar_porgid"), label = "Buscar")
        ),
        #style = "background-color: #f9f9f9; height: 100%; padding: 15px;" # Color de fondo y espacio interno
      ),
      column(
        width = 9,
        div(style = "height: 100%;") # Espacio vacío
      )
    ),
    # Contenido que se muestra debajo
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_historico_contenedor"))
        )
      )
    )
  )
}
    
  #   
  #   
  #   
  #   sidebarLayout(
  #     sidebarPanel(
  #       dateRangeInput(
  #         ns("fecha_incidencias"),
  #         "Rango de fechas:",
  #         start = Sys.Date() - 7,
  #         end = Sys.Date(),
  #         min = Sys.Date() - 60,
  #         max = Sys.Date()
  #       ),
  #       actionButton(ns("btn_buscar"), "Buscar")
  #     ),
  #     mainPanel(DTOutput(ns("tabla_incidencias")))
  #   )
  # )


busquedaGidServer <- function(input, output, session) {
  ns <- session$ns
  
  # observeEvent(input$btn_buscar_porgid, {
  #   print("Botón Buscar presionado")
  # })
  
  # Reactive para la búsqueda
  llenado <- eventReactive(input$btn_buscar_porgid, {
    req(input$txt_busqueda_gid)  # Asegura que el input no esté vacío

    web_historico_completo_llenado_incidencias %>%
      filter(gid == input$txt_busqueda_gid) %>%
      filter(Fecha >= input$fecha_busqueda_gid[1]) %>%
      filter(Fecha <= input$fecha_busqueda_gid[2]) %>% 
      arrange(desc(Fecha),desc(Turno))
  })

    
  
  # # Mostrar los datos filtrados en la tabla
  output$tabla_historico_contenedor <- renderDT({
    data <- llenado()  # Asigna el resultado del eventReactive a `data`

    if (nrow(data) > 0) {

      # transformo el dia-fecha a solo hora
      data$Fecha_hora_pasaje <- ifelse(is.na(data$Fecha_hora_pasaje), "", format(data$Fecha_hora_pasaje, "%H:%M:%S"))

      data %>%
        select(Fecha,Circuito,Posicion,Direccion,Levantado,Turno,Fecha_hora_pasaje,Id_viaje,Incidencia,
               Porcentaje_llenado,Condicion) %>%
        datatable(
          colnames = c("Dia","Circuito", "Posicion", "Dirección","Levante", "Turno", "Hora", "ID_Viaje", "Incidencia", "% Llenado", "Condición"),
          filter = "top",
          options = list(lengthMenu = c(10, 25, 50, 100),
                         pageLength = 100,
                         #scrollX = TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(
                           list(width = '10%', targets = 0,className = 'dt-center'),  # Dia
                           list(width = '5%', targets = 1,className = 'dt-center'),  # Circuito
                           list(width = '5%', targets = 2,className = 'dt-center'),  # Posicion
                           list(width = '5%', targets = 3,className = 'dt-center'), # Direccion
                           list(width = '20%', targets = 4), #Levante
                           list(targets = 5,className = 'dt-center'), # Turno
                           list(width = '5%', targets = 6,className = 'dt-center'), # Hora
                           list(width = '5%', targets = 7,className = 'dt-center'), # ID_Viaje
                           list(width = '20%', targets = 8,className = 'dt-center'), #Incidencia
                           list(width = '5%', targets = 9), # llenado
                           list(width = '20%',targets = 10,className = 'dt-center') # Condicion

                         )



          ),
          rownames = FALSE
        )
    } else {
      datatable(data.frame(message = "Sin datos para mostrar"),
                options = list(pageLength = 1, dom = 't'),
                rownames = FALSE)
    }
  })
}


# nolint end
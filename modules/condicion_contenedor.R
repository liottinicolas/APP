condicionContenedorUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
      
      dateInput(
        ns("filtro_fecha_condicion"),
        "Selecciona una fecha:",
          value = max(historico_completo_llenado_incidencias$Fecha),
        # Valor predeterminado
        min = as.Date("2024-10-10"),
        max = max(historico_completo_llenado_incidencias$Fecha),
        width = '120px'
      ),
      
      radioButtons( 
        inputId = ns("radioBtn_condiciones"), 
        label = "Seleccione opciones", 
        choices = cargar_opciones_condiciones()
      ), 
      
      # checkboxGroupInput(
      #   inputId = "checkbox_condiciones",
      #   label = "Seleccione opciones",
      #   choices = cargar_opciones_condiciones(),
      #   selected = cargar_opciones_condiciones() # Selecciona todas por defecto
      # )
      
      ),
      
      column(
        width = 10,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_condicion_contenedores"))
        )
      )
      
    )
      

  
  )
}

condicionContenedorServer <- function(input, output, session) {
  ns <- session$ns
  
  filtrado_reactivo_condicion_contenedor <- reactive({
    req(input$filtro_fecha_condicion) # Asegúrate de que el filtro de fecha esté disponible
    
    # Filtra inicialmente por la fecha seleccionada y asegura que 'Condicion_contenedor' no esté vacío
    df_condiciones_contenedor <- historico_completo_llenado_incidencias %>%
      filter(Fecha == input$filtro_fecha_condicion) %>% 
      filter(Condicion != "")
    
    # Condición para "Todos": si se selecciona, no aplicar filtro en 'Condicion_contenedor'
    if (input$radioBtn_condiciones != "Todos") {
      df_condiciones_contenedor <- df_condiciones_contenedor %>%
        filter(str_detect(Condicion, fixed(input$radioBtn_condiciones, ignore_case = TRUE)))
    }
    
    # Devuelve el dataframe filtrado o completo si se seleccionó "Todos"
    return(df_condiciones_contenedor)
  })
  
  
  output$tabla_condicion_contenedores <- renderDT({
    filtrado_reactivo_condicion_contenedor() %>% 
      select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Turno_levantado,Porcentaje_llenado,Condicion) %>% 
      # rename(Dia = Dia_incidencia,
      #        Circuito = Circuito_corto) %>% 
      datatable(
        filter = "top",         # Agrega filtros en la parte superior de cada columna
        options = list(lengthMenu = c(10, 25, 50, 100),
                       pageLength = 100),
        rownames = FALSE
      )
  })
  
}

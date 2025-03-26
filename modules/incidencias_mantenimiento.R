incidenciasMantenimientoUI <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
        
      
      dateRangeInput(
        inputId = ns("fecha_Mantenimiento"),
        label = "Seleccione un rango de fechas",
        start = ultima_fecha_registro,
        end = ultima_fecha_registro,
        min = inicio,
        max = ultima_fecha_registro
      ),
      
      checkboxGroupInput(
        inputId = ns("checkbox_Mantenimiento"),
        label = "Seleccione opciones",
        choices = cargar_opciones_responsable("Mantenimiento"),
        selected = cargar_opciones_responsable("Mantenimiento")
      ),
      
      # Agregar el botón de descarga
      downloadButton(ns("descargar_csv_mantenimiento"), "Descargar CSV")
  ),
  
  column(
    width = 10,
    div(
      style = "width: 95%; margin: 0 auto;",
      leafletOutput(ns("map_mantenimiento"), height = "700px"),
    )
  ),
    

  fluidRow(
    column(
      width = 12,
      div(
        style = "width: 95%; margin: 0 auto;",
        DTOutput(ns("tabla_Mantenimiento"))
        )
    )
  )
      
      
)
      
    
  )
  
}

incidenciasMantenimientoServer <- function(input, output, session) {
  ns <- session$ns
  
  filtrado_reactivo_Mantenimiento <- reactive({
    req(input$fecha_Mantenimiento)
    # historico_incidencias_por_gid %>%
    #   filter(Responsable == "Mantenimiento") %>% 
    #   filter(Fecha_incidencia >= input$fecha_Mantenimiento[1]) %>% 
    #   filter(Fecha_incidencia <= input$fecha_Mantenimiento[2]) %>% 
    #   filter(Incidencia %in% input$checkbox_Mantenimiento)  # Filtra según los valores seleccionados en el checkbox
    # 
    # df_final <- funcion_mostrar_responsables_por_incidencias(historico_incidencias_por_gid,
    #                                                          historico_estado_diario,
    #                                                          inicio,
    #                                                          fin,
    #                                                          "Mantenimiento")
    
    
        df_final <- funcion_mostrar_responsables_por_incidencias(historico_completo_llenado_incidencias,
                                                             historico_estado_diario,
                                                             input$fecha_Mantenimiento[1],
                                                             input$fecha_Mantenimiento[2],
                                                             "Mantenimiento")
    return(df_final)
  })
  
  ### Mensaje de error si la fecha de la Mantenimiento está mal ingresada
  output$error_Mantenimiento <- renderText({
    # Si la fecha de inicio es mayor que la fecha de fin, muestra un mensaje de error
    if (!is.null(input$fecha_Mantenimiento) && input$fecha_Mantenimiento[1] > input$fecha_Mantenimiento[2]) {
      return("Error: La fecha de inicio no puede ser mayor que la fecha de fin.")
    }
    NULL
  })
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_Mantenimiento <- renderDT({
    filtrado_reactivo_Mantenimiento() %>% 
      select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
      rename(Fecha = Fecha_incidencia,
             Circuito = Circuito_corto) %>% 
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
  
  output$descargar_csv_mantenimiento <- downloadHandler(
    filename = function() {
      paste("Incidencias_mantenimiento_", input$fecha_Mantenimiento[1], "_",input$fecha_Mantenimiento[2], ".csv", sep = "")
    },
    content = function(file) {
      
      datos <- filtrado_reactivo_Mantenimiento() %>% 
        select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia) %>% 
        rename(Circuito = Circuito_corto)
      
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
  
  output$map_mantenimiento <- renderLeaflet({
    # Filtrar y preparar los datos como antes
    datos <- filtrado_reactivo_Mantenimiento() %>% 
      filter(!is.na(the_geom))
    datos <- modificar_coordenadas_paramapa(datos)
    
    # Crear la paleta de colores si hay datos
    paleta_colores <- colorFactor(palette = "Set1", domain = datos$Incidencia)
    
    # Crear el mapa centrado en Montevideo en coordenadas geográficas
    mapa <- leaflet() %>%
      addTiles() %>%
      setView(lng = lng_montevideo, lat = lat_montevideo, zoom = 12) %>%   # Usar coordenadas lat/lon de Montevideo
      addMeasure(
        position = "topright",             # Ubicación del botón de medición
        primaryLengthUnit = "meters",      # Unidad de medida principal
        secondaryLengthUnit = "kilometers",# Unidad de medida secundaria
        activeColor = "#3D535D",           # Color al medir
        completedColor = "#7D4479",        # Color al completar la medición
        primaryAreaUnit = "sqmeters",      # Unidad de área principal
        secondaryAreaUnit = "hectares"     # Unidad de área secundaria
      )
    
    
    # Si hay datos, agregar los puntos al mapa
    if (nrow(datos) > 0) {
      mapa <- mapa %>%
        addCircleMarkers(data = datos, ~lon, ~lat, color = ~paleta_colores(Incidencia), 
                         radius = 5, fillOpacity = 0.8,
                         popup = ~paste("Incidencia:", Incidencia))
    }
    
    mapa  # Devolver el mapa, con o sin puntos
  }) 
  
  
  
 
  
  
}

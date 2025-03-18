incidenciasGruaUI <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
      
      dateRangeInput(
        inputId = ns("fecha_Grua"),
        label = "Seleccione un rango de fechas",
        start = ultima_fecha_registro,
        end = ultima_fecha_registro,
        min = inicio,
        max = ultima_fecha_registro
      ),
      
      checkboxGroupInput(
        inputId = ns("checkbox_Grua"),
        label = "Seleccione opciones",
        choices = cargar_opciones_responsable("Grua"),
        selected = cargar_opciones_responsable("Grua")
      ),
      
      switchInput(
        inputId = ns("switch_grua_levantados"),
        # ID del switch para hacer referencia en el servidor
        label = "Solo pendientes:",
        # Etiqueta visible para el usuario
        onLabel = "Sí",
        # Texto cuando el switch está activado
        offLabel = "No",
        # Texto cuando el switch está desactivado
        value = TRUE           # Estado inicial del switch (FALSE: apagado)
      ),
      
      
      
      # Agregar el botón de descarga
      downloadButton(ns("descargar_csv_grua"), "Descargar CSV")

    ),
    

    column(
      width = 10,
      div(
        style = "width: 95%; margin: 0 auto;",
        leafletOutput(ns("map_grua"), height = "700px"),
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_Grua"))       
          )
      )
    )
      
    )
  )
}

incidenciasGruaServer <- function(input, output, session) {
  ns <- session$ns

  filtrado_reactivo_Grua <- reactive({
    req(input$fecha_Grua)
    
    ## Aca filtro inicio y fin
    
    
    # df_final <- funcion_mostrar_responsables_por_incidencias(historico_incidencias_por_gid,
    #                                                          historico_estado_diario,
    #                                                          inicio,
    #                                                          fin,
    #                                                          "Grua")
    
    df_final <- funcion_mostrar_responsables_por_incidencias(historico_incidencias_por_gid,
                                                             historico_estado_diario,
                                                             input$fecha_Grua[1],
                                                             input$fecha_Grua[2],
                                                             "Grua")
    
    # df_final <- df_con_puntos_solucionados_eliminados(historico_incidencias_app,historico_informe_deldia,
    #                                                   "Grua",input$fecha_Grua[1],input$fecha_Grua[2],input$checkbox_Grua,
    #                                                   input$switch_grua_levantados)
    
    return(df_final)
    
  })
  
  observeEvent(input$fecha_Grua, {
    # Cambia el estado del switch a TRUE si una de las fechas ha sido seleccionada o cambiada
    updateSwitchInput(session, "switch_grua_levantados", value = TRUE)
  })
  
  ### Mensaje de error si la fecha de la Grua está mal ingresada
  output$error_Grua <- renderText({
    # Si la fecha de inicio es mayor que la fecha de fin, muestra un mensaje de error
    if (!is.null(input$fecha_Grua) && input$fecha_Grua[1] > input$fecha_Grua[2]) {
      return("Error: La fecha de inicio no puede ser mayor que la fecha de fin.")
    }
    NULL
  })
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_Grua <- renderDT({
    datos <- filtrado_reactivo_Grua()
    
    # # Seleccionar las columnas a mostrar dependiendo del estado del switch
    # if (input$switch_grua_levantados) {
    #   # Si el switch está activado, mostrar la columna 'Acumulacion'
    #   datos %>%
    #     select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia,Acumulacion) %>%
    #     rename(Fecha = Fecha_incidencia, Circuito = Circuito_corto) %>%
    #     arrange(desc(Acumulacion)) %>%
    #     datatable(
    #       filter = "top",         # Agrega filtros en la parte superior de cada columna
    #       options = list(lengthMenu = c(10, 25, 50, 100),
    #                      pageLength = 100,
    #                      autoWidth = TRUE,
    #                      columnDefs = list(
    #                        list(width = '10%', targets = 0,className = 'dt-center'),  # Dia
    #                        list(width = '5%', targets = 1,className = 'dt-center'),  # gid
    #                        list(width = '5%', targets = 2,className = 'dt-center'),  # municipio
    #                        list(width = '5%', targets = 3,className = 'dt-center'), # Circuito
    #                        list(width = '5%', targets = 4, className = 'dt-center'), #Posicion
    #                        list(width = '20%',targets = 5), # Direccion
    #                        list(width = '15%', targets = 6), # Descripcion
    #                        list(width = '5%', targets = 7, className = 'dt-center'), #Acumulacoin
    #                        list(width = '5%', targets = 8, className = 'dt-center') #Veces
    #                        
    #                      )),
    #       rownames = FALSE
    #     )
    # } else {
    #   # Si el switch está desactivado, mostrar la acumulación del día.
    #   datos %>%
    #     select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Incidencia, Acumulacion) %>%
    #     rename(Fecha = Fecha_incidencia, Circuito = Circuito_corto) %>%
    #     arrange(desc(Acumulacion)) %>%
    #     datatable(
    #       filter = "top",         # Agrega filtros en la parte superior de cada columna
    #       options = list(lengthMenu = c(10, 25, 50, 100),
    #                      pageLength = 100,
    #                      columnDefs = list(
    #                        list(width = '10%', targets = 0,className = 'dt-center'),  # Dia
    #                        list(width = '5%', targets = 1,className = 'dt-center'),  # gid
    #                        list(width = '5%', targets = 2,className = 'dt-center'),  # municipio
    #                        list(width = '5%', targets = 3,className = 'dt-center'), # Circuito
    #                        list(width = '5%', targets = 4, className = 'dt-center'), #Posicion
    #                        list(width = '20%',targets = 5), # Direccion
    #                        list(width = '15%', targets = 6), # Descripcion
    #                        list(width = '5%', targets = 7, className = 'dt-center') # Acumulacion
    #                      )),
    #       rownames = FALSE
    #     )
    # }
  })
  
  output$descargar_csv_grua <- downloadHandler(
    filename = function() {
      paste("Incidencias_grua_", input$fecha_Grua[1], "_", input$fecha_Grua[2], ".csv", sep = "")
    },
    content = function(file) {
      datos <- filtrado_reactivo_Grua()
      
      # if (input$switch_grua_levantados) {
      #   # Si el switch está activado, mostrar la columna 'Acumulacion'
      #   datos <- datos %>%
      #     select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Descripcion, Acumulacion, Veces_inc,the_geom) %>%
      #     arrange(desc(Veces_inc),desc(Acumulacion), Circuito_corto, Posicion)
      # } else {
      #   datos <- datos %>%
      #     select(Fecha_incidencia, gid, Municipio, Circuito_corto, Posicion, Direccion, Descripcion, Acumulacion,the_geom) %>%
      #     arrange(desc(Acumulacion))
      # }
      
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
  
  output$map_grua <- renderLeaflet({
    # Filtrar y preparar los datos como antes
    datos <- filtrado_reactivo_Grua() %>% 
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

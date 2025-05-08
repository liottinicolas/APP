# nolint start: line_length_linter, object_name_linter


incidenciasPlumaUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        width = 2, # Ancho de la barra lateral (3 columnas de las 12 disponibles)
      
    
      checkboxGroupInput(
        inputId = ns("checkbox_pluma"),
        label = "Seleccione opciones",
        choices = cargar_opciones_responsable("Pluma"),
        selected = cargar_opciones_responsable("Pluma")
      ),
      
      switchInput(
        inputId = ns("switch_pluma_levantados"),
        # ID del switch para hacer referencia en el servidor
        label = "Solo pendientes:",
        # Etiqueta visible para el usuario
        onLabel = "Sí",
        # Texto cuando el switch está activado
        offLabel = "No",
        # Texto cuando el switch está desactivado
        value = TRUE           # Estado inicial del switch (FALSE: apagado)
      ),

      
      # Agregar el botón para exportar Excel
      downloadButton(ns("exportar_excel_pluma"), "Exportar a Excel", 
                   class = "btn-primary", style = "margin-top: 15px;")
  ),
  
  column(
    width = 10,
    div(
      style = "width: 95%; margin: 0 auto;",
      leafletOutput(ns("map_pluma"), height = "700px"),
      )
  ),
      
  fluidRow(
    column(
      width = 12,
      div(
        style = "width: 95%; margin: 0 auto;",
        DTOutput(ns("tabla_pluma"))
      )
    )
  )

  
  )
  )
}

incidenciasPlumaServer <- function(input, output, session) {
  ns <- session$ns
  
  # ---- Filtro Reactivo: Pluma ----
  filtrado_reactivo_pluma <- reactive({
    df_final <- funcion_mostrar_responsables_por_incidencias(web_historico_completo_llenado_incidencias,
                                                             web_historico_estado_diario,
                                                             ultima_fecha_registro,
                                                             "Pluma")$datos
    
    # Depuración para ver qué contiene el checkbox
    print("Valores seleccionados en checkbox:")
    print(input$checkbox_pluma)
    
    # Valores únicos en la columna Incidencia
    print("Valores únicos en Incidencia:")
    print(unique(df_final$Incidencia))
    
    # Filtrar por los responsables seleccionados en el checkbox
    if (!is.null(input$checkbox_pluma) && length(input$checkbox_pluma) > 0) {
      df_final <- df_final %>%
        filter(Incidencia %in% input$checkbox_pluma)
      
      # Mostrar cuántas filas quedaron después del filtro
      print(paste("Filas después del filtro:", nrow(df_final)))
    } else {
      # Si no hay nada seleccionado, devolver un dataframe vacío o con estructura similar
      print("Ningún valor seleccionado, devolviendo dataframe vacío")
      df_final <- df_final[0, ]
    }
    
    return(df_final)
  })
  
  
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_pluma <- renderDT({
    datos <- filtrado_reactivo_pluma()
  })

  
  output$exportar_excel_pluma <- downloadHandler(
    filename = function() {
      paste("Incidencias_pluma_hasta", "_", ultima_fecha_registro, ".xlsx", sep = "")
    },
    content = function(file) {
      # Obtener los datos filtrados
      datos <- filtrado_reactivo_pluma()
      
      # Crear un workbook nuevo
      wb <- createWorkbook()
      
      # Añadir una hoja
      addWorksheet(wb, "Datos")
      
      # Escribir el data frame como tabla con formato
      writeDataTable(wb, sheet = "Datos", x = datos, tableStyle = "TableStyleLight9")
      
      # Ajustar automáticamente el ancho de todas las columnas
      setColWidths(wb, sheet = "Datos", cols = 1:ncol(datos), widths = "auto")
      
      # Guardar directamente en el archivo de descarga
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$map_pluma <- renderLeaflet({
    # Filtrar y preparar los datos como antes
    datos <- filtrado_reactivo_pluma() %>% 
      filter(!is.na(the_geom))
    datos <- modificar_coordenadas_paramapa(datos)
    
    # Crear el mapa base centrado en Montevideo
    mapa <- leaflet() %>%
      addTiles() %>%
      setView(lng = lng_montevideo, lat = lat_montevideo, zoom = 12) %>%
      addMeasure(
        position = "topright",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        primaryAreaUnit = "sqmeters",
        secondaryAreaUnit = "hectares"
      )
    
    # Solo añadir marcadores si hay datos y tienen coordenadas
    if (nrow(datos) > 0) {
      # Filtrar para eliminar filas sin coordenadas
      datos_con_coords <- datos %>% filter(!is.na(the_geom))
      
      if (nrow(datos_con_coords) > 0) {
        # Modificar coordenadas para el mapa
        datos_mapa <- modificar_coordenadas_paramapa(datos_con_coords)
        
        # Crear la paleta de colores
        paleta_colores <- colorFactor(palette = "Set1", domain = datos_mapa$Incidencia)
        
        # Añadir los marcadores al mapa
        mapa <- mapa %>%
          addCircleMarkers(
            data = datos_mapa, 
            ~lon, ~lat, 
            color = ~paleta_colores(Incidencia),
            radius = 5, 
            fillOpacity = 0.8,
            popup = ~paste("Incidencia:", Incidencia)
          )
      }
    }
    
    # Devolver el mapa (con o sin marcadores)
    mapa
  })
  
  
 
  
  
}

# nolint end

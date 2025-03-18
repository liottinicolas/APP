estadoDiarioUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(
        
        width = 3,
    
    dateInput(
      ns("filtro_fecha"),
      "Selecciona una fecha:",
      value = ultima_fecha_registro+1,
      min = as.Date("2025-02-16"),
      max = ultima_fecha_registro+1,
      width = '200px'
    ),
    ## Defino así el output para modificarlo desde el servidor.
    ## Lo calcula según el máximo.
    uiOutput(ns("diasUI"))
    ,

    #style = "background-color: #f9f9f9; height: 100%; padding: 15px;" # Color de fondo y espacio interno
    
    ),
    
    column(
      width = 3,
      checkboxGroupInput( 
        ns("checkbox_activoinactivo"), 
        "Con/sin mantenimiento", 
        c( 
          "Activos" = "Activos", 
          "Inactivos" = "Mantenimiento"
        ),
        selected = "Activos" 
      )
    ),
    
    column(
      width = 6,
      div(style = "height: 100%;") # Espacio vacío
    )
    
    ),
    

    # leafletOutput(ns("map")),
    # DTOutput(ns("tabla_puntos"))
    
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 95%; margin: 0 auto;",
          leafletOutput(ns("map")),
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(
          style = "width: 95%; margin: 0 auto;",
          DTOutput(ns("tabla_puntos"))
        )
      )
    )
    
    
    
  )
}

estadoDiarioServer <- function(input, output, session) {
  ns <- session$ns
  
  #ultima_fecha_registro <- max(historico_estado_diario$Fecha)
  #fecha_para_informediario <- ultima_fecha_registro+1
  
  # ---- Reactiva Informe diario -----
  
  # estado_diario_completo_sin_mantenimiento

  estado_diario <- reactive({
    req(input$filtro_fecha)
    req(input$dias, length(input$dias) == 2, !any(is.na(input$dias)))
    
    tryCatch({
      datos_filtrados <- historico_estado_diario %>%
        filter(Fecha == as.Date(input$filtro_fecha)-1) %>%
        filter(Acumulacion >= input$dias[1]) %>%
        filter(Acumulacion <= input$dias[2])
      
      if (is.null(input$checkbox_activoinactivo) || length(input$checkbox_activoinactivo) == 0) {
        showNotification("No se han seleccionado opciones de mantenimiento. No se muestran datos.", type = "warning")
        return(historico_estado_diario[0, c("gid", "Municipio", "Circuito_corto", "Posicion", "Direccion", "Estado", "Acumulacion", "the_geom")])
      }
      
      if (length(input$checkbox_activoinactivo) == 1) {
        if (input$checkbox_activoinactivo == "Activos") {
          datos_filtrados <- datos_filtrados %>% filter(is.na(Estado))
        } else if (input$checkbox_activoinactivo == "Mantenimiento") {
          datos_filtrados <- datos_filtrados %>% filter(Estado == "Mantenimiento")
        }
      }
      
      if (nrow(datos_filtrados) == 0) {
        showNotification("No se encontraron datos para la fecha seleccionada.", type = "warning")
        return(historico_estado_diario[0, c("gid", "Municipio", "Circuito_corto", "Posicion", "Direccion", "Estado", "Acumulacion", "the_geom")])
      }
      
      return(datos_filtrados)
    }, error = function(e) {
      showNotification(paste("Error al filtrar los datos:", e$message), type = "error")
      return(historico_estado_diario[0, c("gid", "Municipio", "Circuito_corto", "Posicion", "Direccion", "Estado", "Acumulacion", "the_geom")])
    })
  })
  
  # Función reactiva para calcular el valor máximo de la columna "Acumulacion"
  max_acumulacion <- reactive({
    req(input$filtro_fecha)  # Validar que el input tenga un valor válido

    tryCatch({
      # Filtrar el dataframe por la fecha seleccionada
      datos_filtrados <- historico_estado_diario %>%
        filter(Fecha == as.Date(input$filtro_fecha)-1)

      # Si no hay datos en esa fecha, devolver 1 como máximo
      if (nrow(datos_filtrados) == 0) {
        showNotification("No hay datos disponibles para la fecha seleccionada.", type = "warning")
        return(1)
      } else {
        # Calcular el máximo de la columna "Acumulacion" en los datos filtrados
        max_acumulacion_dia <- max(datos_filtrados$Acumulacion, na.rm = TRUE)
        return(max_acumulacion_dia)
      }
    }, error = function(e) {
      # Mostrar una notificación de error y retornar un valor por defecto
      showNotification(paste("Error al calcular el máximo de acumulación:", e$message), type = "error")
      return(1)
    })
  })
  
  output$diasUI <- renderUI({
    req(max_acumulacion())
    sliderInput(
      ns('dias'),
      'Días de acumulación',
      min = 1,
      max = max_acumulacion(),
      value = c(1, max_acumulacion())
    )
  })

  
  # Filtrar y mostrar la tabla de puntos únicos según el filtro de fecha
  output$tabla_puntos <- renderDT({

    df <- estado_diario()
    
    df <- df %>% 
      select(gid,Municipio,Circuito_corto,Posicion,Direccion,Estado,Acumulacion)
    
    datatable(df, filter = "top", selection = "single",
              rownames = FALSE,  # Desactiva la columna autogenerada de índices
              options = list(
                pageLength = 100
                ,
                columnDefs = list(
                  list(width = '10%', targets = 0,className = 'dt-center'),  # Ahora 'Municipio' será target = 0
                  list(width = '10%', targets = 1,className = 'dt-center',filter = 'text'),  # 'gid' será target = 1
                  list(width = '20%', targets = 2,className = 'dt-center'),  # 'Circuito' será target = 2
                  list(width = '10%', targets = 3,className = 'dt-center'),  # 'Posicion' será target = 3
                  list(width = '40%', targets = 4),  # 'Direccion' será target = 4
                  list(width = '5%', targets = 5,className = 'dt-center'),   # 'Acumulacion' será target = 5
                  list(width = '5%', targets = 6,className = 'dt-center')   # 'Acumulacion' será target = 5
                )
              ))
  })  
  
  

  ################### MAPA #############################

  output$map <- renderLeaflet({
    # Filtrar y preparar los datos como antes
    datos <- estado_diario() %>% 
      filter(!is.na(the_geom))
    
    # Crear la paleta de colores si hay datos
    # paleta_colores <- colorFactor(palette = "Set1", domain = datos$Incidencia)
    
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
      
      # Agrego los valores de los colores.
      datos <- modificar_coordenadas_paramapa(datos)
      datos <- datos %>%
        mutate(
          color = case_when(
            Acumulacion %in% c(1, 2) ~ "green",
            Acumulacion == 3 ~ "yellow",
            Acumulacion == 4 ~ "orange",
            Acumulacion == 5 ~ "red",
            Acumulacion >= 6 ~ "black"
          )
        )
      
      # Crear el mapa con marcadores
      mapa <- leaflet(datos) %>%
        addTiles() %>%
        addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
        addCircleMarkers(
          ~lon, ~lat,
          color = ~color, radius = 5, fillOpacity = 0.8,
          popup = ~paste(
            "Direccion: ", Direccion, "<br>",
            "GID: ", gid, "<br>",
            "Acumulacion:", Acumulacion
          )
        ) %>%
        onRender("
      function(el, x) {
        var map = this;
        function updateCircleMarkerSize() {
          var zoom = map.getZoom();
          map.eachLayer(function(layer) {
            if (layer instanceof L.CircleMarker) {
              var newRadius = zoom * 0.1;
              layer.setRadius(newRadius);
            }
          });
        }
        map.on('zoomend', updateCircleMarkerSize);
        updateCircleMarkerSize();
      }
    ")
    } 
    
    mapa  # Devolver el mapa, con o sin puntos
  })  
  
  
  
  


  # Renderizar el mapa inf diario
  output$map <- renderLeaflet({

    # Obtén los datos filtrados
    datos <- estado_diario() %>%
      filter(!is.na(the_geom))

    # Si el data frame está vacío, retorna solo el mapa base sin marcadores
    if (nrow(datos) == 0) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = -56.1645, lat = -34.9011, zoom = 12) %>%
          addSearchOSM(options = searchOptions(collapsed = FALSE))
      )
    } else {

    # Si hay datos, modifícalos para obtener coordenadas y asigna colores
    datos <- modificar_coordenadas_paramapa(datos)
    datos <- datos %>%
      mutate(
        color = case_when(
          Acumulacion %in% c(1, 2) ~ "green",
          Acumulacion == 3 ~ "yellow",
          Acumulacion == 4 ~ "orange",
          Acumulacion == 5 ~ "red",
          Acumulacion >= 6 ~ "black"
        )
      )

    # Crear el mapa con marcadores
    leaflet(datos) %>%
      addTiles() %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color, radius = 5, fillOpacity = 0.8,
        popup = ~paste(
          "Direccion: ", Direccion, "<br>",
          "GID: ", gid, "<br>",
          "Acumulacion:", Acumulacion
        )
      ) %>%
      onRender("
      function(el, x) {
        var map = this;
        function updateCircleMarkerSize() {
          var zoom = map.getZoom();
          map.eachLayer(function(layer) {
            if (layer instanceof L.CircleMarker) {
              var newRadius = zoom * 0.1;
              layer.setRadius(newRadius);
            }
          });
        }
        map.on('zoomend', updateCircleMarkerSize);
        updateCircleMarkerSize();
      }
    ")

    }
  })
 }

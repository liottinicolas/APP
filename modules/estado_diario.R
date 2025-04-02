# nolint start: line_length_linter, object_name_linter

#' Módulo de Estado Diario
#' 
#' Este módulo permite visualizar y analizar el estado diario de puntos en un mapa interactivo.
#' Incluye filtros por fecha, acumulación y estado de mantenimiento.

# Constantes
COLORES_ACUMULACION <- list(
  "1-2" = "green",
  "3" = "yellow",
  "4" = "orange",
  "5" = "red",
  "6+" = "black"
)

#' Función para determinar el color según la acumulación
#' @param acumulacion Número de días de acumulación
#' @return Color correspondiente
determinar_color_acumulacion <- function(acumulacion) {
  if (acumulacion %in% c(1, 2)) return(COLORES_ACUMULACION[["1-2"]])
  if (acumulacion == 3) return(COLORES_ACUMULACION[["3"]])
  if (acumulacion == 4) return(COLORES_ACUMULACION[["4"]])
  if (acumulacion == 5) return(COLORES_ACUMULACION[["5"]])
  return(COLORES_ACUMULACION[["6+"]])
}

#' Función para filtrar datos según los criterios seleccionados
#' @param datos DataFrame con los datos históricos
#' @param fecha Fecha seleccionada
#' @param dias Vector con rango de días
#' @param estados Vector con estados seleccionados
#' @return DataFrame filtrado
filtrar_datos_estado_diario <- function(datos, fecha, dias, estados) {
  if (is.null(estados) || length(estados) == 0) {
    return(datos[0, ])
  }
  
  datos_filtrados <- datos %>%
    filter(Fecha == as.Date(fecha) - 1) %>%
    filter(Acumulacion >= dias[1], Acumulacion <= dias[2])
  
  if (length(estados) == 1) {
    if (estados == "Activos") {
      datos_filtrados <- datos_filtrados %>% filter(is.na(Estado))
    } else if (estados == "Mantenimiento") {
      datos_filtrados <- datos_filtrados %>% filter(Estado == "Mantenimiento")
    }
  }
  
  return(datos_filtrados)
}

#' Función para calcular estadísticas diarias
#' @param datos DataFrame con los datos históricos
#' @param fecha Fecha seleccionada
#' @return Lista con estadísticas
calcular_estadisticas_diarias <- function(datos, fecha) {
  # Manejar posibles errores
  tryCatch({
    # Convertir explícitamente la fecha
    fecha_filtro <- as.Date(fecha) - 1
    
    # Verificar que hay datos para la fecha
    datos_dia <- datos %>%
      filter(Fecha == fecha_filtro)
    
    # Si no hay datos, retornar valores en cero
    if (nrow(datos_dia) == 0) {
      return(list(
        levantados = 0,
        mantenimiento = 0,
        activos = 0
      ))
    }
    
    # Calcular estadísticas
    contenedores_levantados <- tryCatch({
      datos_dia %>%
        filter(Acumulacion == 1) %>%
        filter(is.na(Estado)) %>%
        nrow()
    }, error = function(e) {
      warning(paste("Error al calcular contenedores levantados:", e$message))
      return(0)
    })
    
    contenedores_mantenimiento <- tryCatch({
      datos_dia %>%
        filter(Estado == "Mantenimiento") %>%
        nrow()
    }, error = function(e) {
      warning(paste("Error al calcular contenedores en mantenimiento:", e$message))
      return(0)
    })
    
    contenedores_activos <- tryCatch({
      datos_dia %>%
        filter(is.na(Estado)) %>%
        nrow()
    }, error = function(e) {
      warning(paste("Error al calcular contenedores activos:", e$message))
      return(0)
    })
    
    return(list(
      levantados = contenedores_levantados,
      mantenimiento = contenedores_mantenimiento,
      activos = contenedores_activos
    ))
  }, error = function(e) {
    warning(paste("Error general en cálculo de estadísticas:", e$message))
    return(list(
      levantados = 0,
      mantenimiento = 0,
      activos = 0
    ))
  })
}

estadoDiarioUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Dashboard de estadísticas
    fluidRow(
      column(
        width = 4,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Contenedores Levantados", style = "color: #28a745;"),
          textOutput(ns("contador_levantados")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      ),
      column(
        width = 4,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("En Mantenimiento", style = "color: #dc3545;"),
          textOutput(ns("contador_mantenimiento")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      ),
      column(
        width = 4,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Activos", style = "color: #17a2b8;"),
          textOutput(ns("contador_activos")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        dateInput(
          ns("filtro_fecha"),
          "Selecciona una fecha:",
          value = ultima_fecha_registro + 1,
          min = as.Date("2025-02-16"),
          max = ultima_fecha_registro + 1,
          width = '200px'
        ),
        uiOutput(ns("diasUI")),
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
        div(style = "height: 100%;")
      )
    ),
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
  
  # Reactiva para las estadísticas diarias
  estadisticas_diarias <- reactive({
    req(input$filtro_fecha)
    calcular_estadisticas_diarias(historico_estado_diario, input$filtro_fecha)
  })
  
  # Outputs para los contadores
  output$contador_levantados <- renderText({
    estadisticas_diarias()$levantados
  })
  
  output$contador_mantenimiento <- renderText({
    estadisticas_diarias()$mantenimiento
  })
  
  output$contador_activos <- renderText({
    estadisticas_diarias()$activos
  })
  
  # Reactiva para el estado diario
  estado_diario <- reactive({
    req(input$filtro_fecha)
    req(input$dias, length(input$dias) == 2, !any(is.na(input$dias)))
    
    tryCatch({
      datos_filtrados <- filtrar_datos_estado_diario(
        historico_estado_diario,
        input$filtro_fecha,
        input$dias,
        input$checkbox_activoinactivo
      )
      
      if (nrow(datos_filtrados) == 0) {
        showNotification("No se encontraron datos para la fecha seleccionada.", type = "warning")
        return(historico_estado_diario[0, ])
      }
      
      return(datos_filtrados)
    }, error = function(e) {
      showNotification(paste("Error al filtrar los datos:", e$message), type = "error")
      return(historico_estado_diario[0, ])
    })
  })
  
  # Reactiva para el máximo de acumulación
  max_acumulacion <- reactive({
    req(input$filtro_fecha)
    
    tryCatch({
      datos_filtrados <- historico_estado_diario %>%
        filter(Fecha == as.Date(input$filtro_fecha) - 1)
      
      if (nrow(datos_filtrados) == 0) {
        showNotification("No hay datos disponibles para la fecha seleccionada.", type = "warning")
        return(1)
      }
      
      return(max(datos_filtrados$Acumulacion, na.rm = TRUE))
    }, error = function(e) {
      showNotification(paste("Error al calcular el máximo de acumulación:", e$message), type = "error")
      return(1)
    })
  })
  
  # UI para el slider de días
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
  
  # Tabla de puntos
  output$tabla_puntos <- renderDT({
    df <- estado_diario() %>%
      select(gid, Municipio, Circuito_corto, Posicion, Direccion, Estado, Acumulacion)
    
    datatable(df,
              filter = "top",
              selection = "single",
              rownames = FALSE,
              options = list(
                pageLength = 100,
                columnDefs = list(
                  list(width = '10%', targets = 0, className = 'dt-center'),
                  list(width = '10%', targets = 1, className = 'dt-center', filter = 'text'),
                  list(width = '20%', targets = 2, className = 'dt-center'),
                  list(width = '10%', targets = 3, className = 'dt-center'),
                  list(width = '40%', targets = 4),
                  list(width = '5%', targets = 5, className = 'dt-center'),
                  list(width = '5%', targets = 6, className = 'dt-center')
                )
              ))
  })
  
  # Mapa
  output$map <- renderLeaflet({
    datos <- estado_diario() %>%
      filter(!is.na(the_geom))
    
    if (nrow(datos) == 0) {
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = lng_montevideo, lat = lat_montevideo, zoom = 12) %>%
          addSearchOSM(options = searchOptions(collapsed = FALSE))
      )
    }
    
    datos <- modificar_coordenadas_paramapa(datos) %>%
      mutate(color = map_chr(Acumulacion, determinar_color_acumulacion))
    
    leaflet(datos) %>%
      addTiles() %>%
      addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color,
        radius = 5,
        fillOpacity = 0.8,
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
  })
}

# nolint end
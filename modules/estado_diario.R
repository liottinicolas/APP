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
      
      # Obtener los días en mantenimiento
      dias_mantenimiento <- funcion_cargar_dias_que_esta_en_mantenimiento(datos_filtrados)
      
      # Unir con el dataframe original solo la columna Diferencia
      datos_filtrados <- datos_filtrados %>%
        left_join(
          dias_mantenimiento %>% select(gid, Diferencia),
          by = "gid"
        )
    } else if (estados == "Sin instalar") {
      datos_filtrados <- datos_filtrados %>% filter(Estado == "Sin instalar")
    }
  }
  
  # Eliminamos el distinct para mantener todos los registros, incluyendo duplicados
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
        activos = 0,
        sin_instalar = 0
      ))
    }

    # Calcular estadísticas
    contenedores_sin_atraso <- tryCatch({
      datos_dia %>%
        filter(Acumulacion == 1) %>%
        filter(is.na(Estado)) %>%
        left_join(
          web_historico_completo_llenado_incidencias %>% 
            select(gid, Fecha, Porcentaje_llenado),
          by = c("gid", "Fecha")
        ) %>%
        nrow()
    }, error = function(e) {
      warning(paste("Error al calcular contenedores sin atraso:", e$message))
      return(0)
    })
    
    # Calcular estadísticas
    contenedores_levantados <- tryCatch({
      datos_dia %>%
        filter(Acumulacion == 1) %>%
        filter(is.na(Estado)) %>%
        left_join(
          web_historico_completo_llenado_incidencias %>% 
            select(gid, Fecha, Porcentaje_llenado),
          by = c("gid", "Fecha")
        ) %>%
        filter(!is.na(Porcentaje_llenado)) %>%
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
    
    contenedores_sin_instalar <- tryCatch({
      datos_dia %>%
        filter(Estado == "Sin instalar") %>%
        nrow()
    }, error = function(e) {
      warning(paste("Error al calcular contenedores sin instalar:", e$message))
      return(0)
    })
    
    return(list(
      levantados = contenedores_levantados,
      mantenimiento = contenedores_mantenimiento,
      activos = contenedores_activos,
      sin_instalar = contenedores_sin_instalar,
      sin_atraso = contenedores_sin_atraso
    ))
  }, error = function(e) {
    warning(paste("Error general en cálculo de estadísticas:", e$message))
    return(list(
      levantados = 0,
      mantenimiento = 0,
      activos = 0,
      sin_instalar = 0,
      sin_atraso = 0
    ))
  })
}

estadoDiarioUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Título principal
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("titulo_principal"))
      )
    ),
    # Dashboard de estadísticas
    fluidRow(
      column(
        width = 3,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Contenedores Levantados", style = "color: #28a745;"),
          textOutput(ns("contador_levantados")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      ),
      column(
        width = 3,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("En Mantenimiento", style = "color: #dc3545;"),
          textOutput(ns("contador_mantenimiento")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      ),
      column(
        width = 3,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Activos", style = "color: #17a2b8;"),
          textOutput(ns("contador_activos")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      ),
      column(
        width = 3,
        div(
          class = "well",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4("Sin Instalar", style = "color: #6c757d;"),
          textOutput(ns("contador_sin_instalar")),
          style = "text-align: center; font-size: 24px; font-weight: bold;"
        )
      )
    ),
    div(
      class = "well",
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin-bottom: 20px;",
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
          )
        ),
        column(
          width = 3,
          uiOutput(ns("dias_min_ui"))
        ),
        column(
          width = 3,
          uiOutput(ns("dias_max_ui"))
        ),
        column(
          width = 3,
          radioButtons(
            ns("checkbox_activoinactivo"),
            "Estado",
            c(
              "Activos" = "Activos",
              "En Mantenimiento" = "Mantenimiento",
              "Sin instalar" = "Sin instalar"
            ),
            selected = "Activos"
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput(
            ns("filtro_municipio"),
            "Municipio:",
            choices = NULL,
            multiple = TRUE,
            width = '100%'
          )
        ),
        column(
          width = 6,
          selectInput(
            ns("filtro_circuito"),
            "Circuito:",
            choices = NULL,
            multiple = TRUE,
            width = '100%'
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          style = "text-align: left;",
          downloadButton(
            ns("descargar_csv"),
            "Descargar CSV",
            class = "btn btn-primary",
            style = "margin-top: 10px; width: 200px;"
          ),
          downloadButton(
            ns("descargar_todo_csv"),
            "Descargar Todo",
            class = "btn btn-success",
            style = "margin-top: 10px; margin-left: 10px; width: 200px;"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          # Oculto el mapa
          #style = "width: 95%; margin: 0 auto;",
          #leafletOutput(ns("map")),
          style = "width: 95%; margin: 0 auto; text-align: center; padding: 40px; color: #555;",
          uiOutput(ns("map_placeholder"))
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
  
  # Título reactivo
  output$titulo_principal <- renderUI({
    h3(paste("Informe diario de Gestión", 
             format(input$filtro_fecha, "%d/%m/%Y"),
             "a las 06:00 am"),
       style = "text-align: center; color: #2c3e50; margin-bottom: 30px; font-weight: bold;")
  })
  
  # Reactiva para las estadísticas diarias
  estadisticas_diarias <- reactive({
    req(input$filtro_fecha)
    calcular_estadisticas_diarias(web_historico_estado_diario, input$filtro_fecha)
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
  
  output$contador_sin_instalar <- renderText({
    estadisticas_diarias()$sin_instalar
  })
  
  # Reactiva para obtener los municipios únicos
  municipios_unicos <- reactive({
    req(web_historico_estado_diario)
    sort(unique(web_historico_estado_diario$Municipio))
  })
  
  # Actualizar las opciones del select de municipios
  observe({
    updateSelectInput(
      session,
      "filtro_municipio",
      choices = municipios_unicos(),
      selected = NULL
    )
  })
  
  # Reactiva para obtener los circuitos según el municipio seleccionado
  circuitos_filtrados <- reactive({
    req(input$filtro_municipio)
    req(web_historico_estado_diario)
    
    if (length(input$filtro_municipio) > 0) {
      circuitos <- web_historico_estado_diario %>%
        filter(Municipio %in% input$filtro_municipio) %>%
        pull(Circuito_corto) %>%
        unique() %>%
        sort()
    } else {
      circuitos <- sort(unique(web_historico_estado_diario$Circuito_corto))
    }
    return(circuitos)
  })
  
  # Actualizar las opciones del select de circuitos
  observe({
    updateSelectInput(
      session,
      "filtro_circuito",
      choices = circuitos_filtrados(),
      selected = NULL
    )
  })
  
  # Reactiva para el estado diario con los nuevos filtros
  estado_diario <- reactive({
    req(input$filtro_fecha)
    req(input$dias_min, input$dias_max)
    
    # Validar que el mínimo no sea mayor que el máximo
    if (input$dias_min > input$dias_max) {
      showNotification("El valor mínimo no puede ser mayor que el máximo", type = "error")
      return(web_historico_estado_diario[0, ])
    }
    
    tryCatch({
      datos_filtrados <- filtrar_datos_estado_diario(
        web_historico_estado_diario,
        input$filtro_fecha,
        c(input$dias_min, input$dias_max),
        input$checkbox_activoinactivo
      )
      
      # Aplicar filtros de municipio y circuito
      if (length(input$filtro_municipio) > 0) {
        datos_filtrados <- datos_filtrados %>%
          filter(Municipio %in% input$filtro_municipio)
      }
      
      if (length(input$filtro_circuito) > 0) {
        datos_filtrados <- datos_filtrados %>%
          filter(Circuito_corto %in% input$filtro_circuito)
      }
      
      if (nrow(datos_filtrados) == 0) {
        showNotification("No se encontraron datos para los filtros seleccionados.", type = "warning")
        return(web_historico_estado_diario[0, ])
      }
      
      # Ordenar los datos por Circuito_corto y Posicion
      datos_filtrados <- datos_filtrados %>%
        arrange(Circuito_corto, as.numeric(Posicion))
      
      return(datos_filtrados)
    }, error = function(e) {
      showNotification(paste("Error al filtrar los datos:", e$message), type = "error")
      return(web_historico_estado_diario[0, ])
    })
  })
  
  # Reactiva para el máximo de acumulación
  max_acumulacion <- reactive({
    req(input$filtro_fecha)
    
    tryCatch({
      datos_filtrados <- web_historico_estado_diario %>%
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
  
  # UI para los inputs de días
  output$dias_min_ui <- renderUI({
    req(max_acumulacion())
    numericInput(
      ns('dias_min'),
      'Días mínimos',
      min = 1,
      max = max_acumulacion(),
      value = 1,
      width = '100px'
    )
  })
  
  output$dias_max_ui <- renderUI({
    req(max_acumulacion())
    numericInput(
      ns('dias_max'),
      'Días máximos',
      min = 1,
      max = max_acumulacion(),
      value = max_acumulacion(),
      width = '100px'
    )
  })
  
  # Tabla de puntos
  output$tabla_puntos <- renderDT({
    df <- estado_diario()
    
    # Seleccionar columnas según el estado seleccionado
    if (input$checkbox_activoinactivo == "Mantenimiento") {
      df <- df %>%
        select(gid, Municipio, Circuito_corto, Posicion, Direccion, Estado, Acumulacion, Diferencia)
      
      # Configuración de columnas para estado Mantenimiento
      columnDefs <- list(
        list(width = '10%', targets = 0, className = 'dt-center'),
        list(width = '10%', targets = 1, className = 'dt-center', filter = 'text'),
        list(width = '20%', targets = 2, className = 'dt-center'),
        list(width = '10%', targets = 3, className = 'dt-center'),
        list(width = '35%', targets = 4),
        list(width = '5%', targets = 5, className = 'dt-center'),
        list(width = '5%', targets = 6, className = 'dt-center'),
        list(width = '5%', targets = 7, className = 'dt-center')
      )
    } else {
      df <- df %>%
        select(gid, Municipio, Circuito_corto, Posicion, Direccion, Estado, Acumulacion)
      
      # Configuración de columnas para estado Activos y Sin instalar
      columnDefs <- list(
        list(width = '10%', targets = 0, className = 'dt-center'),
        list(width = '10%', targets = 1, className = 'dt-center', filter = 'text'),
        list(width = '20%', targets = 2, className = 'dt-center'),
        list(width = '10%', targets = 3, className = 'dt-center'),
        list(width = '40%', targets = 4),
        list(width = '5%', targets = 5, className = 'dt-center'),
        list(width = '5%', targets = 6, className = 'dt-center')
      )
    }
    
    datatable(df,
              filter = "top",
              selection = "single",
              rownames = FALSE,
              options = list(
                pageLength = 100,
                columnDefs = columnDefs
              ))
  })
  
  # Descargar datos como CSV
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste("estado_diario_", format(input$filtro_fecha, "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      df <- estado_diario()
      
      # Seleccionar columnas según el estado seleccionado
      if (input$checkbox_activoinactivo == "Mantenimiento") {
        df <- df %>%
          select(gid, Municipio, Circuito_corto, Posicion, Direccion, Estado, Acumulacion, Diferencia)
      } else {
        df <- df %>%
          select(gid, Municipio, Circuito_corto, Posicion, Direccion, Estado, Acumulacion)
      }
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Descargar todos los datos como CSV
  output$descargar_todo_csv <- downloadHandler(
    filename = function() {
      paste("estado_diario_completo_", format(input$filtro_fecha, "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      # Obtener datos para todos los estados
      datos_activos <- filtrar_datos_estado_diario(
        web_historico_estado_diario,
        input$filtro_fecha,
        c(1, max_acumulacion()),
        "Activos"
      )
      
      datos_mantenimiento <- filtrar_datos_estado_diario(
        web_historico_estado_diario,
        input$filtro_fecha,
        c(1, max_acumulacion()),
        "Mantenimiento"
      )
      
      datos_sin_instalar <- filtrar_datos_estado_diario(
        web_historico_estado_diario,
        input$filtro_fecha,
        c(1, max_acumulacion()),
        "Sin instalar"
      )
      
      # Combinar todos los datos
      df_completo <- bind_rows(
        datos_activos,
        datos_mantenimiento,
        datos_sin_instalar,
      )
      
      # Seleccionar las columnas necesarias
      df_completo <- df_completo %>%
        select(gid, Circuito, Municipio, Circuito_corto, Posicion, Direccion,Observaciones, Estado, Acumulacion) %>%
        arrange(Circuito, Posicion)
      
      write.csv(df_completo, file, row.names = FALSE, na = "")
    }
  )
  
  # # Mapa
  # output$map <- renderLeaflet({
  #   datos <- estado_diario()
  #   
  #   if (nrow(datos) == 0) {
  #     return(
  #       leaflet() %>%
  #         addTiles() %>%
  #         setView(lng = lng_montevideo, lat = lat_montevideo, zoom = 12) %>%
  #         addSearchOSM(options = searchOptions(collapsed = FALSE))
  #     )
  #   }
  #   
  #   # Separar datos con y sin geometría
  #   datos_con_geom <- datos %>% filter(!is.na(the_geom))
  #   datos_sin_geom <- datos %>% filter(is.na(the_geom))
  #   
  #   # Procesar datos con geometría para el mapa
  #   datos_mapa <- modificar_coordenadas_paramapa(datos_con_geom) %>%
  #     mutate(color = map_chr(Acumulacion, determinar_color_acumulacion))
  #   
  #   # Crear mapa base
  #   mapa <- leaflet() %>%
  #     addTiles() %>%
  #     addSearchOSM(options = searchOptions(collapsed = FALSE))
  #   
  #   # Agregar marcadores solo para datos con geometría
  #   if (nrow(datos_mapa) > 0) {
  #     mapa <- mapa %>%
  #       addCircleMarkers(
  #         data = datos_mapa,
  #         ~lon, ~lat,
  #         color = ~color,
  #         radius = 5,
  #         fillOpacity = 0.8,
  #         popup = ~paste(
  #           "Direccion: ", Direccion, "<br>",
  #           "GID: ", gid, "<br>",
  #           "Acumulacion:", Acumulacion, "<br>",
  #           "Circuito:", Circuito_corto, "<br>",
  #           "Posicion:", Posicion
  #         )
  #       )
  #   }
  #   
  #   # Agregar mensaje si hay datos sin geometría
  #   if (nrow(datos_sin_geom) > 0) {
  #     mapa <- mapa %>%
  #       addControl(
  #         html = paste0(
  #           "<div style='background-color: white; padding: 10px; border-radius: 5px;'>",
  #           "<strong>Nota:</strong> Hay ", nrow(datos_sin_geom), 
  #           " registros sin coordenadas que se muestran en la tabla.",
  #           "</div>"
  #         ),
  #         position = "topright"
  #       )
  #   }
  #   
  #   mapa %>%
  #     onRender("
  #       function(el, x) {
  #         var map = this;
  #         function updateCircleMarkerSize() {
  #           var zoom = map.getZoom();
  #           map.eachLayer(function(layer) {
  #             if (layer instanceof L.CircleMarker) {
  #               var newRadius = zoom * 0.1;
  #               layer.setRadius(newRadius);
  #             }
  #           });
  #         }
  #         map.on('zoomend', updateCircleMarkerSize);
  #         updateCircleMarkerSize();
  #       }
  #     ")
  # })
  
  output$map_placeholder <- renderUI({
    div(
      style = "font-size: 20px; color: #dc3545; font-weight: bold;",
      "Mapa en mantenimiento"
    )
  })
}

# nolint end
# Dashboard de Reportes de Incidencias
# Módulo para mostrar información agregada del dataframe web_historico_completo_llenado_incidencias

# UI del módulo
reportesIncidenciasUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        h2("Dashboard de Reportes de Incidencias", style = "color: #2E8B57; margin-bottom: 20px;")
      )
    ),
    
    # Panel de filtros
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
          h4("Filtros", style = "color: #495057; margin-bottom: 15px;"),
          
                     fluidRow(
             column(
               width = 4,
               dateRangeInput(
                 inputId = ns("fecha_reporte"),
                 label = "Rango de fechas:",
                 min = as.Date("2024-10-01"),
                 start = ultima_fecha_registro,
                 end = ultima_fecha_registro,
                 format = "dd/mm/yyyy",
                 language = "es"
               )
             ),
             column(
               width = 4,
               selectInput(
                 inputId = ns("agrupacion_principal"),
                 label = "Agrupar por:",
                 choices = c(
                   "Fecha" = "Fecha",
                   "Municipio" = "Municipio", 
                   "Circuito_corto" = "Circuito_corto",
                   "Turno" = "Turno",
                   "Incidencia" = "Incidencia"
                 ),
                 selected = "Incidencia"
               )
             ),
             column(
               width = 4,
               selectInput(
                 inputId = ns("agrupacion_secundaria"),
                 label = "Sub-agrupar por:",
                 choices = c("Ninguna" = "ninguna"),
                 selected = "ninguna"
               )
             )
           ),
          
                     fluidRow(
             column(
               width = 6,
               actionButton(
                 inputId = ns("btn_actualizar"),
                 label = "Actualizar Reporte",
                 icon = icon("refresh"),
                 class = "btn-primary",
                 style = "margin-top: 10px;"
               )
             ),
             column(
               width = 6,
               actionButton(
                 inputId = ns("btn_limpiar"),
                 label = "Limpiar Filtros",
                 icon = icon("eraser"),
                 class = "btn-warning",
                 style = "margin-top: 10px;"
               )
             )
           )
        )
      )
    ),
    
    # Panel de métricas resumidas
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
          h4("Métricas Generales", style = "color: #2E8B57; margin-bottom: 15px;"),
          
                     fluidRow(
             column(
               width = 4,
               div(
                 style = "text-align: center; padding: 15px; background-color: white; border-radius: 5px; margin: 5px;",
                 h3(textOutput(ns("total_registros")), style = "color: #2E8B57; margin: 0;"),
                 p("Total Registros", style = "margin: 5px 0 0 0; color: #666;")
               )
             ),
             column(
               width = 4,
               div(
                 style = "text-align: center; padding: 15px; background-color: white; border-radius: 5px; margin: 5px;",
                 h3(textOutput(ns("total_levantados")), style = "color: #28a745; margin: 0;"),
                 p("Contenedores Levantados", style = "margin: 5px 0 0 0; color: #666;")
               )
             ),
             column(
               width = 4,
               div(
                 style = "text-align: center; padding: 15px; background-color: white; border-radius: 5px; margin: 5px;",
                 h3(textOutput(ns("total_no_levantados")), style = "color: #dc3545; margin: 0;"),
                 p("No Levantados con Incidencia", style = "margin: 5px 0 0 0; color: #666;")
               )
             )
           )
        )
      )
    ),
    
    # Tabla de datos agregados
    fluidRow(
      column(
        width = 12,
        div(
          style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          h4("Resumen Agregado", style = "color: #495057; margin-bottom: 15px;"),
          
          DTOutput(ns("tabla_resumen"))
        )
      )
    ),
    
    # Gráfico de distribución de incidencias (solo cuando se agrupa por Incidencia)
    conditionalPanel(
      condition = "input.agrupacion_principal == 'Incidencia'",
      fluidRow(
        column(
          width = 12,
          div(
            style = "background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-top: 20px;",
            h4("Distribución de Incidencias", style = "color: #495057; margin-bottom: 15px;"),
            
            fluidRow(
              column(
                width = 6,
                plotlyOutput(ns("grafico_barras"), height = "400px")
              ),
              column(
                width = 6,
                plotlyOutput(ns("grafico_pie"), height = "400px")
              )
            )
          )
        )
      )
    ),
    
    # Botón de descarga
    fluidRow(
      column(
        width = 12,
        div(
          style = "text-align: center; margin-top: 20px;",
          downloadButton(
            outputId = ns("btn_descargar"),
            label = "Descargar Reporte (Excel)",
            class = "btn-success"
          )
        )
      )
    )
  )
}

# Server del módulo
reportesIncidenciasServer <- function(input, output, session) {
  ns <- session$ns
  
  # Datos reactivos filtrados
  datos_filtrados <- reactive({
    req(input$fecha_reporte)
    
    web_historico_completo_llenado_incidencias %>%
      filter(Fecha >= input$fecha_reporte[1]) %>%
      filter(Fecha <= input$fecha_reporte[2])
  })
  
  # Actualizar opciones de agrupación secundaria
  observe({
    req(input$agrupacion_principal)
    
    # Obtener todas las opciones disponibles excepto la principal
    opciones_disponibles <- c(
      "Fecha" = "Fecha",
      "Municipio" = "Municipio", 
      "Circuito_corto" = "Circuito_corto",
      "Turno" = "Turno",
      "Incidencia" = "Incidencia"
    )
    
    # Remover la opción principal de las secundarias
    opciones_secundarias <- opciones_disponibles[opciones_disponibles != input$agrupacion_principal]
    opciones_secundarias <- c("Ninguna" = "ninguna", opciones_secundarias)
    
    updateSelectInput(session, "agrupacion_secundaria", 
                     choices = opciones_secundarias,
                     selected = "ninguna")
  })
  

  
  # Botón limpiar filtros
  observeEvent(input$btn_limpiar, {
    updateSelectInput(session, "agrupacion_secundaria", selected = "ninguna")
  })
  
  # Función auxiliar para filtrar datos de manera segura
  filtrar_datos_vacios <- function(df, columna) {
    # Verificar si la columna es de tipo Date
    if (inherits(df[[columna]], "Date")) {
      return(df %>% filter(!is.na(!!sym(columna))))
    } else {
      # Para otros tipos de datos, filtrar NA, vacíos y "NA"
      return(df %>% filter(!is.na(!!sym(columna)) & 
                          !!sym(columna) != "" & 
                          !!sym(columna) != "NA"))
    }
  }
  
  # Datos agregados según la selección del usuario
  datos_agregados <- reactive({
    req(datos_filtrados())
    req(input$agrupacion_principal)
    
    # Filtrar datos vacíos o NA de las columnas de agrupación
    datos_filtrados_por_valor <- filtrar_datos_vacios(datos_filtrados(), input$agrupacion_principal)
    
    # Si hay agrupación secundaria, también filtrar esos valores
    if (!is.null(input$agrupacion_secundaria) && input$agrupacion_secundaria != "ninguna") {
      datos_filtrados_por_valor <- filtrar_datos_vacios(datos_filtrados_por_valor, input$agrupacion_secundaria)
    }
    
    # Determinar las columnas a agrupar
    columnas_agrupar <- c(input$agrupacion_principal)
    
    # Agregar agrupación secundaria si no es "ninguna"
    if (!is.null(input$agrupacion_secundaria) && input$agrupacion_secundaria != "ninguna") {
      columnas_agrupar <- c(columnas_agrupar, input$agrupacion_secundaria)
    }
    
    # Realizar la agregación
    datos_agregados_temp <- datos_filtrados_por_valor %>%
      group_by(across(all_of(columnas_agrupar))) %>%
      summarise(
        Total_Registros = n(),
        Contenedores_Levantados = sum(!is.na(Porcentaje_llenado), na.rm = TRUE),
        Contenedores_No_Levantados_Incidencias = sum(is.na(Porcentaje_llenado), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Si se agrupa por Incidencia, agregar porcentajes
    if (input$agrupacion_principal == "Incidencia") {
      total_incidencias <- sum(datos_agregados_temp$Contenedores_No_Levantados_Incidencias, na.rm = TRUE)
      datos_agregados_temp <- datos_agregados_temp %>%
        mutate(
          Porcentaje_Incidencias = round((Contenedores_No_Levantados_Incidencias / total_incidencias) * 100, 2)
        )
    }
    
    datos_agregados_temp %>%
      # Filtrar filas donde cualquier columna de agrupación esté vacía o sea NA
      filter(if_all(all_of(columnas_agrupar), ~!is.na(.))) %>%
      # Ordenamiento inteligente según el tipo de agrupación
      {
        df <- .
        # Si solo hay agrupación principal
        if (length(columnas_agrupar) == 1) {
          col_principal <- columnas_agrupar[1]
          if (col_principal == "Fecha") {
            # Para fechas: orden descendente (más reciente primero)
            df %>% arrange(desc(!!sym(col_principal)))
          } else {
            # Para otros campos: ordenar por "No Levantados con Incidencia" descendente, luego alfabético
            df %>% arrange(desc(Contenedores_No_Levantados_Incidencias), !!sym(col_principal))
          }
        } else {
          # Si hay agrupación secundaria
          col_principal <- columnas_agrupar[1]
          col_secundaria <- columnas_agrupar[2]
          
          if (col_principal == "Fecha") {
            # Si la principal es fecha, ordenar por fecha descendente, luego por secundaria
            df %>% arrange(desc(!!sym(col_principal)), desc(Contenedores_No_Levantados_Incidencias))
          } else {
            # Si la principal no es fecha, ordenar por "No Levantados", luego alfabético por principal, luego por secundaria
            df %>% arrange(desc(Contenedores_No_Levantados_Incidencias), !!sym(col_principal), !!sym(col_secundaria))
          }
        }
      }
  })
  
  # Métricas generales
  output$total_registros <- renderText({
    req(datos_filtrados())
    format(nrow(datos_filtrados()), big.mark = ",")
  })
  
  output$total_levantados <- renderText({
    req(datos_filtrados())
    levantados <- sum(!is.na(datos_filtrados()$Porcentaje_llenado), na.rm = TRUE)
    format(levantados, big.mark = ",")
  })
  
  output$total_no_levantados <- renderText({
    req(datos_filtrados())
    no_levantados <- sum(is.na(datos_filtrados()$Porcentaje_llenado), na.rm = TRUE)
    format(no_levantados, big.mark = ",")
  })
  
  # Gráfico de barras para incidencias
  output$grafico_barras <- renderPlotly({
    req(datos_agregados())
    req(input$agrupacion_principal == "Incidencia")
    
    datos_grafico <- datos_agregados() %>%
      filter(Contenedores_No_Levantados_Incidencias > 0) %>%
      arrange(desc(Contenedores_No_Levantados_Incidencias)) %>%
      head(10)  # Top 10 incidencias
    
    if (nrow(datos_grafico) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No hay datos para mostrar", 
                             showarrow = FALSE,
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5))
    }
    
    plot_ly(datos_grafico, x = ~Incidencia, y = ~Contenedores_No_Levantados_Incidencias,
            type = 'bar', 
            text = ~paste0(Contenedores_No_Levantados_Incidencias, " (", Porcentaje_Incidencias, "%)"),
            textposition = 'auto',
            marker = list(color = '#dc3545')) %>%
      layout(title = "Top 10 Incidencias por Cantidad",
             xaxis = list(title = "Tipo de Incidencia", 
                         tickangle = 45,
                         showticklabels = TRUE),
             yaxis = list(title = "Cantidad de Contenedores"),
             showlegend = FALSE,
             margin = list(b = 100))  # Margen inferior para etiquetas rotadas
  })
  
  # Gráfico de pie para incidencias
  output$grafico_pie <- renderPlotly({
    req(datos_agregados())
    req(input$agrupacion_principal == "Incidencia")
    
    datos_grafico <- datos_agregados() %>%
      filter(Contenedores_No_Levantados_Incidencias > 0) %>%
      arrange(desc(Contenedores_No_Levantados_Incidencias)) %>%
      head(8)  # Top 8 para el pie chart
    
    if (nrow(datos_grafico) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "No hay datos para mostrar", 
                             showarrow = FALSE,
                             xref = "paper", yref = "paper",
                             x = 0.5, y = 0.5))
    }
    
    # Si hay más de 8 incidencias, agrupar las menores en "Otros"
    if (nrow(datos_agregados()) > 8) {
      otros_datos <- datos_agregados() %>%
        filter(Contenedores_No_Levantados_Incidencias > 0) %>%
        arrange(desc(Contenedores_No_Levantados_Incidencias)) %>%
        slice(9:n()) %>%
        summarise(
          Incidencia = "Otros",
          Contenedores_No_Levantados_Incidencias = sum(Contenedores_No_Levantados_Incidencias, na.rm = TRUE),
          Porcentaje_Incidencias = sum(Porcentaje_Incidencias, na.rm = TRUE)
        )
      
      datos_grafico <- bind_rows(datos_grafico, otros_datos)
    }
    
    plot_ly(datos_grafico, labels = ~Incidencia, values = ~Contenedores_No_Levantados_Incidencias,
            type = 'pie',
            textinfo = 'label+percent',
            textposition = 'outside',
            hoverinfo = 'label+value+percent',
            marker = list(colors = c('#dc3545', '#fd7e14', '#ffc107', '#28a745', 
                                   '#17a2b8', '#6f42c1', '#e83e8c', '#6c757d', '#343a40'))) %>%
      layout(title = "Distribución de Incidencias",
             showlegend = TRUE,
             legend = list(orientation = "v", x = 1.05, y = 0.5))
  })
  
  # Tabla de resumen
  output$tabla_resumen <- renderDT({
    req(datos_agregados())
    
    # Preparar los datos para la tabla
    tabla_datos <- datos_agregados()
    
    # Configurar nombres de columnas dinámicamente
    nombres_columnas <- c()
    
    # Agregar nombres para las columnas de agrupación
    if (input$agrupacion_secundaria == "ninguna") {
      if (input$agrupacion_principal == "Incidencia") {
        nombres_columnas <- c(
          input$agrupacion_principal,
          "Total Registros",
          "Levantados", 
          "No Levantados con Incidencia",
          "Porcentaje (%)"
        )
      } else {
        nombres_columnas <- c(
          input$agrupacion_principal,
          "Total Registros",
          "Levantados", 
          "No Levantados con Incidencia"
        )
      }
    } else {
      if (input$agrupacion_principal == "Incidencia") {
        nombres_columnas <- c(
          input$agrupacion_principal,
          input$agrupacion_secundaria,
          "Total Registros",
          "Levantados",
          "No Levantados con Incidencia",
          "Porcentaje (%)"
        )
      } else {
        nombres_columnas <- c(
          input$agrupacion_principal,
          input$agrupacion_secundaria,
          "Total Registros",
          "Levantados",
          "No Levantados con Incidencia"
        )
      }
    }
    
    # Configurar la tabla con nombres de columnas dinámicos
    datatable(
      tabla_datos,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/Spanish.json'
        )
      ),
      rownames = FALSE,
      colnames = nombres_columnas,
      filter = "top"   # Filtros interactivos en la parte superior
    ) %>%
      formatStyle(
        columns = c("Total_Registros", "Contenedores_Levantados", "Contenedores_No_Levantados_Incidencias"),
        backgroundColor = "#f8f9fa"
      ) %>%
      {
        tabla <- .
        # Solo aplicar formato a Porcentaje_Incidencias si existe la columna
        if (input$agrupacion_principal == "Incidencia" && "Porcentaje_Incidencias" %in% names(tabla_datos)) {
          tabla %>% formatStyle(
            columns = "Porcentaje_Incidencias",
            backgroundColor = "#e8f5e8",
            color = "#2E8B57",
            fontWeight = "bold"
          )
        } else {
          tabla
        }
      }
  })
  
  # Función de descarga
  output$btn_descargar <- downloadHandler(
    filename = function() {
      paste0(
        "reporte_incidencias_",
        format(input$fecha_reporte[1], "%Y%m%d"),
        "_a_",
        format(input$fecha_reporte[2], "%Y%m%d"),
        "_agrupado_por_",
        input$agrupacion_principal,
        ifelse(input$agrupacion_secundaria != "ninguna", 
               paste0("_y_", input$agrupacion_secundaria), ""),
        ".xlsx"
      )
    },
    content = function(file) {
      req(datos_agregados())
      
      # Crear el libro de trabajo
      wb <- createWorkbook()
      
      # Añadir hoja de resumen
      addWorksheet(wb, "Resumen Agregado")
      
      # Preparar datos para Excel
      datos_excel <- datos_agregados()
      
      # Si se agrupa por Incidencia, asegurar que la columna de porcentaje esté presente
      if (input$agrupacion_principal == "Incidencia" && !"Porcentaje_Incidencias" %in% names(datos_excel)) {
        total_incidencias <- sum(datos_excel$Contenedores_No_Levantados_Incidencias, na.rm = TRUE)
        datos_excel <- datos_excel %>%
          mutate(
            Porcentaje_Incidencias = round((Contenedores_No_Levantados_Incidencias / total_incidencias) * 100, 2)
          )
      }
      
      # Escribir datos
      writeDataTable(wb, sheet = "Resumen Agregado", x = datos_excel, 
                    tableStyle = "TableStyleLight9")
      
      # Ajustar ancho de columnas
      setColWidths(wb, sheet = "Resumen Agregado", cols = 1:ncol(datos_excel), 
                   widths = "auto")
      
      # Añadir hoja de métricas generales
      addWorksheet(wb, "Métricas Generales")
      
      metricas_generales <- data.frame(
        Métrica = c("Total Registros", "Contenedores Levantados", "No Levantados con Incidencia"),
        Valor = c(
          format(nrow(datos_filtrados()), big.mark = ","),
          format(sum(!is.na(datos_filtrados()$Porcentaje_llenado), na.rm = TRUE), big.mark = ","),
          format(sum(is.na(datos_filtrados()$Porcentaje_llenado), na.rm = TRUE), big.mark = ",")
        )
      )
      
      writeDataTable(wb, sheet = "Métricas Generales", x = metricas_generales, 
                    tableStyle = "TableStyleLight9")
      
      # Guardar archivo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

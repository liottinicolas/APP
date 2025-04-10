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
            min = as.Date(CONFIGURACION$FECHA_INICIO_HISTORICO_LLENADO),
            max = ultima_fecha_registro
          ),
          actionButton(inputId = ns("btn_buscar_porgid"), label = "Buscar")
        ),
      ),
      column(
        width = 9,
        div(style = "height: 100%;") # Espacio vacío
      )
    ),
    
    # Panel de Dashboard - Solo visible después de buscar
    fluidRow(
      column(
        width = 12,
        uiOutput(ns("dashboard_panel"))
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

busquedaGidServer <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive para la búsqueda
  llenado <- eventReactive(input$btn_buscar_porgid, {
    req(input$txt_busqueda_gid)  # Asegura que el input no esté vacío

    web_historico_completo_llenado_incidencias %>%
      filter(gid == input$txt_busqueda_gid) %>%
      filter(Fecha >= input$fecha_busqueda_gid[1]) %>%
      filter(Fecha <= input$fecha_busqueda_gid[2]) %>% 
      arrange(desc(Fecha),desc(Turno))
  })
  
  # Dashboard panel que se renderiza después de la búsqueda
  output$dashboard_panel <- renderUI({
    req(llenado())
    data <- llenado()
    
    if (nrow(data) == 0) return(NULL)
    
    # Calcular métricas para el dashboard
    total_registros <- nrow(data)
    primer_registro <- min(data$Fecha)
    ultimo_registro <- max(data$Fecha)
    
    # Obtener la dirección del GID
    direccion <- unique(data$Direccion)[1]
    
    # Calcular porcentajes de levante según la lógica correcta:
    # Si tiene % de llenado (incluso 0%), fue levantado
    # Si tiene incidencia o no tiene datos en porcentaje, no fue levantado
    levantados <- sum(!is.na(data$Porcentaje_llenado), na.rm = TRUE)
    no_levantados <- sum(is.na(data$Porcentaje_llenado) | (!is.na(data$Incidencia) & data$Incidencia != ""), na.rm = TRUE)
    porc_levantados <- round((levantados / total_registros) * 100, 1)
    
    # Promedio de llenado (de los contenedores levantados)
    promedio_llenado <- round(mean(data$Porcentaje_llenado, na.rm = TRUE), 1)
    
    tagList(
      div(
        style = "background-color: #f5f5f5; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
        h3(paste("Dashboard para GID:", input$txt_busqueda_gid, "-", direccion)),
        
        # Panel de información básica
        fluidRow(
          column(
            width = 3,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Resumen", style = "margin-top: 0;"),
              p(sprintf("Total registros: %d", total_registros)),
              p(sprintf("Primer registro: %s", primer_registro)),
              p(sprintf("Último registro: %s", ultimo_registro))
            )
          ),
          column(
            width = 3,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Levantes", style = "margin-top: 0;"),
              div(
                style = sprintf("background-color: %s; height: 30px; width: %s%%; border-radius: 5px;", 
                               ifelse(porc_levantados > 50, "#4CAF50", "#F44336"), porc_levantados)
              ),
              p(sprintf("%s%% levantados (%d de %d)", porc_levantados, levantados, total_registros)),
              p(sprintf("No levantados: %d", no_levantados))
            )
          ),
          column(
            width = 3,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Llenado", style = "margin-top: 0;"),
              p(sprintf("Promedio de llenado: %s%%", promedio_llenado)),
              p(sprintf("Min: %s%% | Max: %s%%", 
                       round(min(data$Porcentaje_llenado, na.rm = TRUE), 1),
                       round(max(data$Porcentaje_llenado, na.rm = TRUE), 1)))
            )
          ),
          column(
            width = 3,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Incidencias", style = "margin-top: 0;"),
              p(sprintf("Principales motivos:")),
              renderText({
                incidencias <- table(data$Incidencia)
                if (length(incidencias) > 0) {
                  top_incidencias <- sort(incidencias, decreasing = TRUE)[1:min(3, length(incidencias))]
                  paste(names(top_incidencias), collapse = ", ")
                } else {
                  "Sin incidencias registradas"
                }
              })
            )
          )
        ),
        
        br(),
        
        # Gráficos de distribución
        fluidRow(
          column(
            width = 6,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Distribución de llenado", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_llenado_distribucion"))
            )
          ),
          column(
            width = 6,
            div(
              style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
              h4("Distribución de incidencias", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_incidencias"))
            )
          )
        )
      )
    )
  })
  
  # Gráfico de distribución de llenado
  output$grafico_llenado_distribucion <- renderPlotly({
    req(llenado())
    data <- llenado()
    
    if (nrow(data) == 0) return(NULL)
    
    # Contar por valor de llenado
    distribucion_llenado <- data %>%
      mutate(
        Porcentaje_llenado = ifelse(is.na(Porcentaje_llenado), "No levantado", 
                                   paste0(round(Porcentaje_llenado), "%"))
      ) %>%
      count(Porcentaje_llenado) %>%
      arrange(desc(n))
    
    # Si hay demasiadas categorías, agrupar las menos comunes
    if (nrow(distribucion_llenado) > 5) {
      top_valores <- distribucion_llenado[1:5,]
      otras <- data.frame(
        Porcentaje_llenado = "Otros",
        n = sum(distribucion_llenado$n[6:nrow(distribucion_llenado)])
      )
      distribucion_llenado <- rbind(top_valores, otras)
    }
    
    # Definir colores según el valor
    colores <- sapply(distribucion_llenado$Porcentaje_llenado, function(x) {
      if (x == "100%") return("#000000")  # Negro
      if (x == "75%") return("#FF0000")   # Rojo
      if (x == "50%") return("#FFD700")   # Amarillo
      if (x == "25%") return("#00FF00")   # Verde
      if (x == "No levantado") return("#808080")  # Gris
      return("#A9A9A9")  # Gris claro para otros valores
    })
    
    # Crear gráfico de torta
    p <- plot_ly(distribucion_llenado, 
                labels = ~Porcentaje_llenado, 
                values = ~n, 
                type = 'pie',
                textinfo = 'label+percent',
                insidetextorientation = 'radial',
                marker = list(colors = colores)) %>%
      layout(
        title = "",
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.2
        )
      )
    
    p
  })
  
  # Gráfico de distribución de incidencias
  output$grafico_incidencias <- renderPlotly({
    req(llenado())
    data <- llenado()
    
    if (nrow(data) == 0) return(NULL)
    
    # Contar incidencias
    incidencias_df <- data %>%
      filter(!is.na(Incidencia) & Incidencia != "") %>%
      count(Incidencia) %>%
      arrange(desc(n))
    
    # Si hay demasiadas categorías, agrupar las menos comunes
    if (nrow(incidencias_df) > 5) {
      top_incidencias <- incidencias_df[1:5,]
      otras <- data.frame(
        Incidencia = "Otras",
        n = sum(incidencias_df$n[6:nrow(incidencias_df)])
      )
      incidencias_df <- rbind(top_incidencias, otras)
    }
    
    # Crear gráfico de torta
    p <- plot_ly(incidencias_df, labels = ~Incidencia, values = ~n, type = 'pie',
                textinfo = 'label+percent',
                insidetextorientation = 'radial') %>%
      layout(title = "")
    
    p
  })
  
  # Mostrar los datos filtrados en la tabla
  output$tabla_historico_contenedor <- renderDT({
    data <- llenado()  # Asigna el resultado del eventReactive a `data`

    if (nrow(data) > 0) {

      # transformo el dia-fecha a solo hora
      data$Fecha_hora_pasaje <- ifelse(is.na(data$Fecha_hora_pasaje), "", format(data$Fecha_hora_pasaje, "%H:%M:%S"))
      
      # Determinar estado de levante según lógica correcta para el estilo
      data$Levantado_real <- ifelse(!is.na(data$Porcentaje_llenado), "SI", "NO")

      data %>%
        select(Fecha,Circuito,Posicion,Direccion,Levantado_real,Turno,Fecha_hora_pasaje,Id_viaje,Incidencia,
               Porcentaje_llenado,Condicion) %>%
        datatable(
          colnames = c("Dia","Circuito", "Posicion", "Dirección","Levante", "Turno", "Hora", "ID_Viaje", "Incidencia", "% Llenado", "Condición"),
          filter = "top",
          options = list(lengthMenu = c(10, 25, 50, 100),
                         pageLength = 100,
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
        ) %>%
        formatStyle(
          'Levantado_real',
          backgroundColor = styleEqual(c("SI", "NO"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          'Porcentaje_llenado',
          background = styleColorBar(c(0, 100), '#90caf9'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    } else {
      datatable(data.frame(message = "Sin datos para mostrar"),
                options = list(pageLength = 1, dom = 't'),
                rownames = FALSE)
    }
  })
}

# nolint end
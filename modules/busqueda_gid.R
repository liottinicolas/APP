# nolint start: line_length_linter, object_name_linter

busquedaGidUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Agrego el estilo CSS para letra más chica al principio del UI
    tags$style(HTML(".small-font { font-size: 14px !important; }")),
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
      left_join(
        web_circuitos_planificados %>% 
          select(circuito_corto, Frecuencia, Periodo),
        by = c("Circuito_corto" = "circuito_corto")
      ) %>%
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
        # fluidRow(
        #   column(
        #     width = 3,
        #     div(
        #       style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
        #       h4("Resumen", style = "margin-top: 0;"),
        #       p(sprintf("Total registros: %d", total_registros)),
        #       p(sprintf("Primer registro: %s", primer_registro)),
        #       p(sprintf("Último registro: %s", ultimo_registro))
        #     )
        #   ),
        #   column(
        #     width = 3,
        #     div(
        #       style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
        #       h4("Levantes", style = "margin-top: 0;"),
        #       div(
        #         style = sprintf("background-color: %s; height: 30px; width: %s%%; border-radius: 5px;", 
        #                        ifelse(porc_levantados > 50, "#4CAF50", "#F44336"), porc_levantados)
        #       ),
        #       p(sprintf("%s%% levantados (%d de %d)", porc_levantados, levantados, total_registros)),
        #       p(sprintf("No levantados: %d", no_levantados))
        #     )
        #   ),
        #   column(
        #     width = 3,
        #     div(
        #       style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
        #       h4("Llenado", style = "margin-top: 0;"),
        #       p(sprintf("Promedio de llenado: %s%%", promedio_llenado)),
        #       p(sprintf("Min: %s%% | Max: %s%%", 
        #                round(min(data$Porcentaje_llenado, na.rm = TRUE), 1),
        #                round(max(data$Porcentaje_llenado, na.rm = TRUE), 1)))
        #     )
        #   ),
        #   column(
        #     width = 3,
        #     div(
        #       style = "background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12);",
        #       h4("Incidencias", style = "margin-top: 0;"),
        #       p(sprintf("Principales motivos:")),
        #       renderText({
        #         incidencias <- table(data$Incidencia)
        #         if (length(incidencias) > 0) {
        #           top_incidencias <- sort(incidencias, decreasing = TRUE)[1:min(3, length(incidencias))]
        #           paste(names(top_incidencias), collapse = ", ")
        #         } else {
        #           "Sin incidencias registradas"
        #         }
        #       })
        #     )
        #   )
        # ),
        
        br(),
        
        # Fila superior: Distribución de llenado y Levantados, más juntos
        fluidRow(
          column(
            width = 6,
            div(
              style = "background-color: white; padding: 5px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); font-size: 0.9em; padding-right: 5px;",
              h4("Distribución de llenado", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_llenado_distribucion"), height = "250px"),
              div(
                style = "font-size: 1.0em; color: #666; margin-top: 5px; text-align: center;",
                "Muestra el porcentaje de llenado de contenedores que fueron levantados, independientemente de los días de acumulación sin contemplar si está o no en régimen"
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "background-color: white; padding: 5px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); font-size: 0.9em; padding-left: 5px;",
              h4("Levantados", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_levantados"), height = "250px"),
              div(
                style = "font-size: 1.0em; color: #666; margin-top: 5px; text-align: center;",
                "Comparación de contenedores levantados vs no levantados en viajes realizados"
              )
            )
          )
        ),
        # Espacio entre filas
        div(style = "height: 25px;"),
        # Fila inferior: Distribución de incidencias
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: white; padding: 5px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); font-size: 0.9em;",
              h4("Distribución de incidencias", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_incidencias"), height = "250px"),
              div(
                style = "font-size: 1.0em; color: #666; margin-top: 5px; text-align: center;",
                "Muestra las principales incidencias registradas en el período"
              )
            )
          )
        ),
        # Espacio entre filas
        div(style = "height: 25px;"),
        # Nueva fila: Planificado vs Levantado
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: white; padding: 5px; border-radius: 5px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); font-size: 0.9em;",
              h4("Planificado vs Levantado", style = "margin-top: 0;"),
              plotlyOutput(ns("grafico_planificado_vs_levantado"), height = "250px"),
              div(
                style = "font-size: 1.0em; color: #666; margin-top: 5px; text-align: center;",
                "Compara la cantidad de levantados planificados vs realizados en el período seleccionado"
              )
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
      filter(!is.na(Porcentaje_llenado)) %>%
      mutate(
        valor_llenado = round(Porcentaje_llenado),
        Porcentaje_llenado = paste0(valor_llenado, "%"),
        etiqueta = paste0(valor_llenado, "% llenado")
      ) %>%
      count(Porcentaje_llenado, etiqueta, valor_llenado) %>%
      arrange(desc(valor_llenado)) %>%
      select(-valor_llenado)
    
    # Si hay demasiadas categorías, agrupar las menos comunes
    if (nrow(distribucion_llenado) > 5) {
      top_valores <- distribucion_llenado[1:5,]
      otras <- data.frame(
        Porcentaje_llenado = "Otros",
        etiqueta = "Otros",
        n = sum(distribucion_llenado$n[6:nrow(distribucion_llenado)])
      )
      distribucion_llenado <- rbind(top_valores, otras)
    }
    
    # Paleta de azules (más oscuro para mayor %)
    suppressWarnings({
      if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
    })
    library(RColorBrewer)
    n_cats <- nrow(distribucion_llenado)
    # Si hay "Otros", que sea gris claro
    tiene_otros <- any(distribucion_llenado$Porcentaje_llenado == "Otros")
    n_blues <- ifelse(tiene_otros, n_cats - 1, n_cats)
    paleta_azul <- rev(colorRampPalette(brewer.pal(9, "Blues")[3:9])(n_blues))  # Empezamos desde el tercer color
    if (tiene_otros) {
      colores_azules <- c(paleta_azul, "#A9A9A9")  # Gris más oscuro
    } else {
      colores_azules <- paleta_azul
    }
    
    # Crear gráfico de torta
    p <- plot_ly(distribucion_llenado, 
                labels = ~etiqueta, 
                values = ~n, 
                type = 'pie',
                textinfo = 'label+percent',
                insidetextorientation = 'radial',
                marker = list(colors = colores_azules)) %>%
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
      arrange(n)  # De menor a mayor para barras horizontales
    
    # Si hay demasiadas categorías, agrupar las menos comunes
    if (nrow(incidencias_df) > 5) {
      top_incidencias <- incidencias_df[(nrow(incidencias_df)-4):nrow(incidencias_df),]
      otras <- data.frame(
        Incidencia = "Otras",
        n = sum(incidencias_df$n[1:(nrow(incidencias_df)-5)])
      )
      incidencias_df <- rbind(otras, top_incidencias)
    }
    
    # Paleta de azules: más oscuro para mayor cantidad
    suppressWarnings({
      if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
    })
    library(RColorBrewer)
    n_cats <- nrow(incidencias_df)
    paleta_azul <- colorRampPalette(brewer.pal(9, "Blues")[3:9])(n_cats)  # Empezamos desde el tercer color
    
    plot_ly(
      incidencias_df,
      x = ~n,
      y = ~reorder(Incidencia, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = paleta_azul),
      text = ~n,
      textposition = 'auto'
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Cantidad"),
        yaxis = list(title = ""),
        margin = list(l = 100)
      )
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
                           list(width = '20%', targets = 3,className = 'dt-center'), # Direccion (más ancha)
                           list(width = '5%', targets = 4), # Levante (más chica)
                           list(targets = 5,className = 'dt-center'), # Turno
                           list(width = '5%', targets = 6,className = 'dt-center'), # Hora
                           list(width = '5%', targets = 7,className = 'dt-center'), # ID_Viaje
                           list(width = '20%', targets = 8,className = 'dt-center'), #Incidencia
                           list(width = '5%', targets = 9), # llenado
                           list(width = '20%',targets = 10,className = 'dt-center') # Condicion
                         )
          ),
          rownames = FALSE,
          class = "compact stripe hover small-font"
        ) %>%
        formatStyle(
          'Levantado_real',
          backgroundColor = styleEqual(c("SI", "NO"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          'Porcentaje_llenado',
          # background = styleColorBar(c(0, 100), '#90caf9'),
          # backgroundSize = '98% 88%',
          # backgroundRepeat = 'no-repeat',
          # backgroundPosition = 'center'
        )
    } else {
      datatable(data.frame(message = "Sin datos para mostrar"),
                options = list(pageLength = 1, dom = 't'),
                rownames = FALSE)
    }
  })

  output$grafico_levantados <- renderPlotly({
    req(llenado())
    data <- llenado()
    if (nrow(data) == 0) return(NULL)

    levantados <- sum(!is.na(data$Porcentaje_llenado))
    no_levantados <- sum(is.na(data$Porcentaje_llenado))

    df_levantados <- data.frame(
      Estado = c("Levantado", "No levantado"),
      Cantidad = c(levantados, no_levantados)
    )

    # Paleta de azules más oscura
    suppressWarnings({
      if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
    })
    library(RColorBrewer)
    colores_azules <- colorRampPalette(brewer.pal(9, "Blues")[4:9])(2)  # Usando colores más oscuros

    plot_ly(
      df_levantados,
      labels = ~Estado,
      values = ~Cantidad,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      marker = list(colors = colores_azules)
    ) %>%
      layout(title = "")
  })

  # Gráfico de Planificado vs Levantado
  output$grafico_planificado_vs_levantado <- renderPlotly({
    req(llenado())
    data <- llenado()
    
    if (nrow(data) == 0) return(NULL)
    
    # Calcular levantados por mes
    levantados_mes <- data %>%
      filter(!is.na(Porcentaje_llenado)) %>%
      mutate(mes = format(Fecha, "%Y-%m")) %>%
      group_by(mes) %>%
      reframe(levantados = n()) %>%
      arrange(mes)
    
    # Calcular planificado por mes considerando meses parciales
    planificado_mes <- data %>%
      mutate(mes = format(Fecha, "%Y-%m")) %>%
      group_by(mes) %>%
      reframe(
        primer_dia = min(Fecha),
        ultimo_dia = max(Fecha),
        periodo = first(Periodo)
      ) %>%
      mutate(
        # Para el primer mes, contar desde el primer día registrado
        dias_primer_mes = as.numeric(format(primer_dia, "%d")),
        # Para el último mes, contar hasta el último día registrado
        dias_ultimo_mes = as.numeric(format(ultimo_dia, "%d")),
        # Para meses intermedios, usar todos los días del mes
        dias_mes = case_when(
          mes == min(mes) ~ as.numeric(format(as.Date(paste0(mes, "-01")) + months(1) - 1, "%d")) - dias_primer_mes + 1,
          mes == max(mes) ~ dias_ultimo_mes,
          TRUE ~ as.numeric(format(as.Date(paste0(mes, "-01")) + months(1) - 1, "%d"))
        ),
        planificado = round(dias_mes / periodo)
      ) %>%
      select(mes, planificado) %>%
      arrange(mes)
    
    # Unir los datos
    datos_comparacion <- levantados_mes %>%
      left_join(planificado_mes, by = "mes") %>%
      mutate(
        mes_formateado = format(as.Date(paste0(mes, "-01")), "%b %Y"),
        fecha_orden = as.Date(paste0(mes, "-01"))
      ) %>%
      arrange(fecha_orden) %>%
      select(mes = mes_formateado, levantados, planificado)
    
    # Crear gráfico de barras horizontales
    plot_ly(datos_comparacion) %>%
      add_trace(
        y = ~mes,
        x = ~planificado,
        name = "Planificado",
        type = "bar",
        orientation = "h",
        marker = list(color = "#2c3e50")  # Color más oscuro para planificado
      ) %>%
      add_trace(
        y = ~mes,
        x = ~levantados,
        name = "Levantado",
        type = "bar",
        orientation = "h",
        marker = list(color = "#3498db")  # Color más oscuro para levantado
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Cantidad"),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = rev(~mes)
        ),
        barmode = "group",
        legend = list(
          orientation = "h",
          y = -0.2
        )
      )
  })
}

# nolint end
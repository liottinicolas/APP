condicionContenedorUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Dashboard mejorado
    div(
      class = "panel panel-default",
      style = "margin-bottom: 30px; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); position: relative; z-index: 1;",
      
      div(
        class = "panel-heading",
        style = "background-color: #2c3e50; color: white; border-radius: 8px 8px 0 0; padding: 15px 20px;",
        h3(style = "margin: 0; font-weight: 500;", "Dashboard de Condiciones de Contenedores")
      ),
      
      div(
        class = "panel-body",
        style = "padding: 20px; background-color: #f8f9fa;",
        
        # Filtros en fila horizontal con mejor estilo
        div(
          style = "display: flex; justify-content: flex-start; align-items: center; gap: 20px; margin-bottom: 25px; flex-wrap: wrap;",
          
          div(
            style = "min-width: 200px;",
            div(
              style = "font-weight: 600; margin-bottom: 8px; color: #2c3e50;",
              "Fecha del informe:"
            ),
            dateInput(
              ns("filtro_fecha_resumen"),
              label = NULL,
              value = max(historico_completo_llenado_incidencias$Fecha),
              min = as.Date("2024-10-10"),
              max = max(historico_completo_llenado_incidencias$Fecha),
              width = '100%'
            )
          ),
          
          div(
            style = "min-width: 200px;",
            div(
              style = "font-weight: 600; margin-bottom: 8px; color: #2c3e50;",
              "Municipio:"
            ),
            selectInput(
              ns("filtro_municipio_resumen"),
              label = NULL,
              choices = c("Todos", unique(historico_completo_llenado_incidencias$Municipio)),
              selected = "Todos",
              width = '100%'
            )
          ),
          
          div(
            style = "min-width: 200px;",
            div(
              style = "font-weight: 600; margin-bottom: 8px; color: #2c3e50;",
              "Circuito:"
            ),
            selectInput(
              ns("filtro_circuito_resumen"),
              label = NULL,
              choices = c("Todos", unique(historico_completo_llenado_incidencias$Circuito_corto)),
              selected = "Todos",
              width = '100%'
            )
          )
        ),
        
        # Cards de resumen con mejor estilo
        div(
          style = "margin-bottom: 25px;",
          uiOutput(ns("tarjetas_resumen"))
        ),
        
        # Contenedor para el gráfico de condiciones (ahora a todo ancho)
        div(
          style = "background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); padding: 20px; margin-bottom: 40px; overflow: hidden; position: relative; z-index: 1;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            h4(
              style = "margin: 0; font-weight: 600; color: #2c3e50;",
              "Distribución de Condiciones"
            ),
            div(
              style = "font-style: italic; color: #7f8c8d; font-size: 14px;",
              "* Este gráfico muestra un resumen de las condiciones que encontrará en la tabla de datos abajo"
            )
          ),
          div(
            style = "height: 250px; width: 100%; position: relative;",
            plotOutput(ns("grafico_condiciones"), height = "250px")
          )
        )
      )
    ),
    
    # Tabla existente con estilos mejorados
    div(
      class = "panel panel-default",
      style = "border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); margin-top: 40px; position: relative; z-index: 2;",
      
      div(
        class = "panel-heading",
        style = "background-color: #2c3e50; color: white; border-radius: 8px 8px 0 0; padding: 15px 20px;",
        h3(style = "margin: 0; font-weight: 500;", "Listado Detallado de Condiciones")
      ),
      
      div(
        class = "panel-body",
        style = "padding: 20px; background-color: #f8f9fa;",
        
        fluidRow(
          column(
            width = 2,
            div(
              style = "background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); padding: 20px;",
              
              div(
                style = "font-weight: 600; margin-bottom: 8px; color: #2c3e50;",
                "Fecha:"
              ),
              dateInput(
                ns("filtro_fecha_condicion"),
                label = NULL,
                value = max(historico_completo_llenado_incidencias$Fecha),
                min = as.Date("2024-10-10"),
                max = max(historico_completo_llenado_incidencias$Fecha),
                width = '100%'
              ),
              
              div(
                style = "margin-top: 20px; font-weight: 600; margin-bottom: 8px; color: #2c3e50;",
                "Condiciones:"
              ),
              div(
                style = "max-height: 400px; overflow-y: auto;",
                radioButtons(
                  inputId = ns("radioBtn_condiciones"),
                  label = NULL,
                  choices = cargar_opciones_condiciones()
                )
              )
            )
          ),
          
          column(
            width = 10,
            div(
              style = "background: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); padding: 20px;",
              DTOutput(ns("tabla_condicion_contenedores"))
            )
          )
        )
      )
    )
  )
}

condicionContenedorServer <- function(input, output, session) {
  ns <- session$ns
  
  # Datos reactivos para el dashboard de resumen
  datos_filtrados_resumen <- reactive({
    req(input$filtro_fecha_resumen) # Asegurarse de que la fecha está disponible
    
    # Filtra inicialmente por la fecha seleccionada
    df <- historico_completo_llenado_incidencias %>%
      filter(Fecha == input$filtro_fecha_resumen) %>%
      filter(!is.na(Condicion) & Condicion != "")
    
    # Filtrar por municipio si no es "Todos"
    if(input$filtro_municipio_resumen != "Todos") {
      df <- df %>% filter(Municipio == input$filtro_municipio_resumen)
    }
    
    # Filtrar por circuito si no es "Todos"
    if(input$filtro_circuito_resumen != "Todos") {
      df <- df %>% filter(Circuito_corto == input$filtro_circuito_resumen)
    }
    
    return(df)
  })
  
  # Renderizar las tarjetas de resumen
  output$tarjetas_resumen <- renderUI({
    df <- datos_filtrados_resumen()
    
    total_contenedores_con_condicion <- nrow(df)
    
    # Crear contadores para cada tipo de condición
    contador_condiciones <- sapply(cargar_opciones_condiciones()[-1], function(cond) {
      sum(str_detect(df$Condicion, fixed(cond, ignore_case = TRUE)))
    })
    
    # Colores para las tarjetas
    colores <- c(
      "#3498db", # Total (Azul)
      "#f39c12", # Basura afuera (Naranja)
      "#e74c3c", # Dos ciclos (Rojo)
      "#9b59b6", # Escombro (Morado)
      "#2980b9", # Fuera de lugar (Azul oscuro)
      "#27ae60", # Poda (Verde)
      "#16a085", # Requiere limpieza (Verde azulado)
      "#d35400"  # Requiere Mantenimiento (Naranja oscuro)
    )
    
    # Tarjeta para el total
    tarjeta_total <- div(
      style = paste0(
        "background: linear-gradient(135deg, ", colores[1], ", #2c3e50);",
        "border-radius: 10px;",
        "padding: 20px;",
        "color: white;",
        "box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
        "display: flex;",
        "flex-direction: column;",
        "justify-content: space-between;",
        "min-width: 200px;"
      ),
      div(
        style = "font-size: 16px; font-weight: 500; margin-bottom: 5px;",
        "Total"
      ),
      div(
        style = "font-size: 28px; font-weight: 700; margin-bottom: 5px;",
        total_contenedores_con_condicion
      ),
      div(
        style = "font-size: 14px; opacity: 0.8;",
        "Contenedores con condiciones"
      )
    )
    
    # Crear tarjetas para cada condición
    tarjetas <- lapply(seq_along(contador_condiciones), function(i) {
      condicion <- names(contador_condiciones)[i]
      cantidad <- contador_condiciones[i]
      
      div(
        style = paste0(
          "background: linear-gradient(135deg, ", colores[i+1], ", #34495e);",
          "border-radius: 10px;",
          "padding: 20px;",
          "color: white;",
          "box-shadow: 0 4px 15px rgba(0,0,0,0.1);",
          "display: flex;",
          "flex-direction: column;",
          "justify-content: space-between;",
          "min-width: 200px;"
        ),
        div(
          style = "font-size: 16px; font-weight: 500; margin-bottom: 5px;",
          condicion
        ),
        div(
          style = "font-size: 28px; font-weight: 700; margin-bottom: 5px;",
          cantidad
        ),
        div(
          style = "font-size: 14px; opacity: 0.8;",
          "Contenedores"
        )
      )
    })
    
    # Crear una grid de tarjetas
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 20px;",
      tarjeta_total,
      tarjetas
    )
  })
  
  # Gráfico de barras para las condiciones
  output$grafico_condiciones <- renderPlot({
    df <- datos_filtrados_resumen()
    
    # Contar ocurrencias de cada tipo de condición
    contador_condiciones <- sapply(cargar_opciones_condiciones()[-1], function(cond) {
      sum(str_detect(df$Condicion, fixed(cond, ignore_case = TRUE)))
    })
    
    # Crear dataframe para ggplot
    df_grafico <- data.frame(
      Condicion = names(contador_condiciones),
      Cantidad = as.numeric(contador_condiciones)
    ) %>%
      arrange(desc(Cantidad))
    
    # Colores para las barras
    colores_barras <- c(
      "#f39c12", "#e74c3c", "#9b59b6", "#2980b9", "#27ae60", "#16a085", "#d35400"
    )
    
    # Crear gráfico más compacto
    ggplot(df_grafico, aes(x = reorder(Condicion, Cantidad), y = Cantidad, fill = Condicion)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = colores_barras) +
      labs(title = "", x = "", y = "Cantidad") +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "#333333"),
        axis.title = element_text(size = 12, face = "bold", color = "#333333"),
        plot.margin = margin(5, 5, 5, 5)
      )
  }, height = 250)
  
  # Filtrado reactivo para la tabla de contenedores
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
      select(Fecha, gid, Municipio, Circuito_corto, Posicion, Direccion, Turno, Porcentaje_llenado, Condicion) %>% 
      datatable(
        filter = "top",
        options = list(
          lengthMenu = c(10, 25, 50, 100),
          pageLength = 25,
          dom = 'Bfrtip',
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
  })
  
}

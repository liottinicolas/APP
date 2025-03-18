historicoIncidenciasUI <- function(id) {
  
  
  ns <- NS(id)
  tagList(
    

      fluidRow(
        box(
          #title = "Contenedores con sobrepesos",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("grafica_grua"), height = "500px"),
          downloadButton(ns("descargar_datos_grua"), "Descargar tabla") # Botón de descarga
          
        ),
        
        box(
          #title = "Contenedores con grua",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("grafica_pluma"), height = "500px"),
          downloadButton(ns("descargar_datos_pluma"), "Descargar tabla") # Botón de descarga
          
        )
      )
      
    
    
    
  )
}

historicoIncidenciasServer <- function(input, output, session) {
  ns <- session$ns
  
  output$grafica_grua <- renderPlotly({
    # Crear la gráfica base
    grafica_base <- ggplot(historico_generados_pendientes_GRUA_app, aes(x = Fecha)) +
      geom_line(aes(y = Generados, color = "Generados por día"), size = 1) +
      geom_line(aes(y = Pendientes, color = "Acumulados"), size = 1) +
      scale_color_manual(values = c("Generados por día" = "blue", "Acumulados" = "orange")) +
      labs(
        title = "Servicios para la Grua",
        subtitle = "Contenedores con sobrepeso",
        x = "Fecha",
        y = "Cantidad de contenedores",
        color = NULL
      ) +
      scale_x_date(
        date_breaks = "3 days",  # Etiquetas cada 3 días
        date_labels = "%d/%m/%Y"  # Formato de fecha
      ) +
      guides(x = guide_axis(angle = 45)) +  # Ajusta las etiquetas del eje X
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    # Convertir la gráfica a interactiva con Plotly
    ggplotly(grafica_base) %>% 
      layout(
        title = list(
          text = "Servicios para la Grua<br><sub>Contenedores con sobrepeso</sub>"
        ),
        margin = list(t = 80), # Ajusta el margen superior (t = top)
        xaxis = list(
          tickangle = -45,         # Inclinación de las etiquetas del eje X
          tickformat = "%d/%m/%Y" # Formato de la fecha
        )
      )
  })
  
  historico_generados_pendientes_GRUA_app_reactivo <- reactive({
    historico_generados_pendientes_GRUA_app
  })
  
  # Descargar los datos en Excel
  output$descargar_datos_grua <- downloadHandler(
    filename = function() {
      paste("Historico_Grua_Pendientes_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Crear un workbook
      wb <- openxlsx::createWorkbook()
      
      # Agregar una hoja
      openxlsx::addWorksheet(wb, "Datos")
      
      # Escribir los datos con formato de tabla
      openxlsx::writeDataTable(
        wb,
        sheet = "Datos",
        x = historico_generados_pendientes_GRUA_app_reactivo(),
        tableStyle = "TableStyleMedium2" # Estilo predefinido
      )
      
      # Guardar el archivo Excel
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  output$grafica_pluma <- renderPlotly({
    # Crear la gráfica base
    grafica_base <- ggplot(historico_generados_pendientes_PLUMA_app, aes(x = Fecha)) +
      geom_line(aes(y = Generados, color = "Generados por día"), size = 1) +
      geom_line(aes(y = Pendientes, color = "Acumulados"), size = 1) +
      scale_color_manual(values = c("Generados por día" = "darkslategray", "Acumulados" = "indianred")) +
      labs(
        title = "Servicios para la Pluma",
        subtitle = "Contenedores fuera de alcance, cruzados o buzonera girada",
        y = "Cantidad de contenedores",
        color = NULL
      ) +
      scale_x_date(
        date_breaks = "3 days",  # Etiquetas cada 3 días
        date_labels = "%d/%m/%Y"  # Formato de fecha
      ) +
      guides(x = guide_axis(angle = 45)) +  # Ajusta las etiquetas del eje X
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    
    # Convertir la gráfica a interactiva con Plotly
    ggplotly(grafica_base) %>% 
      layout(
        title = list(
          text = "Servicios para la Pluma<br><sub>Contenedores fuera de alcance, cruzados o buzonera girada</sub>"
        ),
        margin = list(t = 80), # Ajusta el margen superior (t = top)
        xaxis = list(
          tickangle = -45,         # Inclinación de las etiquetas del eje X
          tickformat = "%d/%m/%Y" # Formato de la fecha
        )
      )
  })
  
  historico_generados_pendientes_PLUMA_app_reactivo <- reactive({
    historico_generados_pendientes_PLUMA_app
  })
  
  # Descargar los datos en Excel
  output$descargar_datos_pluma <- downloadHandler(
    filename = function() {
      paste("Historico_Pluma_Pendientes_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Crear un workbook
      wb <- openxlsx::createWorkbook()
      
      # Agregar una hoja
      openxlsx::addWorksheet(wb, "Datos")
      
      # Escribir los datos con formato de tabla
      openxlsx::writeDataTable(
        wb,
        sheet = "Datos",
        x = historico_generados_pendientes_PLUMA_app_reactivo(),
        tableStyle = "TableStyleMedium2" # Estilo predefinido
      )
      
      # Guardar el archivo Excel
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
}

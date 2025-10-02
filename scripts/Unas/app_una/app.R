# app.R
# Paquetes
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)

# --- Carga de datos ---
# Reemplaza esto por tu carga real. Si ya tienes df_final_sololevantados_UNAmayorA100 en memoria, ignora estas 3 líneas.
# df_final_sololevantados_UNAmayorA100 <- read_csv("tu_archivo.csv", show_col_types = FALSE)

# Estandariza tipos
# app.R
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)



ui <- fluidPage(
  titlePanel("Porcentaje BA/RL por Fecha"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("rango", "Rango de fechas",
                     start = min(df_final_sololevantados_UNAmayorA100$Fecha, na.rm = TRUE),
                     end   = max(df_final_sololevantados_UNAmayorA100$Fecha, na.rm = TRUE)),
      selectInput("mun", "Municipio", choices = sort(unique(df_final_sololevantados_UNAmayorA100$Municipio)),
                  selected = unique(df_final_sololevantados_UNAmayorA100$Municipio), multiple = TRUE),
      selectInput("una", "UNA", choices = sort(unique(df_final_sololevantados_UNAmayorA100$UNA)),
                  selected = unique(df_final_sololevantados_UNAmayorA100$UNA), multiple = TRUE),
      radioButtons("total_var", "Total a graficar",
                   choices = c("total_contenedores_con_misma_UNA",
                               "total_contenedores_BA_RL"),
                   inline = TRUE)
    ),
    mainPanel(plotlyOutput("serie", height = 500))
  )
)

server <- function(input, output, session){
  
  filtrado <- reactive({
    df_final_sololevantados_UNAmayorA100 %>%
      filter(Fecha >= input$rango[1], Fecha <= input$rango[2],
             Municipio %in% input$mun,
             UNA %in% input$una)
  })
  
  output$serie <- plotly::renderPlotly({
    dat <- filtrado() %>% 
      dplyr::arrange(Fecha) %>%
      dplyr::mutate(y_total = .data[[input$total_var]])
    
    req(nrow(dat) > 0)
    
    # Gráfico: X=UNA, Y=total, color=% BA/RL
    plotly::plot_ly(
      data = dat,
      x = ~UNA,
      y = ~y_total,
      type = "scatter",
      mode = "markers",
      color = ~porc_BA_RL,              # lo que "se grafica" es el %
      symbol = ~Municipio,              # para distinguir municipios cuando hay varios
      customdata = ~porc_BA_RL,         # para hover limpio del %
      text = ~Municipio,
      hovertemplate = paste(
        "Municipio: %{text}<br>",
        "UNA: %{x}<br>",
        "Total: %{y}<br>",
        "% BA/RL: %{customdata:.2f}%",
        "<extra></extra>"
      ),
      marker = list(sizemode = "diameter", sizemin = 6)
    ) %>%
      plotly::layout(
        xaxis = list(title = "UNA"),
        yaxis = list(title = input$total_var),
        legend = list(title = list(text = "Municipio")),
        coloraxis = list(colorbar = list(title = "% BA/RL"))
      )
  })
}

shinyApp(ui, server)
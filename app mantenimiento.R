library(shiny)

# Simple maintenance mode Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("\n        body { \n          background-color: #f8f9fa; \n          display: flex;\n          justify-content: center;\n          align-items: center;\n          height: 100vh;\n          margin: 0;\n        }\n        .maintenance {\n          text-align: center;\n          font-family: 'Arial', sans-serif;\n          color: #343a40;\n        }\n        .maintenance h1 {\n          font-size: 4rem;\n          margin-bottom: 1rem;\n        }\n        .maintenance p {\n          font-size: 1.25rem;\n        }\n      ")
    )
  ),
  div(
    class = "maintenance",
    h1("En mantenimiento"),
    p("Ya volvemos!!")
  )
)

server <- function(input, output, session) {
  # No server logic needed for maintenance page
}

# Run the application 
shinyApp(ui = ui, server = server)

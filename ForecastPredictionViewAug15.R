library(library(shiny))

ui <- fluidPage(
  titlePanel("Forecast Prediction View Aug-2015"),
  mainPanel(
    includeHTML("C:/Users/akbhalerao/Documents/Forecast Prediction-Aug15.html")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
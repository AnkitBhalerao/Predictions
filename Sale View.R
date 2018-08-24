library(shiny)
library(library(shiny))

ui <- fluidPage(
  titlePanel("Appliance Sale View"),
  mainPanel(
    includeHTML("C:/Users/akbhalerao/Documents/rbind.html")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
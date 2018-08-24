library(shiny)
library(datasets)

ui <- fluidPage(
  shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Appliance Forecaster"),
    
    # Sidebar with controls to select a dataset and specify the number
    # of observations to view
    sidebarPanel(
      selectInput("dataset", "Choose a Zipcode:", 
                  choices = c(dfcombined$ToZip)),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show a summary of the dataset and an HTML table with the requested
    # number of observations
    mainPanel(
      # verbatimTextOutput("summary"),
      
      tableOutput("view")
    )
    
  )
  
  
  )
)

server <- function(input, output, session) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "dfcombined$ToZip" = dfcombined$ToZip)
  })
  
  # Generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })
  
  # Show the first "n" observations
  output$view <- renderTable({
    includeHTML("C:/Users/sys/Desktop/rbind.html")
    
    # head(datasetInput(), n = input$obs)
  })
  
  
}

shinyApp(ui, server)

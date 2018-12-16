library(shiny)
library(shinydashboard)

ui <- fluidPage(
  numericInput(inputId = "n",
  "Sample size", value = 25),
  plotOutput(outputId = "hist"))

server <- function(input, output){
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
}
shinyApp(ui = ui, server = server)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
server <- function(input, output){}
shinyApp(ui = ui, server = server)

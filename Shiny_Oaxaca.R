library(oaxaca)
library(ggplot2)
library(shiny)
library(data.table)

RegData <- as.data.table(read.csv("C:\\Users\\ambatipudir\\Downloads\\data_r.csv", header = TRUE, stringsAsFactors = TRUE))
RegData$totalPay <- RegData$basePay + RegData$bonus
  
ui <- fluidPage(
  headerPanel("Compensation Analysis using Oaxaca Decomposition"), 
  sidebarPanel(
    p("Select the input parameters on which the compensation must depend"),
    selectInput(inputId = "DependentVariable", label = "Dependent Variables", multiple = TRUE, choices = c('perfEval', 
                                                                                                 'age',                     
                                                                                                 'seniority')),
    p("Select the inputs paramter for finding which of the compensation"),
    selectInput(inputId = "IndependentVariable", label = "Independent Variables", multiple = FALSE, choices = c('totalPay'))
  ),
  mainPanel(
    verbatimTextOutput(outputId = "RegSum"),
    verbatimTextOutput(outputId = "DepPrint"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = 'location'),
    plotOutput("plot", click = 'onplotclick')
  )
)

server <- function(input, output) {
  
  lm1 <- reactive({lm(reformulate(input$DependentVariable, input$IndependentVariable), data = RegData)})
  
  output$DepPrint <- renderPrint({input$DependentVariable})
  output$IndPrint <- renderPrint({input$IndependentVariable})
  output$RegSum <- renderPrint({summary(lm1())})
  output$plot <- renderPlot(ggplot(RegData, aes(x = age+perfEval+seniority, y = totalPay, col = gender)) + geom_point() + geom_smooth(method = "lm", se = FALSE))
  output$location <- renderText(paste0("x=", input$onplotclick$x, "\ny=", input$onplotclick$y))

  oaxacamodel <- oaxaca()
}

shinyApp(ui = ui, server = server)
library(oaxaca)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(plotly)
library(htmlwidgets)
library(data.table)
library(shinydashboard)
#library(jsonlite)

RegData <- read.csv("C:\\Users\\ambatipudir\\Downloads\\data_r.csv", header = TRUE, stringsAsFactors = TRUE)
#RegData <- fromJSON("C:\\Users\\ambatipudir\\")
RegData$totalPay <- RegData$basePay + RegData$bonus
RegData$male <- ifelse(RegData$gender == "Male", 1, 0) # Male = 1, Female = 0.

if(interactive()) {
  
  ui <- fluidPage(
    h1("Compensation Analysis using Oaxaca Decomposition"), 
    sidebarPanel(
      h4("Select the input parameters on which the compensation must depend"),
      pickerInput(inputId = "DependentVariable", label = "Dependent Variables", multiple = TRUE, choices = c('perfEval', 
                                                                                                             'age',                     
                                                                                                             'seniority'), options = list(virtualScroll=2, enable_search = TRUE)),
      h4("Select the inputs paramter for finding which of the compensation"),
      pickerInput(inputId = "IndependentVariable", label = "Independent Variables", multiple = FALSE, choices = c('basePay', 'totalPay', 'bonus'))
      
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      #verbatimTextOutput(outputId = "RegSum"),
      p("R Squared Value"),
      verbatimTextOutput(outputId = "RSquared"),
      p("Adjusted R Squared Value"),
      verbatimTextOutput(outputId = "AdjustedRSquared"),
      p("F Statistic Value"),
      verbatimTextOutput(outputId = "FStatistic"),
      p("P Value"),
      verbatimTextOutput(outputId = "PValue"),
      p("Standard Deviation"),
      verbatimTextOutput(outputId = "StdDev"),
      verbatimTextOutput(outputId = "DepPrint"),
      verbatimTextOutput(outputId = "IndPrint"),
      verbatimTextOutput(outputId = 'location'),
      verbatimTextOutput(outputId = "clickData"),
      plotlyOutput("plot"),
      verbatimTextOutput(outputId = "average")
    )
  )
  
  server <- function(input, output, session) {
    
    lm1 <- reactive({lm(reformulate(input$DependentVariable, input$IndependentVariable), data = RegData)})
    
    output$DepPrint <- renderPrint({input$IndependentVariable})
    output$IndPrint <- renderPrint({input$IndependentVariable})
    #output$RegSum <- renderPrint({summary(lm1())})
    output$RSquared <- renderPrint({(summary(lm1()))$r.squared})
    output$AdjustedRSquared <- renderPrint({(summary(lm1()))$adj.r.squared})
    output$FStatistic <- renderPrint({(summary(lm1()))$fstatistic})
    output$PValue <- renderPrint(pf((summary(lm1()))$fstatistic[1],
                                    (summary(lm1()))$fstatistic[2],
                                    (summary(lm1()))$fstatistic[3],lower.tail=F))
    output$StdDev <- renderPrint(summary(lm1())$sigma)
    
    output$average <- renderText({
      average <- mean(RegData[[input$DependentVariable]])
      average
    })
    
    output$plot <- renderPlotly({
        p <- plot_ly(data = RegData, x = input$DependentVariable, y = input$IndependentVariable, type = "scatter") %>% 
        add_lines(x = input$DependentVariable, y = input$IndependentVariable) %>% add_markers(color = RegData$gender)
    })
    
    output$location <- renderText(paste0("x=", input$onplotclick$x, "\ny=", input$onplotclick$y))
    currentSalary <- reactiveVal(value = NA)
    observeEvent(input$onplotclick,
                 {
                   nearestSalary <- nearPoints(RegData, input$onplotclick, maxpoints = 1)
                   currentSalary(as.character(nearestSalary))
                 }
    )
    output$clickData <- renderText(paste0("currentSalary=", currentSalary()[10]))
    
  }
  
  shinyApp(ui = ui, server = server)
  
}
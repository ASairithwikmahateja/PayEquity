library(oaxaca)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(broom)
library(dplyr)
library(htmlwidgets)
#library(jsonlite)
library(data.table)
library(shinyBS)
library(reshape2)
library(rsconnect)

RegData <- read.csv("C:\\Users\\ambatipudir\\Downloads\\data_r.csv", header = TRUE, stringsAsFactors = TRUE)
#RegData <- fromJSON
RegData$totalPay <- RegData$basePay + RegData$bonus
RegData$male <- ifelse(RegData$gender == "Male", 1, 0) # Male = 1, Female = 0.

RegData$dept <- ifelse((RegData$dept) == "Operations", 1,
                  ifelse((RegData$dept) == "Management", 2, 
                    ifelse((RegData$dept) == "Administration", 3,
                      ifelse((RegData$dept) == "Sales", 4, 
                        ifelse((RegData$dept) == "Engineering", 5, NULL)))))

if(interactive()) {
  
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Pay Equity Analysis", titleWidth = 250), 
  dashboardSidebar(width = 250,
    h4("Select the input parameters on which the compensation must depend"),
    pickerInput(inputId = "DependentVariable", label = "Dependent Variables", multiple = TRUE, choices = c('perfEval', 
                                                                                                 'age',                     
                                                                                                 'seniority',
                                                                                                 'edu', 'dept'), options = list(virtualScroll=2, enable_search = TRUE)),
    
    h4("Select the inputs paramter for finding which of the compensation"),
    pickerInput(inputId = "IndependentVariable", label = "Independent Variables", multiple = FALSE, choices = c('basePay', 'totalPay', 'bonus'), selected = FALSE),
    
    numericInput(inputId = "QuantileRange", label = "Quantile Range", min = 0, max = 1, value = 0, step = 0.01),
    bsTooltip(id = "QuantileRange", title = "Enter values with steps increment 0.01 for each percentile", 
              placement = "bottom", trigger = "hover"),
    radioButtons(inputId = "Comparingtwoclasses", label = "Compare between the two classes", choices = c("Don't Compare", "Compare")),
    
    h4("Choose the type of distribution plot "),
    pickerInput(inputId = "plot_type", label = "Distribution Plot type", multiple = FALSE, choices = c('stack', 'fill', 'dodge'), selected = FALSE)
    
    ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 14px;
      
    }'
    ))),
    tags$body(tags$style(HTML('
    .skin-blue .main-sidebar {
      background-color: #DAD000;
    }'))),
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      downloadButton(outputId = 'download.CSV', label = 'Download', icon = shiny::icon("download"))
    ),
    bsTooltip(id = "rule_applied", title = "Rule Applied appears here", 
              placement = "left", trigger = "hover"),
    h6("Rule Applied"),
    verbatimTextOutput(outputId = "rule_applied"),
    #verbatimTextOutput(outputId = "RegSum"),
    bsTooltip(id = "RSquared", title = "R Squared appears here", 
              placement = "left", trigger = "hover"),
    h6("R Squared Value"),
    verbatimTextOutput(outputId = "RSquared"),
    bsTooltip(id = "AdjustedRSquared", title = "Adjusted R Squared appears here", 
              placement = "left", trigger = "hover"),
    h6("Adjusted R Squared Value"),
    verbatimTextOutput(outputId = "AdjustedRSquared"),
    bsTooltip(id = "FStatistic", title = "F Statistic appears here", 
              placement = "left", trigger = "hover"),
    h6("F Statistic Value"),
    verbatimTextOutput(outputId = "FStatistic"),
    bsTooltip(id = "PValue", title = "P Value appears here", 
              placement = "left", trigger = "hover"),
    h6("P Value"),
    verbatimTextOutput(outputId = "PValue"),
    bsTooltip(id = "StdDev", title = "Standard Deviation appears here", 
              placement = "left", trigger = "hover"),
    h6("Standard Deviation"),
    verbatimTextOutput(outputId = "StdDev"),
    bsTooltip(id = "quantilePayrange", title = "Top Pay percentile appears here", 
              placement = "left", trigger = "hover"),
    
    verbatimTextOutput(outputId = "average"),
    box(DT::dataTableOutput(outputId = "groupby_gender_table"), width = 1000),
    plotlyOutput("group_by_gender_table_plot"),
    h6("Pay range quantile"),
    verbatimTextOutput(outputId = "quantilePayrange"),
    verbatimTextOutput(outputId = "DepPrint"),
    verbatimTextOutput(outputId = "concat_dependent"),
    verbatimTextOutput(outputId = "IndPrint"),
    verbatimTextOutput(outputId = 'location'),
    verbatimTextOutput(outputId = "clickData"),
    verbatimTextOutput(outputId = "reactive_oaxacamodel"),
    plotlyOutput("plot", width = 1000, height = 700)
  )
)

server <- function(input, output, session) {
  
  lm1 <- reactive({lm(reformulate(input$DependentVariable, input$IndependentVariable), data = RegData)})
  
  output$DepPrint <- renderPrint({input$DependentVariable})
  output$IndPrint <- renderPrint({input$IndependentVariable})
  #output$RegSum <- renderPrint({summary(lm1())})
  output$RSquared <- renderPrint({(summary(lm1()))$r.squared})
  output$AdjustedRSquared <- renderPrint({(summary(lm1()))$adj.r.squared})
  output$FStatistic <- renderPrint({(summary(lm1()))$fstatistic[1]})
  output$PValue <- renderPrint(pf((summary(lm1()))$fstatistic[1],
                                  (summary(lm1()))$fstatistic[2],
                                  (summary(lm1()))$fstatistic[3],lower.tail=F))
  output$StdDev <- renderPrint(summary(lm1())$sigma)
  
  output$quantilePayrange <- renderPrint({quantile(RegData[[input$IndependentVariable]], probs = input$QuantileRange)})
  
  output$average <- renderPrint({
    if(input$IndependentVariable == "NULL") {
      average <- as.numeric(0)
    } else {
      average <- colMeans(RegData[cat(deparse(input$DependentVariable), sep = '\n')])
      #for (i in cat(deparse(input$DependentVariable), sep = '\n')) { i }])
      average
    }
  })
  
  output$concat_dependent <- renderPrint({ cat(deparse(input$DependentVariable), sep = '\n') })
  
  output$rule_applied <- renderPrint({ 
    paste("Predicting", input$IndependentVariable, "on above selected variables", cat(deparse(input$DependentVariable), sep = '\n'))
  })
  
  output$plot <- renderPlotly({
    p <- ggplotly(
      ggplot(data = RegData, aes_string(x = input$DependentVariable, y = input$IndependentVariable, col = RegData$gender)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
    ) 
    if(input$Comparingtwoclasses == "Compare") {
      p %>% layout(hovermode = "compare")
    } else {
      p
    }
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
  
  output$download.CSV <- downloadHandler(
    filename = function() {
      paste(input$IndependentVariable, "_", paste(cat(deparse(input$DependentVariable), sep = '\n')), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tidy(lm1()), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$DependentVariable, {
    if (is.null(input$DependentVariable))
      shinyjs::hide("download.CSV")
    else if (!is.null(input$DependentVariable)) 
      shinyjs::show("download.CSV")
  })
  
  output$groupby_gender_table <- DT::renderDataTable({ 
    summarise_salary <- 
      RegData %>% 
      group_by(Gender = RegData$gender) %>% 
      summarise(Count = n(), 
        Grouped_Min_Salaries = round(min(!!sym(input$IndependentVariable)), digits = 2),
        Grouped_Mean_Salaries = round(mean(!!sym(input$IndependentVariable)), digits = 2),
        Grouped_Median_Salaries = round(median(!!sym(input$IndependentVariable)), digits = 2),
        Grouped_Max_Salaries = round(max(!!sym(input$IndependentVariable)), digits = 2),
      )
    
    DT::datatable(data = summarise_salary)
  })
 
  output$group_by_gender_table_plot <- renderPlotly({
    g <- ggplotly(
      ggplot(data = melt(RegData %>% group_by(gender) %>% summarise(min(!!sym(input$IndependentVariable)),
                    mean(!!sym(input$IndependentVariable)), median(!!sym(input$IndependentVariable)),
                    max(!!sym(input$IndependentVariable))), id = 'gender'), 
             aes(x = gender, y = value, fill = variable, width = 0.6)) + xlab('Gender') + ylab(paste(input$IndependentVariable)) + geom_bar(position = input$plot_type, stat = 'identity', color = 'black') + ggtitle("Distribution Chart Male Vs. Female")
    )
    g
  })
  
  oaxaca_react_model <- reactive({
    oaxaca(input$IndependentVariable ~ input$DependentVariable | RegData$male, data = RegData) 
  })
  
  output$reactive_oaxacamodel <- renderText({
    paste("The mean for Male population is", oaxaca_react_model()$y$y.A)
    #paste("The mean for Female population is", model$y$y.B)
    #paste("The difference between population groups is", model$y$y.diff)
  })
  
  output$oaxaca_plot <- renderPlot({
    p <- plot(oaxaca(input$IndependentVariable ~ input$DependentVariable | RegData$male, data = RegData), 
                  decomposition = 'twofold')
    p
  })
}

shinyApp(ui = ui, server = server)

}

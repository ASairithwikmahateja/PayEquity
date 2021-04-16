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
  
  output$reactive_oaxacamodel <- renderPrint({
    paste("The mean for Male population is", oaxaca_react_model())
    #paste("The mean for Female population is", model$y$y.B)
    #paste("The difference between population groups is", model$y$y.diff)
  })
  
  output$oaxaca_plot <- renderPlot({
    p <- plot(oaxaca(input$IndependentVariable ~ input$DependentVariable | RegData$male, data = RegData), 
              decomposition = 'twofold')
    p
  })
}
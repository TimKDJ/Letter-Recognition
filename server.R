library(shiny)
source('neural_network_engine.R')
source('server_helper_functions.R')

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  values$show <- 'no'
  
  prediction <- reactive({
    return(values$show)
  })
  
  observeEvent(input$continue, {
    print('cont')
    values$show <- 'yes'
  })
  observeEvent(input$reset, {
    print('reset')
    values$show <- 'no'
  })
  
  coordinates <- reactive({input$coordinates})
  letter <- reactive({input$letter})
  observeEvent(coordinates(), {
    if (!is.null(coordinates())) {
      input <- ParseJSString(coordinates())
      #SaveSample(letter(), input)
      #isolate({input$coordinates <- NeuralNetwork(input, 'run', TRUE)})
      #print(NeuralNetwork(input, 'run', TRUE))
    }
  })
  output$pad <- renderUI(HTML(GenerateTable()))
  output$view <- renderText({ 
    if (!is.null(coordinates())) {
      input <- ParseJSString(coordinates())
      nn <- NeuralNetwork(input, 'run', TRUE)
      pred <- LETTERS[which.max(nn)]
      paste('Prediction:',pred, '(certainty is ', round(max(nn) * 100, 2), '%)')
    }
  })
  #paste('saving letter:',letter(), coordinates()) })
})

#roxygen2 for auto documentation
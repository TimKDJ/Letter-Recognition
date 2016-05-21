source('neural_network_engine.R')
source('server_helper_functions.R')
library(shiny)

shinyServer(function(input, output, session) {
  coordinates <- reactive({
    input$coordinates
  })
  letter <- reactive({
    input$letter
  })
  output$pad <- renderUI({
    HTML(GenerateTable())
  })
  observeEvent(coordinates(), {
    if (!is.null(coordinates())) {
      input <- ParseJSString(coordinates())
      #SaveSample(letter(), input)
    }
  })
  output$prediction <- renderUI({ 
    if (!is.null(coordinates())) {
      input <- ParseJSString(coordinates())
      nn <- NeuralNetwork(input, 'run', TRUE)
      HTML(paste0(
        '<h5>Prediction:</h5>
          <span id ="output">', nn[1], '</span><br>
          <span id="certainty">Certainty: ', nn[2], '%</span>'
      ))
    }
  })
  outputOptions(output, 'prediction', suspendWhenHidden=FALSE)
  output$letter <- renderText({
    letter()
  })
})
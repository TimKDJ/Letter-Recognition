library(shiny)
source('neural_network_engine.R')
source('server_helper_functions.R')

shinyServer(function(input, output, session) {
  coordinates <- reactive({input$coordinates})
  observeEvent(coordinates(), {
    if (!is.null(coordinates())) {
      #letter <- 'b'
      input <- ParseJSString(coordinates())
      #write.table(matrix(c(letter, input), 1, 101), 'samples.csv', row.names=FALSE, col.names=FALSE, append=TRUE)
      print(NeuralNetwork(input, 'run'))
    }
  })
  output$pad <- renderUI(HTML(GenerateTable(kPadRows, kPadCols)))
  output$view <- renderText({ coordinates() })
})
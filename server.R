#############################################################################################################################
# Server that handles user input from the interface.                                                                        #
# To be used in conjunction with the Shiny R file ui.R as well as server_helper_function.R and neural_network_engine.R.     #
# Written by Tim de Jong for the course 'Programming: the next step' - date 25-05-2016.                                     #                                      
#############################################################################################################################


# load required scripts
source('neural_network_engine.R')
source('server_helper_functions.R')
library(shiny)


shinyServer(function(input, output, session) {
  coordinates <- reactive({  # observe the js coordinates object
    input$coordinates
  })
  output$pad <- renderUI({  # build the drawing pad
    HTML(GenerateTable())
  })
  # Uncomment the following lines only when samples need to be created, also uncomment line in ui.R
  #letter <- reactive({  # observe the letter input
  #  input$letter
  #})
  #observeEvent(coordinates(), {  # observe the js coordinates object
  #  if (!is.null(coordinates())) {
  #    input <- ParseJSString(coordinates())  # change the input from rows and cols to a sample vector
  #    SaveSample(letter(), input)  # write sample to CSV
  #  }
  #})
  output$prediction <- renderUI({  # show the prediction
    if (!is.null(coordinates())) {
      input <- ParseJSString(coordinates())  # change the input from rows and cols to a sample vector
      if (length(input) == 0) {  # prevent empty firing if a user overrides JavaScript
        return()
      }
      nn <- ExecNeuralNetwork(input)  # obtain a prediction
      HTML(paste0(  # build the HTML for the prediction box
        '<h5>Prediction:</h5>
          <span id ="primeOutput">', nn[1], '</span><br>
          <span id="certainty">Certainty is ', nn[4], '%, alternatives:</span><br>
          <span id ="alterOutput">', nn[2], '</span><br>
          <span id ="alterOutput">', nn[3], '</span><br>'
        
      ))
    }
  })
  outputOptions(output, 'prediction', suspendWhenHidden=FALSE)  # by default Shiny does not observe hidden elements
})
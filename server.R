library(shiny)
source('neural_network_engine.R')
shinyServer(function(input, output, session) {
  #observe({
  #  input(input$coordinates)
  #  })
  # prints table with selectable cells
  coordinates <- reactive({input$coordinates})
  observeEvent(coordinates(), {
    if (!is.null(coordinates())) {
      parseInput(coordinates())
    }
  })
  
  output$pad <- renderUI(HTML(GenerateTable(kPadRows, kPadCols)))
  output$view <- renderText({ coordinates() }) 
  #output$pad <- renderUI({HTML(df2html())})
  
  # prints row and column numbers of selected cell. 
  # -1 when nothing selected. NULL when table is not generated
  #observe({print(paste0("Table 3: ", ifelse(is.null(input$pad), "NULL", 
  #     paste0(input$pad[1], ',',input$pad[2]))))})
})

GenerateTable <- function(rows, cols) {
  table <- '<table class="drawingPad">'
  for (i in 1:rows) { 
    table <- paste0(table, '<tr>')
    for (i in 1:cols) { 
      table <- paste0(table, '<td class="cell"></td>')
    } 
    table <- paste0(table, '</tr>')
  }
  table <- paste0(table, '</table')
  return(table)
}
library(shiny)
shinyUI(fluidPage(
  headerPanel(
    tags$head(tags$link(rel='stylesheet', type='text/css', href='css/style.css'))
  ),
  fluidRow(
    column(2),
    column(2, align='center',
           img(src='img/draw.jpg'),
           img(src='img/nn.jpg'),
           img(src='img/letters.jpg')
    ),
    column(4, align='center',
           titlePanel(
             'Neural Network for Letter Recognition'
           ),
           fluidRow(
             column(9,
                    uiOutput('pad')
             ),
             column(3,
                    textInput('letter', 'Letter'),
                    actionButton('continue', 'Continue'),
                    actionButton('reset', 'Clear'),
                    conditionalPanel('input.reset == 0', verbatimTextOutput('view')),
                    includeScript('www/js/drawingPad.js')
             )
           )
    ),
    column(4)
  )
))
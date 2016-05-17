library(shiny)
shinyUI(pageWithSidebar(
  headerPanel(
    'Letter Recognition',
    tags$head(tags$link(rel='stylesheet', type='text/css', href='css/style.css'))
    ),
  sidebarPanel(
    uiOutput('pad')
  ),
  mainPanel(
    actionButton('continue', 'Continue'),
    actionButton('reset', 'Clear'),
    textOutput('view'),
    includeScript('www/js/drawingPad.js')
  )
))
library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Letter Recognition"),
  sidebarPanel(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/style.css"),
      tags$script(type = 'text/javascript', src = 'js/drawingPad.js')
    ), 
    actionButton("pencil", label="Next")
  ),
  mainPanel(
    uiOutput('pad'),
    textOutput("view")
  )
))
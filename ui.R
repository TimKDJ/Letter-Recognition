shinyUI(fixedPage(
  title='Letter Recognition',
  tags$head(tags$link(rel='stylesheet', type='text/css', href='css/style.css')),
  tags$head(tags$link(rel='icon', href='img/favicon.ico')),
  fixedRow(
    column(12,
           img(src='img/draw_rs.jpg', class='header'),
           img(src='img/nn_rs.jpg', class='header'),
           img(src='img/letters_rs.jpg', class='header')
    )
  ),
  fixedRow(
    column(12, 
           img(src='img/arrows.png', id='arrow_left'),
           img(src='img/arrows.png', id='arrow_right'),
           h2('Neural Network for Capital Letter Recognition')
    )
  ),
  fixedRow(
    column(1),
    column(6, align='center',
           uiOutput('pad')
    ),
    column(4,
           textInput('letter', 'Letter'),
           verbatimTextOutput('letter'),
           actionButton('continue', 'Continue'),
           br(),
           actionButton('reset', 'Clear'),
           uiOutput('prediction')
    ),
    column(1)
  ),
  includeScript('www/js/drawingPad.js')
))
#############################################################################################################################
# User interface.                                                                                                           #
# To be used in conjunction with the Shiny R file server.R as well as server_helper_function.R and neural_network_engine.R. #
# Written by Tim de Jong for the course 'Programming: the next step' - date 25-05-2016.                                     #                                                                                   
#############################################################################################################################


shinyUI(fixedPage(  # prevent page from collapsing on smaller screens
  title='Letter Recognition',  # tab title
  tags$head(tags$link(rel='stylesheet', type='text/css', href='css/style.css')),  # css
  tags$head(tags$link(rel='icon', href='img/favicon.ico')),  # favicon
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
           h2('Neural Network for Capital Letter Recognition')  # title
    )
  ),
  fixedRow(
    column(1),
    column(6, align='center',
           uiOutput('pad')  # pad for drawing
    ),
    column(4,
           # Uncomment the following line only when samples need to be created, also uncomment lines in server.R
           #textInput('letter', 'Letter'),  # letter input for sample creating
           actionButton('continue', 'Continue'),  # next button
           br(),
           actionButton('reset', 'Clear'),  # reset button
           uiOutput('prediction')  # hidden prediction
    ),
    column(1)
  ),
  includeScript('www/js/interface.js')  # JavaScript needs to be inline to prevent overwriting of event handlers
))
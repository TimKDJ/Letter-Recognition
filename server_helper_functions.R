#############################################################################################################################
# Functions that facilitate the server.                                                                                     #
# To be used in conjunction with the Shiny R files ui.R and server.R as well as neural_network_engine.R.                    #
# Written by Tim de Jong for the course 'Programming: the next step' - date 25-05-2016.                                     #                                      #
#############################################################################################################################


source('neural_network_engine.R')


GenerateTable <- function() {
  #' Build a HTML table with the dimensions kPadRows by kPadCols.
  #'
  #' @return A string containing HTML for the drawing table.
  table <- '<table>'
  for (i in 1:kPadRows) { 
    table <- paste0(table, '<tr>')
    for (i in 1:kPadCols) { 
      table <- paste0(table, '<td class="unselected"></td>')
    } 
    table <- paste0(table, '</tr>')
  }
  table <- paste0(table, '</table')
  return(table)
}


ParseJSString <- function(x) {
  #' Transform a JavaScript vector containing row and col numbers to a network input vector.
  #'
  #' @param x A character vector with elements containing rows and columns separated by a + (e.g. 5+6).
  #' @return A vector of size kPadRows * kPadCols ready to be parsed by the network.
  if (length(grep('+', x)) != length(x)) {  # validate input vector
    warning('Invalid vector, not all elements contain the necessary delimiter (+)')
    return()
  }
  result <- numeric(kPadRows * kPadCols)
  inputs <- numeric(0)
  for (i in 1:length(x)) {  # iterate over the vector
    # Each element consists of coordinates of their place on the pad. These coordinates need to be transformed to a valid vector index.
    row <- as.numeric(unlist(strsplit(x[i], '[+]'))[1]) + 1  # take the element before the separator and add 1 (js index starts with 0)
    col <- as.numeric(unlist(strsplit(x[i], '[+]'))[2]) * kPadRows  # multiply the other element with the number of rows in the pad
    inputs <- c(inputs, row + col)  # add the two together
  }
  if (sum(!inputs %in% 0:(kPadRows * kPadCols)) > 0) {  # ensure that the inputs are in the valid range
    warning('Invalid row/column number(s) in input')
    return()
  }
  result[inputs] <- 1  # each vector index taken from the pad needs to be set to 1
  return(result)
}


SaveSample <- function(letter, input) {
  #' Save a training sample to a space separated CSV file.
  #'
  #' @param letter A character describing what the sample is (e.g. 't').
  #' @param input A vector of length kPadCols * kPadRows.
  if (!letter %in% letters || length(input) != kPadRows * kPadCols) {  # validate input
    stop('Vector needs a descriptive letter at position 1 followed by ', kPadRows * kPadCols, ' inputs')
  }
  sample <- matrix(c(letter, input), 1, length(input) + 1)  # transform to a 1 by x matrix
  write.table(sample, 'samples.csv', row.names=FALSE, col.names=FALSE, append=TRUE)  # append if file exists
}
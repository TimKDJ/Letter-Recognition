source('neural_network_engine.R')

GenerateTable <- function() {
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
  if (length(grep('+', x)) != length(x)) {
    warning('Invalid vector, not all elements contain the necessary delimiter (+)')
    return()
  }
  result <- numeric(kPadRows * kPadCols)
  inputs <- numeric(0)
  for (i in 1:length(x)) {
    row <- as.numeric(unlist(strsplit(x[i], '[+]'))[1]) + 1
    col <- as.numeric(unlist(strsplit(x[i], '[+]'))[2]) * kPadRows
    inputs <- c(inputs, row + col)
  }
  if (sum(!inputs %in% 0:(kPadRows * kPadCols)) > 0) {
    warning('Invalid row/column number(s) in input')
    return()
  }
  result[inputs] <- 1
  return(result)
}

SaveSample <- function(letter, input) {
  if (!letter %in% letters || length(input) != kPadRows * kPadCols) {
    stop('Vector needs a descriptive letter at position 1 followed by ', kPadRows * kPadCols, ' inputs')
  }
  sample <- matrix(c(letter, input), 1, length(input) + 1)
  write.table(sample, 'samples.csv', row.names=FALSE, col.names=FALSE, append=TRUE)
}
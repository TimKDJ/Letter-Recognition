source('neural_network_engine.R')

GenerateTable <- function(rows, cols) {
  table <- '<table>'
  for (i in 1:rows) { 
    table <- paste0(table, '<tr>')
    for (i in 1:cols) { 
      table <- paste0(table, '<td class="unselected"></td>')
    } 
    table <- paste0(table, '</tr>')
  }
  table <- paste0(table, '</table')
  return(table)
}

ParseJSString <- function(x) {
  result <- numeric(kNeuronCount[1])
  inputs <- numeric(0)
  for (i in 1:length(x)) {
    row <- as.numeric(substring(x[i],1,1)) + 1
    col <- as.numeric(substring(x[i],3,3)) * kPadRows
    inputs <- append(inputs, row + col)
  }
  result[inputs] <- 1
  return(result)
}

ParseCSVSamples <- function() {
  data <- read.delim('samples.csv', header=FALSE, sep=' ')
  samples <- matrix(list(), nrow(data), 2)
  for (i in 1:nrow(samples)) {
    letter <- as.character(data[i, 1])
    samples[[i, 1]] <- letter
    input <- unlist(data[i, 2:101])
    samples[[i, 2]] <- input
  }
  return(samples)
}

#input <- ParseCSVSamples()
#NeuralNetwork(input[[48, 2]])
#NeuralNetwork(input, TRUE, TRUE, TRUE)


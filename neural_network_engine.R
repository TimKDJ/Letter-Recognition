kNeuronCount <- c(25, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerCount <- 4
kBiasBound <- 0.1
kWeightBound <- 0.1
kIterations <- 500
kLearningRate <- 1

B <- vector('list', kLayerCount)
W <- vector('list', kLayerCount)

TrainNeuralNetwork <- function(input = FALSE, log = FALSE) {
  input <- c(0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
  output <- GetCorrectOutput('b')
  InitWeightsBiases()
  for (i in 1:kIterations) {
    layerValues <- ForwardPropogation(input)
    BackwardPropogation(output, layerValues, i, log)
  }
}

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function() {
  for (i in 2:kLayerCount) {
    B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
    W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
  }
}

#feedforward through network from input to output
ForwardPropogation <- function(input) {
  layers <- matrix(list(), nrow=kLayerCount, ncol=2)
  colnames(layers) <- c('input', 'output')
  layers[[1, 'output']] <- input
  for (i in 2:kLayerCount) {
    layers[[i, 'input']] <- CalculateLayerInput(W[[i]], B[[i]], layers[[i - 1, 'output']])
    layers[[i, 'output']] <- CalculateLayerOutput(layers[[i, 'input']], kLayerType[i])
  }
  return(layers)
}

CalculateLayerInput <- function(weights, bias, inputs, layer) {
  totalInput <- colSums(inputs * weights) + bias
  return(totalInput)
}

CalculateLayerOutput <- function(input, layer) {
  if (layer == 'hidden') {
    output <- SigmoidFunction(input)
  } else {
    output <- SoftmaxFunction(input)
  }
  return(output)
}

SigmoidFunction <- function(x, deriv = FALSE) {
  result <- 1 / (1 + exp(-x))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}

SoftmaxFunction <- function(x, deriv = FALSE) {
  result <- exp(x) / sum(exp(x))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}

GetCorrectOutput <- function(letter) {
  output <- as.numeric(letters == letter)
  return(output)
}


BackwardPropogation <- function(expectedOutput, layers, iteration, log = FALSE) {
  delta <- vector('list', kLayerCount)
  error <- sum((layers[[kLayerCount, 'output']] - expectedOutput)^2)
  if (log == TRUE) {
    cat('Iteration:', iteration, '| error:', error, '\n')
  }
  delta[[kLayerCount]] <- error * SoftmaxFunction(layers[[kLayerCount, 'input']], deriv = TRUE)
  #  for (i in (kLayerCount - 1):2) {
  #    delta[i] <- delta[[i + 1]] * SigmoidFunction(output[[i]], deriv = TRUE)
  #  }
  W[[kLayerCount]] <<- W[[kLayerCount]] - kLearningRate * delta[[kLayerCount]] * layers[[kLayerCount, 'input']]
  B[[kLayerCount]] <<- B[[kLayerCount]] - kLearningRate * delta[[kLayerCount]]
}
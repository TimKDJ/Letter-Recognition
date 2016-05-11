kNeuronCount <- c(25, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerTotal <- 4
kBiasBound <- 0.5
kWeightBound <- 0.5
kIterations <- 800
kLearningRate <- 0.1

B <- vector('list', kLayerTotal)
W <- vector('list', kLayerTotal)

TrainNeuralNetwork <- function(input = FALSE, log = FALSE) {
  input <- c(0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
  input <- c(1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
  input <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1)
  input <- c(1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  target <- GetCorrectOutput('b')
  weights <- InitWeightsBiases()
  for (i in 1:kIterations) {
    layerValues <- ForwardPropogation(input)
    BackwardPropogation(target, layerValues, i, log)
  }
}

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function() {
  for (i in 2:kLayerTotal) {
    B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
    W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
  }
}

#feedforward through network from input to output
ForwardPropogation <- function(input) {
  layers <- matrix(list(), nrow=kLayerTotal, ncol=2)
  colnames(layers) <- c('input', 'output')
  layers[[1, 'output']] <- input
  for (i in 2:kLayerTotal) {
    layers[[i, 'input']] <- CalculateLayerInput(W[[i]], B[[i]], layers[[i - 1, 'output']])
    layers[[i, 'output']] <- CalculateLayerOutput(layers[[i, 'input']], kLayerType[i])
  }
  return(layers)
}

CalculateLayerInput <- function(weights, bias, inputs) {
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

BackwardPropogation <- function(target, layers, iteration, log = FALSE) {
  delta <- vector('list', kLayerTotal)
  totalError <- sum(1/2 * (target - layers[[kLayerTotal, 'output']])^2)
  derivError <- layers[[kLayerTotal, 'output']] - target
  if (log == TRUE) {
    cat('Iteration:', iteration, '| error:', totalError, '\n')
  }
  delta[[kLayerTotal]] <- derivError * SoftmaxFunction(layers[[kLayerTotal, 'input']], deriv = TRUE)
  for (i in (kLayerTotal - 1):2) {
    delta[[i]] <- rowSums(t(delta[[i + 1]] * t(W[[i + 1]]))) * SigmoidFunction(layers[[i, 'input']], deriv = TRUE)
  }
  for (i in 2:4) {
    W[[i]] <<- W[[i]] - kLearningRate * t(t(layers[[i - 1, 'output']])) %*% t(delta[[i]])
    B[[i]] <<- B[[i]] - kLearningRate * delta[[i]]
  }
}
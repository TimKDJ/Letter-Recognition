kNeuronCount <- c(25, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerCount <- 4
kBiasBound <- 0.1
kWeightBound <- 0.1
kLearningRate <- 0.01

B <- vector('list', kLayerCount)
W <- vector('list', kLayerCount)

TrainNeuralNetwork <- function(input = FALSE) {
  input <- c(0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
  InitWeightsBiases()
  output <- ForwardPropogation(input)
  BackwardPropogation('b', output)
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
  layerOutput <- vector('list', kLayerCount)
  layerOutput[[1]] <- input
  for (i in 2:kLayerCount) {
    layerOutput[[i]] <- CalculateLayerOutput(W[[i]], B[[i]], layerOutput[[i - 1]], kLayerType[i])
  }
  return(layerOutput)
}

CalculateLayerOutput <- function(weights, bias, inputs, layer) {
  totalInput <- colSums(inputs * weights) + bias
  if (layer == 'hidden') {
    output <- ActivationFunction(totalInput)
  } else {
    output <- SoftmaxFunction(totalInput)
  }
  return(output)
}

ActivationFunction <- function(x, deriv = FALSE) {
  result <- 1 / (1 + exp(-x))
  return(result)
}

SoftmaxFunction <- function(x) {
  result <- exp(x) / sum(exp(x))
  return(result)
}

GetCorrectOutput <- function(letter) {
  output <- as.numeric(letters == letter)
  return(output)
}


BackwardPropogation <- function(letter, output) {
  delta <- vector('list', kLayerCount)
  y <- GetCorrectOutput(letter)
  error <- sum((output[[kLayerCount]] - y)^2)
  delta[[kLayerCount]] <- error * (sum(output[[kLayerCount]]) * ( 1 - output[[kLayerCount]]))
  for (i in kLayerCount:2) {
    print(i)
    delta[i - 1] <- delta[[i]] * W[[i - 1]] * (output[[i - 1]] * (1 - output[[i - 1]]))
  }
  print(delta)
}
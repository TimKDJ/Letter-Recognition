kNeuronCount <- c(25, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerCount <- 4
kBiasBound <- 0.1
kWeightBound <- 0.1
kLearningRate <- 0.01

B <- vector('list', kLayerCount)
W <- vector('list', kLayerCount)
input <- c(0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0)


TrainNeuralNetwork <- function() {
  InitWeightsBiases()
  output <- ForwardPropogation()
  print(output)
  GradientDescent()
}

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function() {
  for (i in 2:kLayerCount) {
    B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
    W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
  }
}

#feedforward through network from input to output
ForwardPropogation <- function() {
  previousLayerOutput <- input
  for (i in 2:kLayerCount) {
    previousLayerOutput <- CalculateLayerOutput(W[[i]], B[[i]], previousLayerOutput, kLayerType[i])
  }
  return(previousLayerOutput)
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


BackwardPropogation <- function() {
  
}
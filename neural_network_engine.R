kNeuronCount <- c(25, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerCount <- 4
kBiasBound <- 0.5
kWeightBound <- 0.5
kLearningRate <- 0.01

B <- vector('list', kLayerCount)
W <- vector('list', kLayerCount)
input <- sample(0:1,25,replace=TRUE)

TrainNeuralNetwork <- function() {
  InitWeightsBiases()
  output <- ForwardPropogation()
  print(output)
  GradientDescent()
}

#init weights and biases for hidden layers and the output layer (+1)
InitWeightsBiases <- function() {
  for (i in 2:kLayerCount) {
    B[[i]] <<- runif(kInputCount[i], -kBiasBound, kBiasBound)
    for (j in 1:kInputCount[i]) {
      W[[i]][[j]] <<- runif(kInputCount[i], -kWeightBound, kWeightBound)
    }
  }
}

#feedforward through network from input to output
ForwardPropogation <- function() {
  previousLayerOutput <- input
  tmp <- numeric(0);
  for (i in 2:kLayerCount) {
    for (j in 1:kInputCount[i]) {
      tmp[j] <- CalculateNeuronOutput(W[[i]][[j]], B[[i]][[j]], previousLayerOutput)
    }
    previousLayerOutput <- tmp
    tmp <- numeric(0)
  }
  return(previousLayerOutput)
}

CalculateNeuronOutput <- function(weight, bias, inputs) {
  sum <- sum(weight * inputs + bias)
  output <- reLU(sum)
  return(output)
}

reLU <- function(x, deriv = FALSE) {
  if (x < 0) {
    x <- 0;
  }
  return(x)
}

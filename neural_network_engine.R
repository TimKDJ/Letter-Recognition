kPadCols <- 10
kPadRows <- 10
kNeuronCount <- c(100, 25, 25, 26)
kInputCount <- c(0, 25, 25, 25)
kLayerType <- c('input', 'hidden', 'hidden', 'output')
kLayerTotal <- 4
kBiasBound <- 0.5
kWeightBound <- 0.1
kIterations <- 10000
kLearningRate <- 0.1

B <- vector('list', kLayerTotal)
W <- vector('list', kLayerTotal)

parseInput <- function(x) {
  result <- numeric(0)
  for (i in 1:length(x)) {
    row <- as.numeric(substring(x[i],1,1)) + 1
    col <- as.numeric(substring(x[i],3,3)) * kPadRows
    result <- append(result, row + col)
  }
  print(result)
}

TrainNeuralNetwork <- function(input = FALSE, log = FALSE) {
  InitWeightsBiases()
  error <- 0
  samples <- list()
  samples[[1]] <- c('b',0,0,0,0,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
  samples[[2]] <- c('l',1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  for (i in 1:kIterations) {
    meanError <- 0
    for (j in 1:length(samples)) {
      target <- GetCorrectOutput(samples[[j]][1])
      input <- as.numeric(samples[[j]][2:26])
      layerValues <- ForwardPropogation(input)
      BackwardPropogation(target, layerValues)
      meanError <- (meanError + CalculateError(target, layerValues)) / j
    }
    error <- ((i - 1) * error + meanError) / i
    if (log == TRUE) {
      cat('Iteration:', i, '| error:', error, '\n')
    }
  }
}

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function() {
  for (i in 2:kLayerTotal) {
    B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
    W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
  }
}

UpdateWeightsBiases <- function(delta, layers) {
  for (i in 2:kLayerTotal) {
    B[[i]] <<- B[[i]] - kLearningRate * delta[[i]]
    W[[i]] <<- W[[i]] - kLearningRate * t(t(layers[[i - 1, 'output']])) %*% t(delta[[i]])
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

BackwardPropogation <- function(target, layers) {
  delta <- vector('list', kLayerTotal)
  error <- CalculateError(target, layers, deriv=TRUE)
  for (i in kLayerTotal:2) {
    if (kLayerType[i] == 'output') {
      delta[[i]] <- error * SoftmaxFunction(layers[[i, 'input']], deriv = TRUE)
    } else {
      delta[[i]] <- rowSums(t(delta[[i + 1]] * t(W[[i + 1]]))) * SigmoidFunction(layers[[i, 'input']], deriv=TRUE)
    }
  }
  UpdateWeightsBiases(delta, layers)
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

GetCorrectOutput <- function(letter) {
  output <- as.numeric(letters == letter)
  return(output)
}

CalculateError <- function(target, layers, deriv = FALSE) {
  if (deriv == FALSE) {
    error <- sum(1/2 * (target - layers[[kLayerTotal, 'output']])^2)
  } else {
    error <- layers[[kLayerTotal, 'output']] - target
  }
  return(error)
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
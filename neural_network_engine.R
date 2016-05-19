kPadCols <- 10
kPadRows <- 10
kNeuronCount <- c(100, 63, 26)
kInputCount <- c(0, 100, 63)
kLayerType <- c('input', 'hidden', 'output')
kLayerTotal <- 3
kBiasBound <- 0.5
kWeightBound <- 0.1
kMaxEpochs <- 2000
kLearningRate <- 0.15

B <- vector('list', kLayerTotal)
W <- vector('list', kLayerTotal)

NeuralNetwork <- function(input, mode, loadWeights = FALSE, saveWeights = FALSE, log = TRUE) {
  InitWeightsBiases(loadWeights)
  if (mode == 'run') {
    layerValues <- ForwardPropogation(input)
    return(layerValues[[kLayerTotal, 'output']])
  } else if (mode == 'train') {
    c <- 1
    prevValidation <- Inf
    currValidation <- 10^10
    while (c <= kMaxEpochs && currValidation < prevValidation) {
      prevValidation <- currValidation
      trainingError <- 0
      validationError <- 0
      for (j in 1:nrow(input[['training']])) {
        sample <- input[['training']][[j, 2]]
        layerValues <- ForwardPropogation(sample)
        target <- GetCorrectOutput(input[['training']][[j, 1]])
        BackwardPropogation(target, layerValues)
        trainingError <- trainingError + CalculateError(target, layerValues, n=nrow(input[['training']]))
      }
      for (j in 1:nrow(input[['validation']])) {
        sample <- input[['validation']][[j, 2]]
        layerValues <- ForwardPropogation(sample)
        target <- GetCorrectOutput(input[['validation']][[j, 1]])
        validationError <- validationError + CalculateError(target, layerValues, n=nrow(input[['validation']]))
      }
      if (log == TRUE) {
        cat('Epoch:', c, '| training error:', trainingError, '\n')
        cat('Epoch:', c, '| validation error:', validationError, '\n')
      }
      currValidation <- validationError
      c <- c + 1
    }
    if (saveWeights == TRUE) {
      save(W, file = 'weights.RData')
      save(B, file = 'bias.RData')
    }
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

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function(load) {
  if (load == FALSE) {
    for (i in 2:kLayerTotal) {
      B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
      W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
    }
  } else {
    load('weights.RData', .GlobalEnv)
    load('bias.RData', .GlobalEnv)
  }
}

UpdateWeightsBiases <- function(delta, layers) {
  for (i in 2:kLayerTotal) {
    B[[i]] <<- B[[i]] - kLearningRate * delta[[i]]
    W[[i]] <<- W[[i]] - kLearningRate * t(t(layers[[i - 1, 'output']])) %*% t(delta[[i]])
  }
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

CalculateError <- function(target, layers, deriv = FALSE, n = FALSE) {
  if (deriv == FALSE) {
    error <- sum(1 / n * (target - layers[[kLayerTotal, 'output']])^2)
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
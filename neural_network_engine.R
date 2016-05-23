kPadCols <- 10
kPadRows <- 10
kNeuronCount <- c(100, 63, 26)
kInputCount <- c(0, 100, 63)
kLayerType <- c('input', 'hidden', 'output')
kLayerTotal <- 3
kBiasBound <- 0.1
kWeightBound <- 0.01
kMaxEpochs <- 120
kLearningRate <- 0.1

B <- vector('list', kLayerTotal)
W <- vector('list', kLayerTotal)


NeuralNetwork <- function(input, mode, loadWeights = FALSE, saveWeights = FALSE, log = TRUE) {
  InitWeightsBiases(loadWeights)
  
  if (mode == 'run') {
    layers <- ForwardPropogation(input)
    output <- GetPrediction(layers, TRUE)
    return(output)
    
  } else if (mode == 'train') {
    error <- list()
    c <- 1
    while (c <= kMaxEpochs) {
      
      error[['training']][c] <- 0
      for (j in 1:nrow(input[['training']])) {
        layers <- ForwardPropogation(input[['training']][[j, 2]])
        target <- GetCorrectOutput(input[['training']][[j, 1]])
        BackwardPropogation(target, layers)
        error[['training']][c] <- error[['training']][c] + CalculateError(target, layers, n=nrow(input[['training']]))
      }
      
      error[['validation']][c] <- 0
      for (j in 1:nrow(input[['validation']])) {
        layers <- ForwardPropogation(input[['validation']][[j, 2]])
        target <- GetCorrectOutput(input[['validation']][[j, 1]])
        error[['validation']][c] <- error[['validation']][c] + CalculateError(target, layers, n=nrow(input[['validation']]))
      }
      
      correct <- 0
      for (j in 1:nrow(input[['test']])) {
        layers <- ForwardPropogation(input[['test']][[j, 2]])
        output <- GetPrediction(layers)
        if (output[1] == toupper(input[['test']][[j, 1]])) {
          correct <- correct + 1
        }
      }
      error[['test']][c] <- round(correct / nrow(input[['test']]) * 100, 2)
      
      if (log == TRUE) {
        cat('Epoch:', c, '\n')
        cat('training error:', error[['training']][c], '\n')
        cat('validation error:', error[['validation']][c], '\n')
        cat('Test accuracy: ', error[['test']][c], '%\n\n', sep='')
      }
      
      c <- c + 1
    }
    
    if (saveWeights == TRUE) {
      save(W, file = 'weights.RData')
      save(B, file = 'bias.RData')
    }
    
    return(error)
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

GetPrediction <- function(layers, top3=FALSE) {
  output <- layers[[kLayerTotal, 'output']]
  certainty <- round(max(output) * 100, 2)
  if (top3 == FALSE) {
    letter <- LETTERS[which.max(output)]
  } else {
    letter <- LETTERS[match(sort(output, decreasing=TRUE)[1:3], output)]
    if (certainty == 100) {
      letter[2:3] <- '-'
    }
  }
  return(c(letter, certainty))
}

#init weights and biases for hidden layers and the output layer
InitWeightsBiases <- function(loadOld) {
  if (loadOld == FALSE) {
    for (i in 2:kLayerTotal) {
      B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)
      W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])
    }
  } else {
    if (!file.exists('weights.RData') || !file.exists('bias.RData')) {
      stop('Cannot locate weights.RData and bias.RData, place these files in ', getwd())
    }
    load('weights.RData', .GlobalEnv)
    load('bias.RData', .GlobalEnv)
    for (i in 2:kLayerTotal) {
      if (nrow(W[[i]]) != kInputCount[i] || ncol(W[[i]]) != kNeuronCount[i]) {
        stop('Mismatch in dimensions of weights (', nrow(W[[i]]), 'x', ncol(W[[i]]), ') and neurons/inputs (', kInputCount[i], 'x', kNeuronCount[i], '), loading failed')
      }
      if (length(B[[i]]) != kNeuronCount[i]) {
        stop('Mismatch in amount of biases (', length(B[[i]]), ') and neurons (', kNeuronCount[i], '), loading failed')
      }
      if (sum(!is.finite(W[[i]])) != 0 || sum(!is.finite(B[[i]])) != 0) {
        stop('Weights and biases may only contain numeric values, loading failed')
      }
    }
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
    error <- 1 / n * sum((target - layers[[kLayerTotal, 'output']])^2)
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
  max <- max(x) #normalize output to prevent exponent from going to Inf
  result <- exp(x - max) / sum(exp(x - max))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}
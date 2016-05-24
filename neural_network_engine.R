###############################################################################################################
# Deep neural network engine for letter recognition.                                                          #
# To be used in conjunction with the Shiny R files ui.R and server.R as well as server_helper_function.R.     #
# Written by Tim de Jong for the course 'Programming: the next step' - date 25-05-2016.                       #                                                                     #
###############################################################################################################


# Global constants. 
# These parameters may be adjusted to change the structure of the network.
# The following guidelines must be followed:
# kPadCols * kPadRows must equal kNeuronCount[1].
# kNeuronCount[3] must be 26.
# kInputCount must follow from kNeuronCount.
# The first layer must be input, the final one must be output.
kPadCols <- 10  # rows of the drawing pad
kPadRows <- 10  # columns of the drawing pad
kNeuronCount <- c(100, 63, 26)  # neurons in each layer
kInputCount <- c(0, 100, 63)  # inputs into each layer
kLayerType <- c('input', 'hidden', 'output')  # type of each layer
kLayerTotal <- 3  # total amount of layers
kBiasBound <- 0.1  # upper and lower bounds from which to randomly sample bias values
kWeightBound <- 0.01  # upper and lower bounds from which to randomly sample weight values
kMaxEpochs <- 100  # maximal number of epochs to train the network for
kLearningRate <- 0.1  # learning modifier that adjusts the gradient descent


# Global variables used internally.
B <- vector('list', kLayerTotal)  # biases
W <- vector('list', kLayerTotal)  # weights


ExecNeuralNetwork <- function(input, loadWeights = TRUE) {
#' Parse an input vector with the deep neural network to generate a prediction.
#'
#' @param input A vector of length kPadCols * kPadRows.
#' @param loadWeights Boolean, run the network with existing (TRUE) or new (FALSE) weights.
#' @return A vector with the top 3 most likely letters followed by the certainty of the first letter.
  InitWeightsBiases(loadWeights)
  layers <- ForwardPropogation(input)  # create input-output for each layer-node
  output <- GetPrediction(layers, TRUE)
  return(output)
}


TrainNeuralNetwork <- function(input, loadWeights = FALSE, saveWeights = FALSE, log = TRUE) {
#' Train the weights and biases of the neural network.
#'
#' @param input A list holding the lists 'training', 'validation', 'test' - 
#'        each holds a matrix with letters in the first and input vectors in the second column.
#' @param loadWeights Boolean, train the network with existing (TRUE) or new (FALSE) weights.
#' @param saveWeights Boolean, after training save the weights (TRUE) or discard them (FALSE).
#' @param log Boolean, print epoch details (TRUE) or do not show them (FALSE).
#' @return A list with the vectors of the training and validation errors and the accuracy of the test predictions.
  c <- 1
  epoch <- list()
  InitWeightsBiases(loadWeights)
  while (c <= kMaxEpochs) { #iterate over the sets until kMaxEpochs
    epoch[['training']][c] <- 0
    for (j in 1:nrow(input[['training']])) {  # loop through the trainingset
      layers <- ForwardPropogation(input[['training']][[j, 2]])  # create input <-> output values for each layer-node
      target <- GetCorrectOutput(input[['training']][[j, 1]])  # obtain the intended output of the final layer
      BackwardPropogation(target, layers)  # update the weights with gradient descent
      epoch[['training']][c] <- epoch[['training']][c] + CalculateError(target, layers, n = nrow(input[['training']]))
    }
    epoch[['validation']][c] <- 0
    for (j in 1:nrow(input[['validation']])) {  # loop through the validationset, no backpropogation
      layers <- ForwardPropogation(input[['validation']][[j, 2]])
      target <- GetCorrectOutput(input[['validation']][[j, 1]])
      epoch[['validation']][c] <- epoch[['validation']][c] + CalculateError(target, layers, n = nrow(input[['validation']]))
    }
    correct <- 0
    for (j in 1:nrow(input[['test']])) {  # loop through the testset, only predictions
      layers <- ForwardPropogation(input[['test']][[j, 2]])
      output <- GetPrediction(layers)
      if (output[1] == toupper(input[['test']][[j, 1]])) {  # check if the output letter corresponds with the label
        correct <- correct + 1
      }
    }
    epoch[['test']][c] <- round(correct / nrow(input[['test']]) * 100, 2)  # divide the correct predictions by the total to get percentage correct
    if (log == TRUE) {
      cat('Epoch:', c, '\n')
      cat('training error:', epoch[['training']][c], '\n')
      cat('validation error:', epoch[['validation']][c], '\n')
      cat('Test accuracy: ', epoch[['test']][c], '%\n\n', sep='')
    }
    c <- c + 1  # increment counter before starting new loop
  }
  if (saveWeights == TRUE) {
    save(W, file = 'weights.RData')
    save(B, file = 'bias.RData')
  }
  return(epoch)
}


ForwardPropogation <- function(input) {
  #' Perform forward propogation of the input through the network.
  #'
  #' @param input A vector of length kPadCols * kPadRows.
  #' @return A matrix of lists, each row represents a layer, column1 = input, column2 = output.
  layers <- matrix(list(), nrow=kLayerTotal, ncol=2)  # construct the output matrix
  colnames(layers) <- c('input', 'output')
  layers[[1, 'output']] <- input  # output of layer 1 is the same as the network input
  for (i in 2:kLayerTotal) {  # loop over the layers
    layers[[i, 'input']] <- CalculateLayerInput(W[[i]], B[[i]], layers[[i - 1, 'output']])  # calculate input
    layers[[i, 'output']] <- CalculateLayerOutput(layers[[i, 'input']], kLayerType[i])  # calculate output
  }
  return(layers)
}


BackwardPropogation <- function(target, layers) {
  #' Perform backward propogation of the error with gradient descent.
  #'
  #' @param target The intended output.
  #' @param layers A matrix of lists, each row represents a layer.
  delta <- vector('list', kLayerTotal)
  error <- CalculateError(target, layers, deriv = TRUE)
  for (i in kLayerTotal:2) {
    if (kLayerType[i] == 'output') {
      delta[[i]] <- error * SoftmaxFunction(layers[[i, 'input']], deriv = TRUE)  # delta is the derivative of the error function
    } else {
      delta[[i]] <- rowSums(t(delta[[i + 1]] * t(W[[i + 1]]))) * SigmoidFunction(layers[[i, 'input']], deriv = TRUE)  # hidden layers need the backpropogated error from higher layers
    }
  }
  UpdateWeightsBiases(delta, layers)  # adjust the weights based on the calculated gradient
}


GetPrediction <- function(layers, top3 = FALSE) {
#' Change the output vector of the final layer to a letter prediction.
#'
#' @param layers A matrix of lists, each row represents a layer.
#' @param top3 Boolean, return the best prediction (FALSE) or top 3 best predictions (TRUE).
#' @return A vector consisting of prediction(s) and the certainty of the best prediction.
  output <- layers[[kLayerTotal, 'output']]
  certainty <- round(max(output) * 100, 2)  # certainty equals the raw prediction
  if (top3 == FALSE) {
    letter <- LETTERS[which.max(output)]  # use the index of the max to get a letter
  } else {
    letter <- LETTERS[match(sort(output, decreasing=TRUE)[1:3], output)]  # sort the output, use the first 3 indices to get letters
    if (certainty == 100) {  # if certainty is 100%, do not show other letters
      letter[2:3] <- '-'
    }
  }
  return(c(letter, certainty))
}


InitWeightsBiases <- function(loadOld) {
  #' Initialize weights and biases from scratch or load saved ones.
  #'
  #' @param loadOld Boolean, load existing weights and biases (TRUE) or initialize new ones (FALSE).
  if (loadOld == FALSE) {
    for (i in 2:kLayerTotal) { #loop over the layers
      B[[i]] <<- runif(kNeuronCount[i], -kBiasBound, kBiasBound)  # fill each bias vector with random bounded numbers
      W[[i]] <<- matrix(runif(kInputCount[i] * kNeuronCount[i], -kWeightBound, kWeightBound), kInputCount[i], kNeuronCount[i])  # fill each weight matrix
    }
  } else {  # load old weights
    if (!file.exists('weights.RData') || !file.exists('bias.RData')) {  # ensure files are present
      stop('Cannot locate weights.RData and bias.RData, place these files in ', getwd())
    }
    load('weights.RData', .GlobalEnv)  # load to the global param environment
    load('bias.RData', .GlobalEnv)
    for (i in 2:kLayerTotal) {  # perform validation on loaded weights
      if (nrow(W[[i]]) != kInputCount[i] || ncol(W[[i]]) != kNeuronCount[i]) {  # check dimensions of weights
        stop('Mismatch in dimensions of weights (', nrow(W[[i]]), 'x', ncol(W[[i]]), ') and neurons/inputs (', kInputCount[i], 'x', kNeuronCount[i], '), loading failed')
      }
      if (length(B[[i]]) != kNeuronCount[i]) {  # check dimensions of biases
        stop('Mismatch in amount of biases (', length(B[[i]]), ') and neurons (', kNeuronCount[i], '), loading failed')
      }
      if (sum(!is.finite(W[[i]])) != 0 || sum(!is.finite(B[[i]])) != 0) {  # check if the matrices and vectors are all numbers
        stop('Weights and biases may only contain numeric values, loading failed')
      }
    }
  }
}


UpdateWeightsBiases <- function(delta, layers) {
  #' Use the delta values (one per neuron) to perform a weight/bias update down the gradient.
  #'
  #' @param delta A list with vectors containing gradient values.
  #' @param layers A matrix of lists, each row represents a layer.
  for (i in 2:kLayerTotal) {
    B[[i]] <<- B[[i]] - kLearningRate * delta[[i]]
    W[[i]] <<- W[[i]] - kLearningRate * t(t(layers[[i - 1, 'output']])) %*% t(delta[[i]])  # multiply the delta values with the output of the previous layer
  }
}


CalculateLayerInput <- function(weights, bias, inputs) {
  #' Sum up all inputs to every neuron in one layer.
  #'
  #' @param weights Matrix of weight values for one layer.
  #' @param bias Vector of bias values for one layer.
  #' @param inputs Vector of input values into each neuron.
  #' @return The sum input per neuron.
  totalInput <- colSums(inputs * weights) + bias
  return(totalInput)
}


CalculateLayerOutput <- function(input, layer) {
  #' Perform an activation function on the input.
  #'
  #' @param input Vector of input values.
  #' @param layer String, type of layer (e.g. 'hidden').
  #' @return The output of the activation function.
  if (layer == 'hidden') {
    output <- SigmoidFunction(input)
  } else {  # output layer
    output <- SoftmaxFunction(input)
  }
  return(output)
}


GetCorrectOutput <- function(letter) {
  #' Get the output vector that is correct for a certain letter.
  #'
  #' @param letter Character that needs to be vectorized.
  #' @return A vector with the correct output in 0's and a 1.
  output <- as.numeric(letters == letter)  # this outputs TRUE's and FALSE's for the letters object
  return(output)
}


CalculateError <- function(target, layers, deriv = FALSE, n = FALSE) {
  #' Obtain the total error of the network or the derivative error.
  #'
  #' @param target Vector with the correct output.
  #' @param layers layers A matrix of lists, each row represents a layer.
  #' @param deriv Boolean, get the derivative error (TRUE) or the total error (FALSE).
  #' @param n Amount of samples used or FALSE.
  #' @return The sum of squares error between target and prediction.
  if (deriv == FALSE) {
    error <- 1 / (2 * n) * sum((target - layers[[kLayerTotal, 'output']])^2)  # the error is weighted per sample
  } else {  # get the error used in the derivative
    error <- layers[[kLayerTotal, 'output']] - target
  }
  return(error)
}


SigmoidFunction <- function(x, deriv = FALSE) {
  #' Activation function for the hidden layers.
  #'
  #' @param x Vector with summed inputs per neuron.
  #' @param deriv Boolean, use the derivative sigmoid (TRUE) or the normal (FALSE).
  #' @return The output vector of a layer.
  result <- 1 / (1 + exp(-x))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}


SoftmaxFunction <- function(x, deriv = FALSE) {
  #' Activation function for the output layer.
  #'
  #' @param x Vector with summed inputs per neuron.
  #' @param deriv Boolean, use the derivative softmax (TRUE) or the normal (FALSE).
  #' @return The output vector of the layer.
  max <- max(x)  # used to normalize output to prevent exponent going to Inf
  result <- exp(x - max) / sum(exp(x - max))
  if (deriv == TRUE) {
    result <- result * (1 - result)
  }
  return(result)
}
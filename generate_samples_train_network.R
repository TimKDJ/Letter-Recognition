############################################################################################################################################
# Functions that help create multiple noisy samples from the ones drawn on the pad. These are used for training by the code at the bottom. #                                                       
# To be used in conjunction with neural_network_engine.R.                                                                                  #
# Written by Tim de Jong for the course 'Programming: the next step' - date 25-05-2016.                                                    #                                      
############################################################################################################################################


source('neural_network_engine.R')


CreateSets <- function() {
  #' Build sets of samples for training (70%), validation (20%) and testing (10%). Include both original samples and newly generated ones.
  #'
  #' @return A list of sets (training/validation/test) which each contain matrices of letters and samples.
  originalSamples <- ParseCSVSamples()
  newSamples <- GenerateMultipleSamples(originalSamples)  # create shifted samples with added noise.
  # Each letter has a different amount of samples, which would introduce a bias in the network.
  # Therefore the lowest amount of samples for a given letter is the amount used for creating sets. 
  # This will ensure each letter has an equal amount of samples.
  v <- numeric(0)
  for (i in 1:length(newSamples)) {  # iterate over each letter
    v <- c(v, length(newSamples[[i]]))  # save the amount of samples
  }
  cat('Amount of samples per letter:', min(v))  # min(v) holds the lowest amount
  # Create 3 sequential sequences which will divide the amount of samples.
  trainSeq <- 1:floor(.7 * min(v))
  validateSeq <- (max(trainSeq) + 1):(max(trainSeq) + floor(.2 * min(v)))
  testSeq <- (max(validateSeq) + 1):(max(validateSeq) + floor(.1 * min(v)))
  sets <- list()
  for (i in 1:length(newSamples)) {  # iterate over each letter
    # Generate empty lists to be filled with samples.
    trainingSet <- matrix(list(), length(trainSeq), 2)
    validationSet <- matrix(list(), length(validateSeq), 2)
    testSet <- matrix(list(), length(testSeq), 2)
    letter <- names(newSamples)[i]  # current letter of the iteration
    # Create the trainingset for this letter.
    samples <- newSamples[[i]][trainSeq]
    trainingSet[, 1] <- letter
    trainingSet[, 2] <- samples
    # Create the validationset for this letter.
    samples <- newSamples[[i]][validateSeq]
    validationSet[, 1] <- letter
    validationSet[, 2] <- samples
    # Create the testset for this letter.
    samples <- newSamples[[i]][testSeq]
    testSet[, 1] <- letter
    testSet[, 2] <- samples
    # Add the generated sets for this letter to those of the other letters.
    sets[['training']] <- rbind2(sets[['training']], trainingSet)
    sets[['validation']] <- rbind2(sets[['validation']], validationSet)
    sets[['test']] <- rbind2(sets[['test']], testSet)
  }
  sets[['training']] <- sets[['training']][sample(nrow(sets[['training']])),]  # shuffle (avoids training letter for letter)
  return(sets)
}


ParseCSVSamples <- function() {
  #' Parse the samples file to obtain a workable list with samples.
  #'
  #' @return A matrix of lists, each row holds a sample letter and a sample vector.
  if (!file.exists('samples.csv')) {  # check if file exists
    stop('Cannot find samples.csv in ', getwd())
  }
  data <- read.delim('samples.csv', header=FALSE, sep=' ')  # it is a comma separated file
  if (ncol(data) != 1 + kPadRows * kPadCols) {  # check if the samples correspond with the current network, + 1 because it contains a letter
    stop('Number of columns in file (', ncol(data), ') do not correspond to needed amount (', 1 + kPadRows * kPadCols, ')')
  }
  samples <- matrix(list(), nrow(data), 2)
  for (i in 1:nrow(data)) {
    if (!data[i, 1] %in% letters || sum(!data[1, 2:ncol(data)] %in% 0:1) > 0) {  # check if the samples have a valid letter and vector
      stop('Invalid vector on line number ', i)
    }
    samples[[i, 1]] <- as.character(data[i, 1])  # sample letter
    samples[[i, 2]] <- as.numeric(data[i, 2:ncol(data)])  # sample vector
  }
  return(samples)
}


GenerateMultipleSamples <- function(input) {
  #' Take the original samples and generate multiple noisy shifted ones.
  #'
  #' @param input A matrix of lists, per row a letter and vector of length kPadCols * kPadRows.
  #' @return A list of letters, each containing the original and newly generated sample vectors.
  samples <- list()
  for (i in 1:nrow(input)) {  # iterate over the samples
    letter <- input[[i, 1]]
    s <- input[[i, 2]]
    sv <- ShiftSampleInSpace(s)  # shift each sample in space
    nsv <- numeric(0)
    for (j in 1:length(sv)) {  # iterate over the new shifted samples
      nsv <- c(nsv, AddNoise(sv[[j]]))  # add noise to each
    }
    samples[[letter]] <- sample(c(samples[[letter]], nsv))  # add the noisy shifted samples to the sample letter and shuffle them
    cat('letter:', letter, '| Amount of samples:', length(nsv), '\n')
  }
  return(samples)
  
}


AddNoise <- function(x) {
  #' Add random noise to a vector, resulting in several new vectors.
  #'
  #' @param x A vector of length kPadCols * kPadRows.
  #' @return A list containing the original and newly generated sample vectors.
  result <- list()
  result[[1]] <- x #add original vector in first slot
  c <- 2
  for (i in 1:length(x)) {  # loop over every element in the vector
    prob <- runif(1)
    if (prob > .90) { #each element has a .1 probability of changing and thereby producing a new vector
      result[[c]] <- x
      result[[c]][i] = ifelse(x[i] == 0, 1, 0) #change a 1 to 0 or 0 to 1 in position i
      c <- c + 1
    }
  }
  return(result)
}


ShiftSampleInSpace <- function(x) {
  #' Shift the entire sample in different directions to create new samples.
  #'
  #' @param x A vector of length kPadCols * kPadRows.
  #' @return A list containing the original and newly generated sample vectors.
  m <- matrix(x, kPadRows, kPadCols)  # change the vector to pad sized matrix
  moves <- GetFreeMovements(m)
  shiftedVec <- list()
  shiftedVec[[1]] <- x #add original vector in first slot
  c <- 2
  for (i in 1:ncol(moves)) {  # iterate over the directions
    if (moves[, i] > 0) {  # check if a direction allows a shift
      tmp <- m
      direction <- colnames(moves)[i]
      for (j in 1:moves[, i]) {  # iterate over the amount of shifts
        if (direction == 'up') {
          tmp <- tmp[-1, ]  # remove top row
          tmp <- rbind(tmp, 0)  # add bottom row
        } else if (direction == 'down') {
          tmp <- tmp[-kPadRows, ]  # remove bottom row
          tmp <- rbind(0, tmp)  # add top row
        } else if (direction == 'left') {
          tmp <- tmp[, -1]  # remove left column
          tmp <- cbind(tmp, 0)  # add right column
        } else if (direction == 'right') {
          tmp <- tmp[, -kPadCols]  # remove right column
          tmp <- cbind(0, tmp)  # add left column
        }
        shiftedVec[[c]] <- as.vector(tmp)  # revert the matrix back to a vector
        c <- c + 1
      }
    }
  }
  return(shiftedVec)
}


GetFreeMovements <- function(x) {
  #' Find the rows and columns that were left open and where the drawn sample can shift to.
  #'
  #' @param x A sample in matrix form.
  #' @return A matrix which states the possible shifts in any direction (e.g. 'up': 2, etc).
  direction <- matrix(0, 1, 4)
  colnames(direction) <- c('up', 'down', 'left', 'right')
  for (i in 1:kPadRows) {  # loop over matrix from top to bottom
    if (sum(x[i, ]) == 0) {  # the next encountered row is empty
      direction[, 'up'] <- direction[, 'up'] + 1
    } else {  # the next encountered row is not empty
      break
    }
  } 
  for (i in kPadRows:1) {  # loop over matrix from bottom to top
    if (sum(x[i, ]) == 0) {
      direction[, 'down'] <- direction[, 'down'] + 1
    } else {
      break
    }
  }
  for (i in 1:kPadCols) {  # loop over matrix from left to right
    if (sum(x[, i]) == 0) {
      direction[, 'left'] <- direction[, 'left'] + 1
    } else {
      break
    }
  }
  for (i in kPadCols:1) {  # loop over matrix from right to left
    if (sum(x[, i]) == 0) {
      direction[, 'right'] <- direction[, 'right'] + 1
    } else {
      break
    }
  }
  return(direction)
}

# create sets and train the network.
sets <- CreateSets()
result <- TrainNeuralNetwork(sets, loadWeights = FALSE, saveWeights = TRUE)  # train
layout(matrix(c(1,2,3), 1, 3))
plot(result[['training']], type='l', xlab='Epoch', ylab='Error', bty='l', las=1, col=2, main='Training', cex.lab=1.5, cex.main=2)  # plot training error
plot(result[['validation']], type='l', xlab='Epoch', ylab='Error', bty='l', las=1, col=3, main='Validation', cex.lab=1.5, cex.main=2)  # plot validation error
plot(result[['test']], type='l', xlab='Epoch', ylab='Accuracy', bty='l', las=1, col=4, main='Test', cex.lab=1.5, cex.main=2)  # plot test prediction accuracy
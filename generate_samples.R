source('neural_network_engine.R')

ParseCSVSamples <- function() {
  data <- read.delim('samples.csv', header=FALSE, sep=' ')
  samples <- matrix(list(), nrow(data), 2)
  for (i in 1:nrow(data)) {
    samples[[i, 1]] <- as.character(data[i, 1])
    samples[[i, 2]] <- unlist(data[i, 2:ncol(data)])
  }
  return(samples)
}

AddNoise <- function(x) {
  result <- list()
  result[[1]] <- x
  c <- 2
  for (i in 1:length(x)) {
    prob <- runif(1)
    if (prob > .5) { #every pixel has a .8 probability of changing and thereby giving a new vector
      result[[c]] <- x
      result[[c]][i] = ifelse(x[i] == 0, 1, 0)
      c <- c + 1
    }
  }
  return(result)
}

GetFreeMovements <- function(mat) {
  direction <- matrix(0, 1, 4)
  colnames(direction) <- c('up', 'down', 'left', 'right')
  for (i in 1:kPadRows) {
    if (sum(mat[i, ]) == 0) {
      direction[, 'up'] <- direction[, 'up'] + 1
    } else {
      break
    }
  } 
  for (i in kPadRows:1) {
    if (sum(mat[i, ]) == 0) {
      direction[, 'down'] <- direction[, 'down'] + 1
    } else {
      break
    }
  }
  for (i in 1:kPadCols) {
    if (sum(mat[, i]) == 0) {
      direction[, 'left'] <- direction[, 'left'] + 1
    } else {
      break
    }
  }
  for (i in kPadCols:1) {
    if (sum(mat[, i]) == 0) {
      direction[, 'right'] <- direction[, 'right'] + 1
    } else {
      break
    }
  }
  return(direction)
}

shiftSampleInSpace <- function(x) {
  m <- matrix(x, kPadRows, kPadCols)
  moves <- GetFreeMovements(m)
  shiftedVec <- list()
  shiftedVec[[1]] <- x
  c <- 2
  for (i in 1:ncol(moves)) {
    if (moves[, i] > 0) {
      tmp <- m
      direction <- colnames(moves)[i]
      for (j in 1:moves[, i]) {
        if (direction == 'up') {
          tmp <- tmp[-1, ]
          tmp <- rbind(tmp, 0)
        } else if (direction == 'down') {
          tmp <- tmp[-kPadRows, ]
          tmp <- rbind(0, tmp)
        } else if (direction == 'left') {
          tmp <- tmp[, -1]
          tmp <- cbind(tmp, 0)
        } else if (direction == 'right') {
          tmp <- tmp[, -kPadCols]
          tmp <- cbind(0, tmp)
        }
        shiftedVec[[c]] <- as.vector(tmp)
        c <- c + 1
      }
    }
  }
  return(shiftedVec)
}

GenerateMultipleSamples <- function(input) {
  samples <- list()
  for (i in 1:nrow(input)) {
    letter <- input[[i, 1]]
    s <- input[[i, 2]]
    sv <- shiftSampleInSpace(s)
    nsv <- numeric(0)
    for (j in 1:length(sv)) {
      nsv <- c(nsv, AddNoise(sv[[j]]))
    }
    samples[[letter]] <- sample(c(samples[[letter]], nsv))
    cat('letter:', letter, '| Amount of samples:', length(nsv), '\n')
  }
  return(samples)
  
}

CreateSets <- function() {
  writtenSamples <- ParseCSVSamples()
  newSamples <- GenerateMultipleSamples(writtenSamples)
  v <- numeric(0)
  for (i in 1:length(newSamples)) {
    v <- c(v, length(newSamples[[i]]))
  }
  cat('Amount of samples per letter:', min(v))
  trainSeq <- 1:floor(.7 * min(v))
  validateSeq <- (max(trainSeq) + 1):(max(trainSeq) + floor(.2 * min(v)))
  testSeq <- (max(validateSeq) + 1):(max(validateSeq) + floor(.1 * min(v)))
  sets <- list()
  for (i in 1:length(newSamples)) {
    #generate empty lists to be filled
    trainingSet <- matrix(list(), length(trainSeq), 2)
    validationSet <- matrix(list(), length(validateSeq), 2)
    testSet <- matrix(list(), length(testSeq), 2)
    
    letter <- names(newSamples)[i]
    #create trainingset one letter at a time
    samples <- newSamples[[i]][trainSeq]
    trainingSet[, 1] <- letter
    trainingSet[, 2] <- samples
    #validationset
    samples <- newSamples[[i]][validateSeq]
    validationSet[, 1] <- letter
    validationSet[, 2] <- samples
    #testset
    samples <- newSamples[[i]][testSeq]
    testSet[, 1] <- letter
    testSet[, 2] <- samples
    
    sets[['training']] <- rbind2(sets[['training']], trainingSet)
    sets[['validation']] <- rbind2(sets[['validation']], validationSet)
    sets[['test']] <- rbind2(sets[['test']], testSet)
  }
  sets[['training']] <- sets[['training']][sample(nrow(sets[['training']])),]
  return(sets)
}
sets <- CreateSets()
#NeuralNetwork(input[[3, 2]], 'run', FALSE) #run
result <- NeuralNetwork(sets, 'train', FALSE, TRUE) #train with samples csv

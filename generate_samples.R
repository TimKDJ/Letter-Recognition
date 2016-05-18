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
    result[[c]] <- x
    result[[c]][i] = ifelse(x[i] == 0, 1, 0)
    c <- c + 1
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
  mat <- matrix(x, kPadRows, kPadCols)
  moves <- GetFreeMovements(mat)
  shiftedVec <- list()
  shiftedVec[[1]] <- x
  c <- 2
  for (i in 1:ncol(moves)) {
    if (moves[, i] > 0) {
      tmp <- mat
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

GenerateNoisySamples <- function(input) {
  samples <- numeric(0)
  for (i in 1:nrow(input)) {
    letter <- input[[i, 1]]
    s <- input[[i, 2]]
    sv <- shiftSampleInSpace(s)
    nsv <- numeric(0)
    for (j in 1:length(sv)) {
      nsv <- c(nsv, AddNoise(sv[[j]]))
    }
    nsvSamples <- matrix(list(), length(nsv), 2)
    nsvSamples[, 1] <- letter
    nsvSamples[, 2] <- nsv
    samples <- rbind2(samples, nsvSamples)
    cat('letter:', letter, '| Amount of samples:', length(nsv), '\n')
  }
  return(samples)
  
}



#p <- list(sample(0:1, 100, TRUE))
#f <- input[[1, ]]
#x[seq(10, 100, 10)] <- 0
#x[1:10] <- 0
#x[81:100] <- 0
#matrix(input[[1,2]], kPadRows, kPadCols)
#z <- GenerateNoisySamples(input)

singularSamples <- ParseCSVSamples()
multipleSamples <- GenerateNoisySamples(singularSamples)
#NeuralNetwork(input[[3, 2]], 'run', FALSE) #run
NeuralNetwork(multipleSamples, 'train', FALSE, TRUE) #train with samples csv

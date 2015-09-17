# Gini index, two class case.
impurity <- function(vect){
  freq <- (sum(vect == 0))/length(vect)
  return (freq * (1-freq))
}

# Given a vector of integers, returns a list of potential splitpoints.
splitpoints <- function(vect){
  sorted <- sort(unique(vect))
  splitpoints <- (sorted[1:(length(vect)-1)]+sorted[2:length(vect)])/2
  splitpoints <- splitpoints[!is.na(splitpoints)]
  
  return (splitpoints)
}

# x - input dataframe
# y - classes vector
# c - column index
optimalsplit <- function(x, y, c){
  potentialsplits <- splitpoints(x[,c])
  
  splits <- mapply(function(pivot) split(x, y, c, pivot), potentialsplits)
  
  print(splits)

  return(potentialsplits)
}

# split the dataset into two parts and return the impurity for this split
split <- function(x, y, c, pivot){
  ordering <- order(x[,c])
  x.ordered <- x[ordering,]
  y.ordered <- y[ordering]
  
  x.left <- subset(x.ordered, x.ordered[,c] < pivot)
  x.right <- subset(x.ordered, x.ordered[,c] > pivot)
  
  offset <- nrow(x.left)
  
  y.left <- y.ordered[1:offset]
  y.right <- y.ordered[offset+1:(length(y)-offset)]
  y.length <- length(y)
  
  i.left = impurity(y.left)*(length(y.left)/y.length)
  i.right = impurity(y.right)*(length(y.right)/y.length)
  score = i.left + i.right
  
  return(list(score, x.left, x.right))
}

# x - input data matrix
# y - class label vector
# nmin - minimum number of observations in a node
# minleaf - minimum number of observations in a leaf
tree.grow <- function(x, y, nmin, minleaf){
  
}

# x - input data matrix
# tr - a tree object
tree.classify <- function(x, tr){
  
}

credit.dat <- read.csv('~/UU/MDM/credit.txt')

classes <- (credit.dat[,6])

a <- optimalsplit(credit.dat, classes, 4)



# income <- (credit.dat[,4])
# input <- data.frame(income, classes)
# input <- input[order(input[,1]),]
# 
# test <- subset(input, input[,1] > 52)
# test2 <- subset(input, input[,1] < 52)


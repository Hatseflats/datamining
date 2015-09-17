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

# Returns all possible splits (yay bruteforcing).
# x - input data matrix.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
allsplits <- function(x, y, nmin, minleaf){
  
}

# Given a list of splits, filter out the splits that do not adhere to the nmin and minleaf constraints.
# splits - a list of splits.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
filtersplits <- function(splits, nmin, minleaf){
  eligible <- (apply(splits, 2, function(s) checksplit(s, nmin, minleaf)))
  return (splits[,eligible])
}

# Filtersplits helper function.
checksplit <- function(split, nmin, minleaf){
  left.i <- split[[1]][[1]] # impurity is stored in the first position
  left.size <- length(split[[1]][[2]])  # left part of the split

  right.i <- split[[2]][[1]] # impurity is stored in the first position
  right.size <- length(split[[2]][[2]])  # left part of the split
  
  validate <- function(i, size){
    if(i == 0){ # its a leaf
      return (size >= minleaf)
    } else { # its a node
      return (size >= nmin)
    }
  }
  
  return (validate(left.i, left.size) & validate(right.i, right.size))
}

# Returns a list of all possible splits on one column with their scores.
# x - input dataframe.
# y - classes vector.
# c - column index.
columnsplits <- function(x, y, c){
  potentialsplits <- splitpoints(x[,c])
  splits <- mapply(function(pivot) split(x, y, c, pivot), potentialsplits)
  return(splits)
}

# Split the dataset into two parts and return the impurity for this split.
# x - input dataframe.
# y - classes vector.
# c - column index.
# pivot - value to split on.
split <- function(x, y, c, pivot){
  ordering <- order(x[,c])
  x.ordered <- x[ordering,]
  y.ordered <- y[ordering]
  
  x.left <- which(x.ordered[,c] < pivot)
  x.right <- which(x.ordered[,c] > pivot)
  
  offset <- length(x.left)
  
  y.left <- y.ordered[1:offset]
  y.right <- y.ordered[offset+1:(length(y)-offset)]
  y.length <- length(y)
  
  i.left = impurity(y.left)*(length(y.left)/y.length)
  i.right = impurity(y.right)*(length(y.right)/y.length)
  score = i.left + i.right
  
  return(list(list(i.left, x.left), list(i.right, x.right)))
  # return(list(score, x.left, x.right))
}

# Grow a tree.
# x - input data matrix.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
tree.grow <- function(x, y, nmin, minleaf){
  
}

# Predict the classes of x.
# x - input data matrix
# tr - a tree object
tree.classify <- function(x, tr){
  
}

# tr - a tree object
tree.simplify <- function(tr){
  
}

credit.dat <- read.csv('~/UU/MDM/datamining/assignment1/data/credit.txt')

classes <- (credit.dat[,6])

a <- columnsplits(credit.dat, classes, 4)
b <- filtersplits(a, 2, 2)

print(a)
print(b)




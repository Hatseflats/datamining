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
  splits <- (mapply(function(c) columnsplits(x,y,c), colnames(x)))
  filtered <- (lapply(splits, function(s) filtersplits(s, nmin, minleaf)))

  return(filtered)
}

# Given a list of splits, filter out the splits that do not adhere to the nmin and minleaf constraints.
# splits - a list of splits.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
filtersplits <- function(splits, nmin, minleaf){
  eligible <- Filter(function(s) checksplit(s, nmin, minleaf), splits)
  return (eligible)
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
  splits <- lapply(potentialsplits, function(pivot) split(x, y, c, pivot))
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
  
  x.left <- rownames(subset(x, x[,c] < pivot))
  x.right <- rownames(subset(x, x[,c] > pivot))
  
  offset <- length(x.left)
  
  y.left <- y.ordered[1:offset]
  y.right <- y.ordered[offset+1:(length(y)-offset)]
  y.length <- length(y)
  
  i.left = impurity(y.left)*(length(y.left)/y.length)
  i.right = impurity(y.right)*(length(y.right)/y.length)
  score = i.left + i.right
  
  return(list(list(i.left, x.left), list(i.right, x.right), c, pivot))
}

# Given a list of splits, returns the best one.
bestsplit <- function(splits){
  return (Reduce(minsplit, splits, splits[[1]]))
}

# Given two splits, returns the split with the lowest impurity.
minsplit <- function(s1, s2){
  score1 <- s1[[1]][[1]] + s1[[2]][[1]]
  score2 <- s2[[1]][[1]] + s2[[2]][[1]]
  
  if(score1 <= score2){
    return(s1)
  } else {
    return(s2)
  }
}
# Grow a tree.
# x - input data matrix.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
tree.grow <- function(x, y, nmin, minleaf){
  
  possiblesplits <- allsplits(x, y, nmin, minleaf)
  possiblesplits <- Filter(function(column) length(column) > 0, possiblesplits)
  
  columns.optimal <- lapply(possiblesplits, bestsplit)
  optimal <- bestsplit(columns.optimal)

  left.i <- optimal[[1]][[1]]
  left.rownames <- optimal[[1]][[2]]
  left.classes <- y[which(rownames(x) %in% left.rownames)]
  left.rows <- x[left.rownames,]
  
  right.i <- optimal[[2]][[1]]
  right.rownames <- optimal[[2]][[2]]
  right.classes <- y[which(rownames(x) %in% right.rownames)]
  right.rows <- x[right.rownames,]
  
  split.column <- optimal[[3]]
  split.pivot <- optimal[[4]]
  
  if(left.i > 0){
    left.result <- tree.grow(left.rows, left.classes, nmin, minleaf)
  } else {
    left.result <- list(left.rownames, left.classes)
  }
  
  if(right.i > 0){
    right.result <- tree.grow(right.rows, right.classes, nmin, minleaf)
  } else {
    right.result <- list(right.rownames, right.classes)
  }
  
  node <- list(rownames(x), left.result, right.result, split.column, split.pivot, y)
  return(node)
}

# Predict the classes of x.
# x - input data matrix
# tr - a tree object
tree.classify <- function(x, tr){
  
}

# Predicts the class of x.
# x - input row.
# tr - a tree object.
tree.classify.findleaf <- function(x, tr){
  if(length(x) == 2){ # leaves only have a list of rownumbers and hteir classes
    return(x)
  } else { # nodes are of length 6.
    left <- tr[[2]]
    right <- tr[[3]]
    column <- tr[[4]]
    pivot <- tr[[5]]
    
    value <- x[column]
    
    if(value < pivot){
      result <- tree.classify.row(x, left)
    } else {
      result <- tree.classify.row(x, right)
    }
  }
}

# tr - a tree object
tree.simplify <- function(tr){
  
}

credit.dat <- read.csv('~/UU/MDM/datamining/assignment1/data/credit.txt')

classes <- (credit.dat[,6])
credit.dat <- (credit.dat[,-6])

tr <- tree.grow(credit.dat, classes, 2, 1)

print(tr)






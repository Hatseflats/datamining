
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
  
  if(length(possiblesplits) == 0){
    return(list(rownames(x), y))
  }
  
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
  
  classify <- function(row, tree){
    leaf <- findleaf(row,tree)
    return(majorityclass(leaf))
  }
  
  return(apply(x, 1, function(x_) classify(x_, tr)))
}

# Given a label of class vectors, returns the majority class
majorityclass <- function(y){
 count.1 <- sum(y == 1)
 count.0 <- sum(y == 0)
 
  if(count.0 > count.1){
    return (0)
  } else {
    return (1)
  }
}

# Finds the leaf that x should belong to
# x - input row.
# tr - a tree object.
findleaf <- function(x, tr){
  if(length(tr) == 2){ # leaves only have a list of rownumbers and their classes
    return(tr[[2]])
  } else { # nodes are of length 6.
    left <- tr[[2]]
    right <- tr[[3]]
    column <- tr[[4]]
    pivot <- tr[[5]]
    
    value <- x[column]
    
    if(value < pivot){
      result <- findleaf(x, left)
    } else {
      result <- findleaf(x, right)
    }
  }
}

# tr - a tree object
tree.simplify <- function(tr){
  if(isleaf(tr)){
    return(tr)
  }
  
  leftchild <- tr[[2]]
  rightchild <- tr[[3]]
  
  if(isleaf(leftchild) & isleaf(rightchild)){
    leftmajority <- majorityclass(leftchild[[2]])
    rightmajority <- majorityclass(rightchild[[2]])
    
    if(leftmajority == rightmajority){
      return(list(tr[[1]], tr[[6]]))
    }
  } else {
    return(list(tr[[1]], tree.simplify(tr[[2]]), tree.simplify(tr[[3]]), tr[[4]], tr[[5]], tr[[6]]))
  }
}

isleaf <- function(tr){
  if(length(tr) == 2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

counterrors <- function(classes, predictions){
  results <- data.frame(class=classes,prediction=predictions)
  wrong <- 0
  
  for(i in 1:nrow(results)){
    row <- results[i,]
    
    if(row[,1] != row[,2]){
      wrong <- wrong + 1
    }
  }
  return(wrong)
}

getfolds <- function(data, n){
  
  folds <- list()
  input <- data
  while(nrow(input) != 0){

    fold <- input[sample(nrow(input), n),]
    remainder <- input[-(which(rownames(input) %in% rownames(fold))),]
    input <- remainder
    folds[[length(folds)+1]] <- fold 
  }
  return(folds)
}

crossvalidation <- function(x, y, nmin, minleaf){
  trainingset <- x[sample(nrow(x), 200),]
  trainingset.indices <- which(rownames(x) %in% rownames(trainingset))
  trainingset.classes <- y[trainingset.indices]
  
  testsample <- x[-(trainingset.indices),]
  
  folds <- getfolds(trainingset, 20)
  
  foldtree <- function(fold, trainingset, classes, nmin, minleaf){
    fold.indices <- which(rownames(trainingset) %in% rownames(fold))
    fold.classes <- classes[fold.indices]
    
    data <- trainingset[-fold.indices,]
    data.classes <- classes[-fold.indices]
    
    tree <- tree.grow(data, data.classes, nmin, minleaf)
    predictions <- tree.classify(fold, tree)
    errors <- counterrors(fold.classes, predictions)
    
    return(list(errors))
  }
  
  start.time <- Sys.time()

  result <- lapply(folds, function(f) foldtree(f, trainingset, trainingset.classes, nmin, minleaf))
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  print(time.taken)
  
  return(result)
}

main <- function(){
  data <- read.csv('~/UU/MDM/datamining/assignment1/data/heartbin.txt')
  classes <- data[,length(data)]
  data <- data[,1:length(data)-1]
  
  results <- crossvalidation(data, classes, 1, 1)
  
  print(results)
  
}  

main()





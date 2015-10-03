# Definitions:
# Tree -  A list containing the rownumbers in the current node, the left tree/leaf, 
#         the right tree/leaf, the classes of the rows in this node and information about the split made. 
# Leaf -  A list containing a list of rownumbers and a list of their classes.
# Split - A list containing the type of the split (NUMERIC/CATEGORICAL), a list with the rownumbers and impurity of the left child, 
#         a list with the rownumbers and impurity of the right child and information about how the split is made.

# impurity
# vect - a binary numeric vector.
# returns: a numeric value indicating the impurity of the given input vector.
# Calculates the impurity using the Gini-index (binary class case).
impurity <- function(vect){
  freq <- (sum(vect == 0))/length(vect)
  return (freq * (1-freq))
}

# splitpoints
# vect - a numeric vector.
# returns: a numeric vector containing possible splitpoints.
# Calculates all values between two consecutive values in the input list, to be used as splitpoints.
splitpoints <- function(vect){
  sorted <- sort(unique(vect))
  splitpoints <- (sorted[1:(length(vect)-1)]+sorted[2:length(vect)])/2
  splitpoints <- splitpoints[!is.na(splitpoints)]
  
  return (splitpoints)
}

# allsplits
# x - input dataframe.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
# returns: a list of lists, containing all possible splits per attribute. 
# Calculates all possible splits that satisfy the nmin and minleaf constraints.
allsplits <- function(x, y, nmin, minleaf){
  column.names <- numeric.attributes()
  numeric.splits <- mapply(function(c) columnsplits.numeric(x,y,c), column.names)
  numeric.filtered <- lapply(numeric.splits, function(s) filtersplits(s, nmin, minleaf))
  
  categories <- categorical.attributes()
  categorical.sets <- lapply(categories, function(cat) categorical.splitpoints(x[,cat], y))
  # ugh
  categorical.splits <- lapply(categorical.sets, function(set) lapply(set, function(subset) split.categorical(x, y, subset)))
  categorical.filtered <- lapply(categorical.splits, function(s) filtersplits(s, nmin, minleaf))
  

  return(append(categorical.filtered, numeric.filtered))
}

# filtersplits.
# splits - a list of splits.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
# returns: a list of eligible split objects.
# Given a list of splits, filter out the splits that do not adhere to the nmin and minleaf constraints.
filtersplits <- function(splits, nmin, minleaf){
  eligible <- Filter(function(s) checksplit(s, nmin, minleaf), splits)
  return (eligible)
}

# checksplit.
# split - a split object.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
# returns: a boolean value indicating that this split is usable. 
# Checks if a split adhere to the nmin and minleaf constraints and that it is usable.
checksplit <- function(split, nmin, minleaf){
  left.i <- split[[2]][[1]] # impurity is stored in the first position
  left.size <- length(split[[2]][[2]])  # left part of the split

  right.i <- split[[3]][[1]] # impurity is stored in the first position
  right.size <- length(split[[3]][[2]])  # left part of the split
  
  validate <- function(i, size){
    if(size == 0){
      return(FALSE)
    }
    if(i == 0){ # its a leaf
      return (size >= minleaf)
    } else { # its a node
      return (size >= nmin)
    }
  }

  return (validate(left.i, left.size) & validate(right.i, right.size))
}

# columnsplits.numeric
# x - input dataframe.
# y - classes vector.
# c - column index.
# returns: a list of all splits on one numeric column.
# Calculates all splits on a numeric column.
columnsplits.numeric <- function(x, y, c){
  potentialsplits <- splitpoints(x[,c])
  splits <- lapply(potentialsplits, function(pivot) split.numeric(x, y, c, pivot))
  return(splits)
}

# split.numeric
# x - input dataframe.
# y - classes vector.
# c - column index.
# pivot - value to split on.
# returns: a split object.
# Splits the input in two at a certain point and calculates the impurity, only works on numeric attributes.
split.numeric <- function(x, y, c, pivot){
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
  
  return(list("NUMERIC", list(i.left, x.left), list(i.right, x.right), c, pivot))
}

# bestsplit
# splits - a list of split objects.
# returns: the split object with the lowest impurity
# Finds the optimal split from a list of splits.
bestsplit <- function(splits){
  return (Reduce(minsplit, splits, splits[[1]]))
}

# minsplit
# s1 - a split object.
# s2 - a split object.
# returns: a split object with the lowest impurity.
# Compares two split objects to find the one with the lowest impurity, used to determine the optimal split.
minsplit <- function(s1, s2){
  score1 <- s1[[2]][[1]] + s1[[3]][[1]]
  score2 <- s2[[2]][[1]] + s2[[3]][[1]]
  
  if(score1 <= score2){
    return(s1)
  } else {
    return(s2)
  }
}
# tree.grow
# x - input dataframe.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
# returns: a tree object.
# Grows a classification tree on the dataset.
tree.grow <- function(x, y, nmin, minleaf){

  possiblesplits <- allsplits(x, y, nmin, minleaf)
  possiblesplits <- Filter(function(column) length(column) > 0, possiblesplits)
  
  if(length(possiblesplits) == 0){
    return(list(rownames(x), y))
  }
  
  columns.optimal <- lapply(possiblesplits, bestsplit)
  optimal <- bestsplit(columns.optimal)

  left.i <- optimal[[2]][[1]]
  left.rownames <- optimal[[2]][[2]]
  left.classes <- y[which(rownames(x) %in% left.rownames)]
  left.rows <- x[left.rownames,]
  
  right.i <- optimal[[3]][[1]]
  right.rownames <- optimal[[3]][[2]]
  right.classes <- y[which(rownames(x) %in% right.rownames)]
  right.rows <- x[right.rownames,]
  
  split.info <- list
  
  if(optimal[[1]] == "NUMERIC"){
    split.info <- list("NUMERIC", optimal[[4]], optimal[[5]])
  } else {
    split.info <- list("CATEGORICAL", optimal[[4]])
  }
  
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
  
  node <- list(rownames(x), left.result, right.result, y, split.info)
  return(node)
}

# tree.classify
# x - input dataframe
# tr - a tree object
# returns: a vector of classes.
# Tries to guess the classes of all rows in the input dataframe. 
tree.classify <- function(x, tr){
  classify <- function(row, tree){
    leaf <- findleaf(row,tree)
    return(majorityclass(leaf))
  }
  return(apply(x, 1, function(x_) classify(x_, tr)))
}

# majorityclass
# y - class label vector.
# returns: a binary number.
# Calculates the majority class of the given class vector.
majorityclass <- function(y){
 count.1 <- sum(y == 1)
 count.0 <- sum(y == 0)
 
  if(count.0 > count.1){
    return (0)
  } else {
    return (1)
  }
}

# findleaf
# x - input row.
# tr - a tree object.
# returns: a leaf object.
# Traverses the tree, using the attributes from the input row until a leaf is found. 
findleaf <- function(x, tr){
  if(isleaf(tr)){ # leaves only have a list of rownumbers and their classes
    return(tr[[2]])
  } else { # nodes are of length 6.
    left <- tr[[2]]
    right <- tr[[3]]
    info <- tr[[5]]
    
    if(info[[1]] == 'NUMERIC') {
      column <- info[[2]]
      pivot <- info[[3]]
      
      value <- x[column]
      
      if(value < pivot){
        result <- findleaf(x, left)
      } else {
        result <- findleaf(x, right)
      }
      return(result)
    } else {
      columns <- info[[2]]

      if(categorical.subset(x, columns)){
        result <- findleaf(x, left)
      } else {
        result <- findleaf(x, right)
      }
      return(result)
    }
  }
}

# tree.simplify
# tr - a tree object.
# returns: a tree object.
# Tries to simplify the tree by removing sibling leafs if their majority classes are equal. 
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

# isleaf
# tr - A tree/leaf object.
# returns: a boolean.
# Checks if the input is a leaf or an internal node.  
isleaf <- function(tr){
  if(length(tr) == 2){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# counterrors
# classes - class label vector.
# predictions - class label vector.
# returns: amount of errors made. 
# Compares predictions with the actual class labels and counts the amount of errors made.
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

# getfolds
# data - input dataframe.
# n - size of a fold.
# returns: a list of folds. 
# Takes random folds of size n from the input data, until the input is empty and returns all folds afterwards.
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

# crossvalidation
# x - input dataframe.
# y - class label vector.
# nmin - minimum number of observations in a node.
# minleaf - minimum number of observations in a leaf.
# returns: a list of vectors with the errorrate, the amount of internal nodes and the amount of leaves per fold.
# Performs 10 fold crossvalidation. 
crossvalidation <- function(x, y, nmin, minleaf){
  trainingset <- x[sample(nrow(x), 200),]
  trainingset <- trainingset[order(as.numeric(rownames(trainingset))),]
  trainingset.indices <- which(rownames(x) %in% rownames(trainingset))
  trainingset.classes <- y[trainingset.indices]
  
  testsample <- x[-(trainingset.indices),]
  
  folds <- getfolds(trainingset, 20)
  
  foldtree <- function(fold, trainingset, classes, nmin, minleaf){
    fold <- fold[order(as.numeric(rownames(fold))),]

    fold.indices <- which(rownames(trainingset) %in% rownames(fold))
    fold.classes <- classes[fold.indices]
    
    data <- trainingset[-fold.indices,]
    data.classes <- classes[-fold.indices]
    
    tree <- tree.grow(data, data.classes, nmin, minleaf)
    predictions <- tree.classify(fold, tree)
    errors <- counterrors(fold.classes, predictions)
    
    errorrate <- errors/(nrow(fold))
    
    return(list(errorrate, tree.size(tree)))
  }
  
  result <- lapply(folds, function(f) foldtree(f, trainingset, trainingset.classes, nmin, minleaf))
  
  return(result)
}

# categorical.subset
# row - a row from a dataframe.
# columns - A subset of columns of a categorical attribute.
# returns: a boolean value.
# Checks if the given row has any of the category labels from the given attribute.
categorical.subset <- function(row, columns){
  s <- sapply(columns, function(c) row[c] == 1)
  return(any(s))
}

# split.categorical
# x - input dataframe.
# y - classes vector.
# s - vector of possible categories for a categorical attribute.
# returns: a split object.
# Splits the input in two at a certain point and calculates the impurity, only works on categorical attributes.
split.categorical <- function(x, y, s){
  rows <- apply(x, 1, function(row) categorical.subset(row, s))
  
  x.left <- rownames(x[rows,])
  x.right <- rownames(x[!rows,])
  
  y.left <- y[which(rownames(x) %in% x.left)]
  y.right <- y[which(rownames(x) %in% x.right)]
  
  y.length <- length(y)
  
  i.left = impurity(y.left)*(length(y.left)/y.length)
  i.right = impurity(y.right)*(length(y.right)/y.length)
  
  return(list("CATEGORICAL", list(i.left, x.left), list(i.right, x.right), s))
}

# categorical.splitpoints
# data <- A subset of the original data, containing only the columns for one categorical attribute.
# classes <- class vector.
# returns: a list of possible splits to be made on a categorical attribute. 
# Calculates all possible splitpoints on a categorical attributes. 
# Uses the method shown in class so that we only have to check L-1 splits.
categorical.splitpoints <- function(data, classes){
  
  calcprob <- function(col, classes){
    m <- matrix(c(col, classes), nrow=length(classes), ncol=2)
    total <- sum(m[,1] == 1)
    zeroes <- sum(m[,1] == 1 & m[,2] == 0)

    return(zeroes/total)
  }
  
  result <- apply(data, 2, function(x) calcprob(x,classes))
  result <- sort(result)
  
  cat.splits <- list()
  
  for(i in 1:(length(result)-1)){
    cat.splits[length(cat.splits)+1] <- list(names(result)[1:i])
  }
  
  return(cat.splits)
}

# numeric.attributes
# returns: list of numeric attributes in the heartdisease dataset.
# Hardcoded list to make life easier. 
numeric.attributes <- function(){
  return(list("Age","Sex","RestBP","Chol","Fbs","RestECG","MaxHR","ExAng","Oldpeak","Slope","Ca"))
}

# categorical.attributes
# returns: list of categorical attributes in the heartdisease dataset.
# Hardcoded list to make life easier. 
categorical.attributes <- function(){
  cat1 <- c("ChestPain.asymptomatic","ChestPain.nonanginal","ChestPain.nontypical","ChestPain.typical")
  cat2 <- c("Thal.fixed","Thal.normal","Thal.reversable")
  
  return(list(cat1, cat2))
}

# tree.size
# tree - a tree object.
# returns: a numeric vector with the amount of internal nodes and the amount of leaves.
# Calculates the size of a tree.
tree.size <- function(tree){
  if(isleaf(tree)){
    return (c(0,1))
  } 

  result <- c(1,0) + tree.size(tree[[2]]) + tree.size(tree[[3]])
  return(result)
}

# parameterexperiment
# data - input data.
# classes - class label vector.
# returns: a list of numeric vectors with nmin, minleaf, errorrate, number of leaves, number of nodes and tree construction time. 
# Used to test several nmin and minleaf parameters.
parameterexperiment <- function(data, classes){
  
  runcrossval <- function(data, classes, runs, nmin, minleaf){
    totalerrors <- 0
    totalsize <- c(0,0)
    totaltime <- 0
    
    for (i in 1:runs){
      start.time <- Sys.time()
    
      results <- crossvalidation(data, classes, nmin, minleaf)
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      totaltime <- totaltime + time.taken
      
      for(i in 1:length(results)){
        row <- results[[i]]
        totalerrors <- totalerrors + row[[1]]
        totalsize <- totalsize + row[[2]]
      }
    }
    
    avgerrors <- totalerrors/(10*runs)
    avgsize <- totalsize/(10*runs)
    avgtime <- totaltime/(10*runs)
    
    return(c(avgerrors, avgsize, avgtime))
  }
  
  results <- list()
  nmin <- 0
  minleaf <- 0
  
  for(i in 1:20){
    nmin <- nmin + 4
    minleaf <- minleaf + 1
    
    result <- c(nmin, minleaf, runcrossval(data,classes,5,nmin,minleaf))
    results[[length(results)+1]] <- result
    
  }
  
  return(results)
}

# main function
main <- function(){
  data <- read.csv('~/UU/MDM/datamining/assignment1/data/heartbin.txt')
  classes <- data[,length(data)]
  data <- data[,1:(length(data)-1)]

  
  a <- parameterexperiment(data, classes)
  
  print(a)

}  
main()

# graph.cliques
# graph - an adjacency matrix.
# returns: a list of numeric vectors.
# Finds all cliques in the given graph.
graph.cliques <- function(graph){
  graph.cliques_ <- function (R,P,X,graph,cliques) {
    if (length(P)==0 & length(X)==0) {
      cliques <- list(R)
    } else {
      pivot <- P[sample(length(P),1)]
      for(i in 1:length(P)){
        pivot.nb <- graph.neighbors(graph,pivot)
        if(!is.element(P[i],pivot.nb)){
          P.temp <- setdiff(P,P[i])
          R.new <- union(R,P[i])
          P.new <- intersect(P.temp,graph.neighbors(graph,P[i]))
          X.new <- intersect(X,graph.neighbors(graph,P[i]))
          cliques <- c(cliques,graph.cliques_(R.new,P.new,X.new,graph,list()))
          X <- union(X,P[i])}
      }}
    return(cliques)
  }

  cliques <- graph.cliques_(c(),1:nrow(graph),c(),graph,list())

  return(unique(lapply(cliques,sort)))
}

# graph.neighbors
# graph - an adjacency matrix.
# node - numeric.
# returns: a vector of integers.
# Returns the nodes adjacent to the given node.
graph.neighbors <- function (graph,node) {
  nnodes <- dim(graph)[2]
  return((1:nnodes)[graph[node,]==1])
}

# graph.init
# nnodes - numeric.
# returns: an adjacency matrix.
# Returns an empty adjacency matrix of size nnodes.
graph.init <- function(nnodes){
  return(matrix(0,nnodes,nnodes))
}

# graph.neighborhood
# graph - an adjacency matrix.
# forward - boolean.
# backward - boolean.
# returns: a list of matrices.
# Calculates all neighbors of the given graph, 
#   where neighbors are the graphs obtained by removing or adding a single edge.
graph.neighborhood <- function(graph, forward, backward){

  neighbor <- function(forward, backward){
    if(forward & backward){
      return (function(x){
        return(1-x)
      })
    } else if(!forward & !backward){
      return (identity)
    } else if(forward & !backward){
      return (function(x){
        return(1)
      })
    } else if(!forward & backward){
      return (function(x){
        return(0)
      })
    }
  }

  func <- neighbor(forward, backward)

  nnodes <- nrow(graph)

  neighbors <- list()

  for(i in 1:(nnodes-1)){
    for(j in (i+1):nnodes){
      copy <- graph
      x <- func(copy[i,j])
      copy[j,i] <- x
      copy[i,j] <- x

      if(!matrix.equal(graph,copy)){
        neighbors[[length(neighbors)+1]] <- copy
      }
    }
  }
  return(neighbors)
}

# graph.random
# prob - numeric between 0 and 1.
# nnodes - numeric.
# returns: an adjacency matrix of size nnodes.
# Constructs a random graph of size nnodes, 
#   where prob is the probability of an edge between nodes.
graph.random <- function(prob, nnodes){
  graph <- graph.init(nnodes)

  for(i in 1:(nnodes-1)){
    range <- (i+1):nnodes
    edges <- sapply(runif(length(range), 0, 1),function(x_i) as.numeric(x_i < prob))
    
    for(j in range){
      graph[i,j] <- edges[j-i]
      graph[j,i] <- edges[j-i]
    }
  }
  return(graph)
}  

# gm.search
# data - numeric table.
# graph - an adjacency matrix.
# forward - boolean.
# backward - boolean.
# score - string ("AIC"|"BIC").
# returns: a list with the following named components:
#  - cliques: a list of numeric vectors.
#  - score: numeric, AIC or BIC score of the final model.
#  - trace: list of traces of the search process.
#  - call: the call to the function gm.search that produced this result.
# Tries to find a good graphical model by using a local search.
gm.search <- function(data, graph, forward, backward, score){
  
}

# gm.restart
# nstart - numeric.
# prob - numeric between 0 and 1. 
# seed - numeric.
# data - numeric table.
# graph - an adjacency matrix.
# forward - boolean.
# backward - boolean.
# score - string ("AIC"|"BIC").
# returns: a list with the following named components:
#  - cliques: a list of numeric vectors.
#  - score: numeric, AIC or BIC score of the final model.
#  - trace: list of traces of the search process.
#  - call1: the call to the function gm.search that produced this result.
#  - call2: the call to the function gm.restart that produced this result.
# Performs a multistart localsearch to find a good graphical model.
gm.restart <- function(nstart, prob, seed, data, forward, backward, score, graphsize = 6){
  set.seed(seed)

}

# gm.score
# data - numeric table.
# cliques - a list of numeric vectors.
# score - string ("AIC"|"BIC").
# returns: numeric score.
# Calculates the score of a graphical model.
gm.score <- function(data, cliques, score){
  if(score != 'AIC' & score != 'BIC'){
    stop("Unknown kind of score")
  }

  model <- loglin(data, cliques, print=FALSE)

  dim <- length(data) - model$df
  n <- sum(data)
  dev <- model$lrt

  if(score == 'AIC'){
    return(dev + 2 * dim)
  } else {
    return(dev + log(n) * dim)
  }
}

# matrix.equal
# x - a matrix.
# y - another matrix.
# returns: a boolean.
# Checks whether the matrices x and y are equal.
matrix.equal <- function(x, y){
  is.matrix(x) && is.matrix(y) && 
    dim(x) == dim(y) && all(x == y)
}

coronary.dat <- read.table("~/UU/MDM/datamining/assignment2/data/coronary.txt",header=T)
  
t <- table(coronary.dat)

g <- graph.random(0.5, 6)

g.n <- graph.neighborhood(g, TRUE, FALSE)

print(graph.neighbors(g,2))


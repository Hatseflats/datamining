graph.cliques <- function(graph){
  graph.cliques_ <- function (R,P,X,graph,cliques) {
    if (length(P)==0 & length(X)==0) {
      cliques <- list(R)
    } else {
      pivot <- P[sample(length(P),1)]
      for(i in 1:length(P)){
        pivot.nb <- neighbors(graph,pivot)
        if(!is.element(P[i],pivot.nb)){
          P.temp <- setdiff(P,P[i])
          R.new <- union(R,P[i])
          P.new <- intersect(P.temp,neighbors(graph,P[i]))
          X.new <- intersect(X,neighbors(graph,P[i]))
          cliques <- c(cliques,graph.cliques_(R.new,P.new,X.new,graph,list()))
          X <- union(X,P[i])}
      }}
    return(cliques)
  }
  return(graph.cliques_(c(),1:nrow(graph),c(),graph,list()))
}

graph.neighbors <- function (graph,node) {
  nnodes <- dim(graph)[2]
  return((1:nnodes)[graph[node,]==1])
}

graph.init <- function(nnodes){
  return(matrix(0,nnodes,nnodes))
}

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


post.process <- function (cliques) {
  unique(lapply(cliques,sort))
}

gm.search <- function(counts, graph, forward, backward, score){
  
}

gm.restart <- function(nstart, prob, seed, counts, forward, backward, score, graphsize = 6){
  
}

coronary.dat <- read.table("~/UU/MDM/datamining/assignment2/data/coronary.txt",header=T)
  
t <- table(coronary.dat)

g <- graph.random(0.50,6)

n <- graph.neighbors(g,1)

print(g)


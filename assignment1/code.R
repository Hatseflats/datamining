credit.dat <- read.csv('~/UU/MDM/credit.txt')
y <- c(1,0,1,1,1,0,0,1,1,0,1)

impurity <- function(vect){
  freq <- (sum(vect == 0))/length(vect)
  return (freq * (1-freq))
}

split <- function(vect, pivot){
	
}

optimalsplit <- function(x,y){

  sorted <- sort(unique(x))
  splitpoints <- (sorted[1:(length(x)-1)]+sorted[2:length(x)])/2
  splitpoints <- splitpoints[!is.na(splitpoints)]
#  imp <- impurity(y)
  
  ind <- order(x);
  dat <- (cbind(x[ind], y[ind]))
}
  
#   bestsplit <- c(0,Inf)
#   for(i in 1:length(splitpoints)){
#     
#     temp <- impurity(y[])
#     if(temp < bestsplit[2])
#     {
#       bestsplit <- c(splitpoints[i], temp);
#     }
#   }
#   
#   return (bestsplit[1]);


optimalsplit(credit.dat[,4],credit.dat[,6])


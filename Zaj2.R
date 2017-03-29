library("MCDA")
library("OutrankingTools")
library("topsis")

TOPSISVector <- function(decision, #matrix with all the alternatives
                         weights,  #vector with the numeric values of the weights
                         cb        #vector with the "type" of the criteria (benefit = "max", cost = "min")
)
{
  #Checking the arguments
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  if(sum(weights) != 1)
    stop("The sum of 'weights' is not equal to 1")
  if(! is.character(cb))
    stop("'cb' must be a character vector with the type of the criteria")
  if(! all(cb == "max" | cb == "min"))
    stop("'cb' should contain only 'max' or 'min'")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(length(cb) != ncol(decision))
    stop("length of 'cb' does not match the number of the criteria")
  
  
  #TOPSIS method
  
  #1. Normalization and weighting
  d = sqrt(colSums(decision^2))
  NW <- matrix(nrow = nrow(decision), ncol = ncol(decision))
  for(j in 1:ncol(decision)){
    NW[,j] <- (decision[,j] / d[j]) * weights[j]
  }
  
  #2. Ideal solutions
  posI <- as.integer(cb == "max") * apply(NW, 2, max) +
    as.integer(cb == "min") * apply(NW, 2, min)
  negI <- as.integer(cb == "min") * apply(NW, 2, max) +
    as.integer(cb == "max") * apply(NW, 2, min)
  
  #3. Distances to the ideal solutions
  distance =function(x,y){
    sqrt(sum((x - y) ^ 2))
  }
  posDis <- apply(NW, 1, distance, posI)
  negDis <- apply(NW, 1, distance, negI)
  
  #4. R index
  R <- negDis/(negDis+posDis)
  
  #5. Rank the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), R = R, Ranking = rank(-R, ties.method= "first")))
  
}

decision <- matrix(c(276, 130, 3399, 262, 98, 2099, 196, 75, 899, 192, 98, 1399, 206, 98, 1799, 228, 111, 1199, 188, 98, 1699, 279, 87, 1999, 206, 98, 2199, 246, 98, 2099, 100, 56, 890, 210, 98, 1899, 212, 98, 1799, 361, 211, 3449, 166, 64, 1198), nrow=15, ncol=3)
weights <- c(0.3, 0.3, 0.4)
cb <- c("max","min","min")
TOPSISVector(decision, weights, cb)
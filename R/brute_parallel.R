#'@import parallel
#'@title Kanpsack Bruteforce
#'
#'@param x is a data frame represents the total items, containing the two columns w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represents the maximum weight of the container
#'@param parallel It is set to FALSE as default, if you set it to TRUE the function will be parallellized.
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export
#'
#install.packages("doParallel")
#library(parallel)
#library(doParallel)
parallel_knapsack_brute_force <- function(x,W,parallel=FALSE){
  stopifnot(is.data.frame(x) & x> 0 ,is.numeric(W))
  w <- x$w
  n <- length(w)
  v <- x$v
  no_cores <- detectCores(logical = TRUE) -1
  # making cluster by socket version
  c1 <- makeCluster(no_cores, type='PSOCK')
  mat <- parLapply(c1, 1:2^n, function(x){as.integer(intToBits(x)[1:n])})
  stopCluster(c1)
  # make all combination of the weights
  c2 <- makeCluster(no_cores, type='PSOCK')
  w1 <- simplify2array(parLapply(c2, mat,function(y){y%*%x$W}))
  stopCluster(c2)
  # make all combination of the values
  c3 <- makeCluster(no_cores, type = "PSOCK")
  v1 <- simplify2array(parLapply(c3, mat, function(y){y%*%x$v}))
  stopCluster(c3)

   
  v1[w1 > W] = 0
  max_value <- which.max(v1)
  item_id <- mat[[max_value]]
  
  items <- c(c(1:n) * item_id)
  items <- items[items > 0]
  
  return(list(value = round(max_value), elements = items))
  
}



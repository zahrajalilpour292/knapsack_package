#'Knapsack
#'@title Kanpsack Bruteforce
#'
#'@param x is a data frame represents the total items, containing the two columns w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represents the maximum weight of the container
#'
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export
#'

brute_force_knapsack <- function(x,W){
  stopifnot(is.data.frame(x) & x> 0 ,is.numeric(W),W >0)
  w <- x$w
  n <- length(w)
  v <- x$v
  max_value <- 0
  chosen_item <- c()
  len_calc <- 1:((2^n)-1)
  for(i in len_calc){
    item=which(intToBits(i)==01)
    total_weights=sum(w[item])
    total_value=sum(v[item])
    if(total_value > max_value && total_weights <= W){
      chosen_item=item
      max_value=total_value
    }
  }
  result=list("value"=(max_value),"elements"=chosen_item) 
  return (result)
  
}
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

brute_force_knapsack <- function(x, W, parallel = FALSE){
  stopifnot(is.data.frame(x) & x> 0 ,is.numeric(W),W >0)
  
  if(parallel){
    
  }else{
    #browser()
    # separating the weights, values
    items_weight <- x$w
    items_value <- x$v
    total_items <- length(items_weight)
    
    # items included in knapsack, their total value
    max_value <- 0
    # index of the items in the knapsack
    chosen_item <- c()
    # all possible combinations of the total items
    calc_combinations <- 1:((2^total_items)-1)
    
    for(i in calc_combinations){
      
      item <- which(intToBits(i) == 01)
      
      total_weights <- sum(items_weight[item])
      total_value <- sum(items_value[item])
      
      if(total_value > max_value && total_weights <= W){
        
        chosen_item <- item
        max_value <- total_value
      
        }
    }
    result=list("value"=round(max_value),"elements"=chosen_item) 
    return (result)
  }
 
}

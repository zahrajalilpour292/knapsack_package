knapsack_brute_force <- function(x,W){
  stopifnot(is.data.frame(x) & x> 0 ,is.numeric(W))
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
  result=list("value"=(max_value),"items"=chosen_item) 
  return (result)
  
}
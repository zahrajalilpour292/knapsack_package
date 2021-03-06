#'Knapsack
#'@title Kanpsack Bruteforce
#'@description This is a brute force algorthm which check all possible combinations
#'@param x is a data frame represents the total items, containing the two columns w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represents the maximum weight of the container
#'@param parallel It's for the code to run on multiple cores of a computer to excute the code faster
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export
#'

brute_force_knapsack <- function(x, W, parallel = FALSE){
  # 
  stopifnot(is.data.frame(x) & x> 0,
            is.numeric(W),
            W >0,
            is.logical(parallel)
  )
  
  # separating the weights, values
  items_weight <- x$w
  items_value <- x$v
  total_items <- length(items_weight)
  result <- list()
  
  if(parallel){
    
    # add parallel code inside this block
    if(Sys.info()["sysname"][[1]] %in% c("Linux", "Darwin", "Windows")){
      
      f <- function(i){
        item <- which(as.logical(intToBits(i)))
        total_weights <- sum(items_weight[item])
        total_value <- sum(items_value[item])
        
        if(total_weights <= W){
          return(total_value)}
        else{
          return(0)
        }
        
      }
      # detect the cores
      no_of_cores <- parallel::detectCores()
      my_cluster <- parallel::makeCluster(no_of_cores, type="PSOCK")
      # import the data fram x and weight W in cluster
      parallel::clusterExport(cl = my_cluster,
                              c("x", "W","f"),
                              envir = environment()
      )
      
      comb <-  parallel::parSapply(my_cluster,
                                   0:(2^(total_items)-1),
                                   f)
      max_value <- max(comb)
      chosen_item <- which(as.logical(intToBits(which(comb == max_value)[1]-1)))

    }else{
      print("Brute Force Knapsack does not suported this system for the parrlel version of code ):-")
    }
    
    
  }else{
    
    max_value <- 0
    chosen_item <- c()
    # all possible combinations of the total items
    calc_combinations <- 1:((2^total_items)-1)
    
    for(i in calc_combinations){
      
      item <- which(intToBits(i) == 01)
      
      total_weights <- sum(items_weight[item])
      total_value <- sum(items_value[item])
      
      if(total_value > max_value && total_weights <= W){
        
        chosen_item <- item
        max_value <- total_value}
      
    }
  }
  result <- list("value"=round(max_value),"elements"=chosen_item) 
  
  #returning the selected elements in knapsack and their toatl value
  return (result)
  
}


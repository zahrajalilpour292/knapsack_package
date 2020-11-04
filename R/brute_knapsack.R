#'Knapsack
#'@title Kanpsack Bruteforce
#'
#'@param x is a data frame represents the total items, containing the two columns w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represents the maximum weight of the container
#'@param parallel It's for the code to run on multiple cores or single
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export
#'

brute_force_knapsack <- function(x, W, parallel = FALSE){
  # 
  stopifnot(is.data.frame(x) & x> 0,
            is.numeric(W),
            W >0
            )
  
  # separating the weights, values
  items_weight <- x$w
  items_value <- x$v
  total_items <- length(items_weight)
  result <- list()
  
  if(parallel){
    
    # add parallel code inside this block
    if(Sys.info()["sysname"][[1]] %in% c("Linux", "Darwin", "Windows")){
     
      # detect the cores
      no_of_cores <- parallel::detectCores()
        my_cluster <- parallel::makeCluster(no_of_cores)
      # import the data fram x and weight W in cluster
      parallel::clusterExport(cl = my_cluster,
                              c("x"),
                              envir = environment()
      )
      
      comb <-  parallel::parLapply(my_cluster,
                                   1:2^length(x$w),
                                   function(x){as.integer(intToBits(x)[1:total_items])})
      w1 <- simplify2array(parallel::parLapply(my_cluster, comb,function(y){y%*%x$W}))
      v1 <- simplify2array(parallel::parLapply(my_cluster, mat, function(y){y%*%x$v}))
      #browser()
      
      # brute force
      
      
      parallel::stopCluster(my_cluster)
    
      v1[w1 > W] = 0
      max_value <- which.max(v1)
      item_id <- mat[[max_value]]
      
      items <- c(c(1:n) * item_id)
      items <- items[items > 0]
      
      result <- list(value = round(max_value), elements = items)
      
      }else{
      print("Brute Force Knapsack does not suported this system for the parrlel version of code ):-")
    }
    
    
  }else{
    #browser()
    
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
    result <- list("value"=round(max_value),"elements"=chosen_item) 
  }
  #returning the selected elements in knapsack and their toatl value
  return (result)
 
}


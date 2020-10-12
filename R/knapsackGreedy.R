#'Knapsack
#'@title Kanpsack Dynamic Programming
#'
#'@param x is a data frame reprents the totla items, containing the two colums w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represnts the maximun weight of the container
#'
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export

greedy_knapsack <- function(x, W){
  
  # check the input
  stopifnot(is.data.frame(x), is.numeric(W) , length(W) == 1, W > 0)
  stopifnot(length(colnames(knapsack_objects)) == 2,
            colnames(x)[1] == "w",
            colnames(x)[2] == "v")
  
  #creating a new column in dataframe
  col_name <- "density"
  x[col_name] <- apply(x, 1, function(x) { round(x["v"] / x["w"],3)})
  #x[col_name] <- x["v"] / x["w"]
  # sort the dataframe according to the key value
  # sorts the items in decreasing order of value per unit of weight, 
  #  v{1} / w{1} .... >= ...... v{n} / w{n}
  
  x <- x[order(x[col_name], decreasing = TRUE),]
  browser()
  knapsack_weight <- 0
  knapsack_value <- 0
  knapsack_items <- vector()

  for (item in 1:nrow(x)) {
    
    #item_desity <- as.numeric(x[item ,][col_name])
    item_weight <- as.numeric(x[item ,]["w"])
    item_value <-  as.numeric(x[item ,]["v"])
    
    if(knapsack_weight + item_weight < W){
      
      knapsack_weight = knapsack_weight + item_weight
      knapsack_value = knapsack_value + item_value
      knapsack_items <- append(knapsack_items, as.numeric(row.names(x)[item])) 
      
      }else{
      
        break
    }
  }
  
  return(list("value" = round(knapsack_value),
              "elements" = knapsack_items))
}


# test data
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
)
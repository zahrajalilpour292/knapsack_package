#'
#'
#'
#'@export

greedy_knapsack <- function(x, W){
  
  # check the input
  stopifnot(is.data.frame(x), is.numeric(W) , length(W) == 1)
  stopifnot(length(colnames(knapsack_objects)) == 2,
            colnames(x)[1] == "w",
            colnames(x)[2] == "v")
  
  #creating a new column in dataframe
  col_name <- "density"
  x[col_name] <- apply(x, 1, function(x) { round(x["v"] / x["w"],3)})
  
  # sort the dataframe according to the key value
  # sorts the items in decreasing order of value per unit of weight, 
  #  v{1} / w{1} .... >= ...... v{n} / w{n}
  
  x <- x[order(x[col_name], decreasing = TRUE),]
  
  knapsack_weight <- 0
  knapsack_value <- 0
  knapsack_items <- vector()

  for (item in 1:nrow(x)) {
    
    item_desity <- as.numeric(x[item ,][col_name])
    item_weight <- as.numeric(x[item ,]["w"])
    item_value <-  as.numeric(x[item ,]["v"])
    
    if(knapsack_weight < W){
      
      knapsack_weight = knapsack_weight + item_weight
      knapsack_value = knapsack_value + item_value
      knapsack_items <- append(knapsack_items, as.numeric(row.names(x)[item])) 
      
      }else{
      
        break
    }
  }
  
  return(list("value" = knapsack_value,
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
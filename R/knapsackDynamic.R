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

knapsack_dynamic <- functionc(x, W){
  
  # check the input
  stopifnot(is.data.frame(x), is.numeric(W) , length(W) == 1)
  stopifnot(length(colnames(knapsack_objects)) == 2,
            colnames(x)[1] == "w",
            colnames(x)[2] == "v")
}


#'Knapsack
#'@title Kanpsack Dynamic Programming
#'
#'@description Fills the bag with approximatilly best weights and vlues
#'
#'@param x is a data frame reprents the totla items, containing the two colums w and v, w represents thw weight of item and v represnets the value
#'@param W is an integer that represnts the maximun weight of the container
#'
#'@return  It return the list, which the total value of item in knapsack and their position in the data frame
#'
#'@references \url{https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem}
#'@export

knapack_dynamic  <- function(x, W) {
  
  # check the input
  stopifnot(is.data.frame(x), is.numeric(W) , length(W) == 1 , W > 0)
  stopifnot(length(colnames(knapsack_objects)) == 2,
            colnames(x)[1] == "w",
            colnames(x)[2] == "v")
  
  no_items <- nrow(x)
  
  # at start its sparse, latter filled with values
  sparse_matrix <- matrix(NA, nrow = (no_items + 1) , ncol = (W + 1) )
  
  row_index <- 1
  col_index <- 1
  
  # getting colums as vectors
  weights_vector <- append(0,as.vector(x[,1])) 
  values_vector <-  append(0,as.vector(x[,2])) 
  #row_upper_bound <- 0
  #col_upper_bound <- 0
  while (row_index <= (no_items+1)) {
    while (col_index <= (W + 1)) {
      if (row_index == 1 | col_index == 1) {
        sparse_matrix[row_index,col_index] <- 0
      }else if(weights_vector[row_index] <= col_index){
        
        max_value <- max(values_vector[row_index] + sparse_matrix[row_index-1,col_index - weights_vector[row_index]],
                         sparse_matrix[row_index -1,col_index])
        sparse_matrix[row_index,col_index] <- max_value
      }else{
        sparse_matrix[row_index,col_index] <- sparse_matrix[row_index - 1, col_index]
      }   
      col_index <- col_index + 1
    }
    col_index <- 1
    row_index <- row_index + 1
  }
  
  # bracking tracking from n*m
  row <- nrow(sparse_matrix)
  col <- ncol(sparse_matrix)
  elements <- c()
  values <- 0
  browser()
  while (row > 1 & col > 1) {
    
      if(sparse_matrix[row,col] == sparse_matrix[row-1,col]){
        row <- row - 1
      }else{
        elements <- append(elements,weights_vector[row])
        values <- values + values_vector[row]
        row <- row -1
        col <- col - weights_vector[row]
      }
    
    }
  
  result <- list("value" = values,
              "elements" = elements)
  return(result)
}

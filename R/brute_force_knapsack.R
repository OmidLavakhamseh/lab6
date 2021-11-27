#' Brute force algorithm for the knapsack problem
#' @param x data.frame,with a weight w and a value v
#' @param W Knapsack capacity
#' @description takes a data.frame cx with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame)
#' @return Maximum knapsack value and which elements (row in x)
#' @details takes a data.frame cx with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame)
#' @source  https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' @export 
#' @importFrom utils combn

brute_force_knapsack <- function(x, W){
  if (is.data.frame(x) == FALSE & is.numeric(W) == FALSE){
    stop()
  }
  if(W<=0){
    stop()
  }
  i = 2
  max_value = 0     
  element <- c()
  wei <- c()
  values <- c()
  while(i <= nrow(x))
  {
    w <- as.data.frame(combn(x[,1], i))
    v <- as.data.frame(combn(x[,2], i))
    sum_w <- colSums(w)
    sum_v <- colSums(v)
    wei <- which(sum_w <= W)
    if(length(wei) != 0){ 
      values <- sum_v[wei]
      max_value <- max(values)
      val <- which((values) == max_value)
      index_weight <- wei[val]
      max_weight <- w[,index_weight]
      j <- 1
      while (j <= i){
        element[j] <- which(x[,1] == max_weight[j])
        j = j+1
      }
    }
    i = i+1
    
  }
  
  return(list(value = round(max_value),elements = element))
}

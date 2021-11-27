#' Brute force algorithm for the knapsack problem
#' @param x data.frame,with a weight w and a value v
#' @param W Knapsack capacity
#' @param parallel boolean, if 'TRUE' then implements parallelization
#' @description takes a data.frame cx with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame)
#' @return Maximum knapsack value and which elements (row in x)
#' @details takes a data.frame cx with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame)
#' @source  https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' @export 
#' @importFrom utils combn



brute_force_knapsack_parallel <- function(x, W, parallel = FALSE){
  if (is.data.frame(x) == FALSE & is.numeric(W) == FALSE){
    stop()
  }
  if(W<=0){
    stop()
  }
  v <- NULL
  rows <- nrow(x) # Rows of Dataframe
  new_indx <- c()
  row_v <- c()
  cores <- parallel::detectCores() - 1
  
  # Define Function to Calculate Weight Sum
  sum_w <- function(column,W,df){
    if (sum(column)<= W & !is.null(sum(column))){
      indx <- c(column)
      return(match(indx, df[,1]))
    }
    
  }
  # Function definition ends
  
  if (parallel ==  TRUE){
    
    # Start Cluster
    cl <- parallel::makeCluster(cores,type = "PSOCK")
    
    for (i in 1:rows){
      
      mat <- combn(x[,1],m = i)
      
      
      real_w <- parallel::parApply(cl = cl,X = mat, MARGIN = 2,
                                        FUN = sum_w,W = W,df=x)
      
      
      # Remove Nulls
      real_w <- real_w[lengths(real_w)!=0]
      
      new_indx <- c(new_indx,real_w)
      
    }
    # Stop Cluster
    parallel::stopCluster(cl)
  }
  else{
    for (i in 1:rows){
      mat <- combn(x[,1],m = i)
      real_w <- apply(mat, MARGIN = 2,FUN = sum_w,W = W,df=x)
      # Remove Nulls
      real_w <- real_w[lengths(real_w)!=0]
      
      new_indx <- c(new_indx,real_w)
      
    }
  }
  # Get v
  
  for (i in seq(new_indx)){
    
    new_v <-sum(x[new_indx[[i]],2])
    
    v <- max(new_v,v)
    
    if (v == new_v){
      row_v <- new_indx[[i]]
    }
  }
  
  result <- list(
    value = round(v),
    elements = row_v
  )
  
  return(result)
  
}


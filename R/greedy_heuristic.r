#' Implements Greedy approach for knapsack problem
#' @param x data.frame,with a weight w and a value v
#' @param W Knapsack capacity
#' @description a greedy approximation algorithm which solves the unbounded knapsack problem
#' @return Maximum knapsack value and which elements (row in x)
#' @details This algorithm will not give an exact result,it will reduce the computational complexity considerably
#' @source  https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' @export 



greedy_knapsack <- function(x, W){
  i=1
  c1=c()
  c=c()
  
  if (is.data.frame(x) == FALSE & is.numeric(W) == FALSE){
    stop()
  }
  if(W<=0){
    stop()
  }
  v=v1 = 0
  vrat = x$v/x$w
  numrow<- nrow(x) 
  x$OI= 1:numrow
  vd=vd1 = 0
  x=x[order(vrat, decreasing=TRUE),]

  while((v1<W)|(v1==W)){
    v=v1
    vd=vd1
    c=c1
    v1=v1+x$w[i]
    vd1=vd1+x$v[i]
    c1=c(c1,i)
    i=i+1
  }

  greedylist=list(value=round(vd),elements=x$OI[c])


  return(greedylist)

}



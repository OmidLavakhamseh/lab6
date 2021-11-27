## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lab6pack)

## ---- eval=F------------------------------------------------------------------
#  # Function to get time
#  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#  n <-16
#  knapsack_objects <-
#    data.frame(
#      w=sample(1:4000, size = n, replace = TRUE),
#      v=runif(n = n, 0, 10000)
#    )
#  get_time <- function(i){
#    time <- system.time(expr = brute_force_knapsack(x = knapsack_objects[1:16,], W=i))[3]
#    return(time)}
#  # Function to get mean execution time
#  mean(unlist(lapply(c(10,100,500,1000,1500,2000,3000,4000), FUN = get_time)))

## ---- eval=F------------------------------------------------------------------
#  # Data Object
#  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#  n <-500
#  knapsack_objects <-
#    data.frame(
#      w=sample(1:4000, size = n, replace = TRUE),
#      v=runif(n = n, 0, 10000)
#    )
#  
#  
#  # Function to get time
#  get_time <- function(i){
#    time <- system.time(expr = knapsack_dynamic(x = knapsack_objects[1:500,], W=i))[3]
#    return(time)}
#  # Function to get mean execution time
#  mean(unlist(lapply(c(100,500,1000,1500,2000,3000,4000,5000), FUN = get_time)))

## ---- eval=F------------------------------------------------------------------
#  # Data Object
#  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#  n <-1000000
#  knapsack_objects <-
#    data.frame(
#      w=sample(1:4000, size = n, replace = TRUE),
#      v=runif(n = n, 0, 10000)
#    )
#  
#  # Function to get time
#  get_time <- function(i){
#    time <- system.time(expr = greedy_knapsack(x = knapsack_objects[1:1000000,], W=i))[3]
#    return(time)}
#  
#  # Function to get mean execution time
#  mean(unlist(lapply(c(1000,2500,5000,10000,15000,20000), FUN = get_time)))


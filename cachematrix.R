## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL  #initial value
  
  
  #set function
  set <- function(y) {
          x <<- y
          inverse_value <<- NULL
  }
  
  #get functionto return value x
  get <- function() x
  
  
  ##set inverse value 
  set_Inverse <- function(inverse) inverse_value <<- inverse
  ##get inverse value 
  get_Inverse <- function() inverse_value
  
  #list to return all 4 values of object
  list(set = set,
       get = get,
       setInverse = set_Inverse,
       getInverse = get_Inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## set the initial value as inverse of x usinf get inverse function   
  inverse_value<- x$getInverse()
  
  #if its not NULL then return
  if (!is.null(inverse_value)) {
   
    return(inverse_value)
  }
  
  
  #get the matrix as matrix_x
  matrix_x<- x$get()
  #get the inverse 
  inverse_value <- solve(matrix_x, ...)
  #set the value to the calculated value
  x$setInverse(inverse_value)
  #returning the final value
  inverse_value
}

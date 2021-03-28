## function for the cachematrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## inv<- Null
  set <- function(y){
    x <<- y
    inv <<- NULL  #
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv  ##
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## function for the cachesolve
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()   ## inv <- x$getInverse() declares get inverse function
  if(!is.null(inv)){
    message("aquiring data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
  ## Return a matrix that is the inverse of 'x'
}
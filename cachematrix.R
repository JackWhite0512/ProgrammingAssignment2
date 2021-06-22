

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  ##inverse 
  v <- NULL
  
  ##set matrix
  set <- function(y){
    x <<- y
    v <<- NULL
  }
  
  ##get matrix
  get <- function() {
    x
  }
  ##set inverse of matrix
  setInverse <- function(solveMatrix) {
    v <<- solveMatrix
  }
  ##get inverse of matrix
  getInverse <- function() {
    v
  }
  
  ##list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  v <- x$getInverse()
  
  if(!is.null(v)){
    message("getting cached data")
    return(v)
  }
  
  data <- x$get()
  v <- solve(data)
  x$setInverse(v)
  v    

}

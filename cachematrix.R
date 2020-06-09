## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #first initialize as null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse #inversing m matrix
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getInverse() #try get the inverse if result is true then just return
  if(!is.null(y)){
    message("getting cached data")
    return(y) #no need to recompute the matrix
  }
  mat <- y$get()
  y <- solve(mat,...)
  x$setInverse(y)
  y
}

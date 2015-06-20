## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     ##setters
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     setinverse <- function(y) {
          inverse <<- y
     }
     ##getters
     get <- function() x
     getinverse <- function() inverse
     ##create caching matrix
     list(
          set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
     )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("returning cached data")
          return(inv)
     }
     mat <- x$get()
     inv = solve(mat)
     x$setinv(inv)
     inv
}

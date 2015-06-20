## create cacheMatrix object
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
     
     ##create caching matrix and return
     list(
          set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
     )
}

## solve matrix and cache if needed
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     ## if returned value is not null, it has previously been calc'd and cached
     if(!is.null(inv)) {
          message("returning cached data")
          return(inv)
     }
     ## if no cached value, get x, solve inverse, cache and return inverse
     mat <- x$get()
     inv = solve(mat)
     x$setinv(inv)
     inv
}

## Function makeCacheMatrix creates a list Vector
## Steve Bruen
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
               x <<- y
               i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }


## Function calculates the inverse of a matrix from makeCacheMatrix function.  
## Checks if the matrix has already been inverted; if so, it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}        


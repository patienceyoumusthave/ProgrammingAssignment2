## R Programming  - Assignment 2
## Jeff Stewart
## Demonstration of caching a calculation using the "<<-" operator
## 
## makeCacheMatrix - function to calculate the inverse of a matrix
##                 - requires an invertible matrix as input
##                 - returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvert <- function(solve) m <<- solve
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}



## cacheSolve - function to check for cached result, and calculate if not
##            - takes result of "makeCacheMatrix" as input
##            - checks to see if data is cached

cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

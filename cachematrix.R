## The goal of these functions is to cache the results of computing matrix
## inversions in order to save time if the inverse must be computed multiple
## times

## makeCacheMatrix is used to create an object to store a matrix and cache
## its mean. 
## the function creates a special matrix which is really a list containing a
## function to 1. set the value of the matrix, 2. get the value of the matrix
## 3. set the value of the inverse, 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Caclulates the inverse of a matrix. If the inverse has already been
## calculated before it takes the cached result instead of recalculating

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}

## This program calculates the inverse of a matrix and stores it in a cache.
## The, the program retrieves the inverse of the matrix from the cache.

## the makeCacheMatrix function is a list of 4 functions which accepts a matrix (set), 
#returns the matrix (get), stores the inverse of the matrix in a cache called m (setinverse)
#and returns the inverse of the matrix from the cache (getinverse)

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## This function returns the inverse of a matrix from a cache. 
## If the inverse is not stored in the cache, it calculates the inverse
## and stores it in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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

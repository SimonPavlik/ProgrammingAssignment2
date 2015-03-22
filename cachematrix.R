## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # reset old inverse value
    set <- function(y) {
        x <<- y # set new matrix value
        i <<- NULL # reset old inverse value
    }
    get <- function() x # retrieve matrix value
    setinverse <- function(solve) i <<- solve #calculate new inverse
    getinverse <- function() i # retrieve matrix inverse
    list(set = set, get = get, # return all 4 functions above
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() # retrieve matrix inverse of 'x'
    if(!is.null(i)) { # the inverse of 'x' has previously been calculated
        message("getting cached data")
        return(i)
    }
    data <- x$get() # retrieve matrix value
    i <- solve(data, ...) # create a solve function for a particular 'x'
    x$setinverse(i) # calculate the inverse of 'x'
    i
}
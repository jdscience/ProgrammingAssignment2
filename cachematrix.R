## These functions manage the caching of an inverted matrix.

## makeCacheMatrix provides the constructor for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(solve) {
        m <<- solve
    }
    
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    
}


## cacheSolve computes the inverse or returns it when needed.

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


## The purpose of this R package is to take a matrix and calculate the inverse of the 
## matrix. We then store that into cache to avoid processing again if the calculation 
## takes a while. If we've already processed and stored, we'll print a message and then
## show the inverse.

## This function stores the matrix into the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks the cache for the matrix, if it exists
## is is retrieved. If not it is processed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


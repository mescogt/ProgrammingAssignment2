
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set inverse matrix to NULL
        inverse_m <- NULL
        ##Set value of the matrix
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        ##Gets value of the matrix
        get <- function() x
        ##Sets value of inverse
        setInverse <- function(inverse) inverse_m <<- inverse
        ##Gets value of invers
        getInverse <- function() inverse_m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$getInverse()
        if (!is.null(inverse_m)) {
                message("getting cached data")
                return(inverse_m)
        }
        mat <- x$get()
        inverse_m <- solve(mat, ...)
        x$setInverse(inverse_m)
        inverse_m
}


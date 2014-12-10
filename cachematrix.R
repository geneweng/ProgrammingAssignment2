## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly.  Following two functions
## implements a cache mechanism for matrix inversion 

## This function creates a special "matrix" object that can cache 
## its inverse.

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


## This function computes the inverse of the special "matrix" 
## returned by  makeCacheMatrix  above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then  cacheSolve  should retrieve the inverse from the cache.
## Computing the inverse of a square matrix is done with the  
## solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting inversion from cached data...")
                return(m)
        }
        data <- x$get()
	  message("computing inversion using solve...")
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

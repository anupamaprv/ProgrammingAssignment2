## Author : Anupama Rajaram
## Date: 25-Jul 2015
## Function to Cache the Inverse of a Matrix
## 
## makeCacheMatrix => function to create a matrix object
## cachesolve => function that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL	## this is where the result of inversion is stored
        
	## the "set" function stores a matrix sent as input to makeCacheMatrix 
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x		## return input matrix
        setInverse <- function(inverse) inv <<- inverse		## set the inversed matrix
        getInverse <- function() inv		## return the inversed matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of matrix object. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# These functions cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y) {
                x<<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse<- function() inv <<- inverse
        getinverse<- function() inv
        list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
        
}

# This function returns the inverse of the matrix. First, it checks if
# the inverse has already been computed. Then, if it has been computed, the gets the result and skips the
# computing. Otherwise, the inverse is computed--the value is set in the cache using the
# setinverse function.


# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data)
        x$setinverse(inv)
        inv
        
}
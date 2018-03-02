makeCacheMatrix <- function(x = matrix()) {
##x is a matrix to be inverted 
##this returns a list of functions used as input to the cacheSolve function
##set is used to set the matrix
##get is used to get the matrix
##setinverse is used to set the matrix inverse
##getinverse is used to get the matrix inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
##check if inverse exist
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
##Below sets inverse in the cache
        x$setinverse(inv)
        inv
}
## This two functions are used to cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x_origin <- x;
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse, x_origin = x_origin)

}

## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, x0, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!identical(x0, x$x_origin)) {
                message("Original Matrix has been changed! ")
                message("Please re-do \"makeCacheMatrix\" again! ")
                return(NULL)
        } ## this is to test if the matrix has been changed or not. 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## testing code: 
# x <- matrix(1:4, nrow=2, ncol=2)
# xM <- makeCacheMatrix(x)
# cacheSolve(xM, x)
# cacheSolve(xM, x)
# x <- x+1
# cacheSolve(xM, x)


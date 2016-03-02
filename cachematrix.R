## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly. This function is able to cache the inverse of a matrix
## after the fist computation and retrieve the inverse directly from cache if it is computed again.

## makeCacheMatrix is a function to make one matrix cacheable

makeCacheMatrix <- function(x = matrix()) {
        if (nrow(x)!=ncol(x)) { 
                message("Error in solve.default(x), matrix must be square")
        }
        else {
                sol <- NULL
                set <- function(y) {
                        x <<- y
                        sol <<- NULL
                }
                get <- function() x
                setsol <- function(solve) sol <<- solve
                getsol <- function() sol
                list(set = set, get = get,
                     setsol = setsol,
                     getsol = getsol)
        }
}


## cacheSolve is a function to calculate the inverse of the matrix if it is valid by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsol()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsol(sol)
        sol
}

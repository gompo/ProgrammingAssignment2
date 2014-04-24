## The pair of functions caches the inverse of the matrix

## Return the list with functions, which set and get the value of
## the matrix, and set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	    s <- NULL
        set <- function(y = matrix()) {
                x <<- y
                s <<- NULL
        }
        get <- function() x 
        set_inverse <- function(solve) s <<- solve
                get_inverse <- function() s
        list(set = set, get = get,
             "set_inverse" = set_inverse,
             "get_inverse" = get_inverse)

}


## Return a matrix that is the inverse of 'x' and describe what to
## do, when we have cache inverse matrix and when we haven't one

cacheSolve <- function(x, ...) {
        s <- x$get_inverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$set_inverse(s)
        s
        
}

## Functions of this script provide an efficient way to calculate
## reverse matrix using cache

## makeCacheMatrix returns a list containing functions to set, get
## the value of the matrix, the inverse matrix and the matrix which 
## inverse was calculated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    b <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    set_base <- function(base) b <<- base
    get_base <- function() b
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse,
         set_base = set_base,
         get_base = get_base)
}


## cacheSolve checks if matrix didn't change and the cache 
## is available, if both true, then value from cache is returned,
## otherwise inverse matrix is recalculated and saved to cache

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv) && !is.null(x$get_base()) && identical(x$get_base(), x$get())) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$set_inverse(inv)
    x$set_base(matrix)
    inv
}
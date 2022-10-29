## Functions of this script provide an efficient way to calculate
## reverse matrix using cache

## makeCacheMatrix returns a list containing functions to set, get
## the value of the matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve checks if the cache is available,
## if it is true, then value from cache is returned,
## otherwise inverse matrix is calculated and saved to cache

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$set_inverse(inv)
    inv
}
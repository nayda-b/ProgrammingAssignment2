## My makeCacheMatrix function was created to set and get the value of a matrix
## and to set and get the value of the inverse

#Sorry for my bad english ;)


makeCacheMatrix <- function(x = matrix()) {
    # the inverse matrix is initialized to null
    inv <- NULL

    # the value of the matrix is set
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    # the following step sets the value of the inverse
    set_inverse <- function(inv_input) inv <<- inv_input
    # now we get the value of the inverse
    get_inverse <- function() inv
    
    # all the functions are returned as a list
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## this function verifies if the inverse for the matrix was already calculated
## otherwise it calculates it

cacheSolve <- function(x, ...) {
    # check if the inverse is in the cache and uses it
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("the inverse from the cache will be used")
        return(inv)
    }
    # we calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)

    inv
}

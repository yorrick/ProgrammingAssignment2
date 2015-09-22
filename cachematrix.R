## cacheSolve return the inverse of the given matrix, using a cache
## cacheSolve is designed to be used with special matrix objects, that can
## be built using makeCacheMatrix function


## This function builds a list object encapsulating a matrix and 
## its accessor / modifiers functions.
## Example: 
##    makeCacheMatrix(rbind(c(1, 2), c(3, 4)))
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set_matrix <- function(y) {
        x <<- y
        inverse <<- NULL  # do not forget to re-initialize inverse as well
    }
    get_matrix <- function() x
    
    set_inverse <- function(i) { inverse <<- i }
    get_inverse <- function() inverse
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse, get_inverse = get_inverse)
}


## Returns matrix inverse, using cached value if available, computing
## and updating cache if not.
## Example: 
##   m <- makeCacheMatrix(rbind(c(1, 2), c(3, 4)))
##   i <- cacheSolve(m)
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get_matrix()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    
    inverse
}

## These functions allow the inverse of a matrix to be cached to a special 
## "matrix" object to avoid recalculation of the inverse at a later stage.

## Creates a "matrix" list object containing functions for storing a matrix, x, 
## retrieving x and setting and retrieving the inverse of x. When x is set, the
## inverse of x is reset.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(xinv) inv <<- xinv
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks the "matrix" list object to see if the inverse has been cached; if so, 
## returns cached inverse; if not, inverse is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

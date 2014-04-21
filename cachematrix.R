## This function has the following properties.
##      1. it takes an argument x of type numeric matrix
##      2. it returns a list with 4 list items

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) m <<- solve
        getinvmatrix <- function()                         
        list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## This function is a client function that uses the "makeCacheMatrix" function in its implementation.
##      1. it takes an argument x computed via the makeCachematrix function.
##      2. it returns the inverse matrix, either by accessing the cache or through computing.

cacheSolve <- function(x, ...) {

        m <- x$getinvmatrix()                   # Queries the x's cache   
        if(!is.null(m)) {                       # Checks if value has already been computed
                message("getting cached data")  # Displays message in case it has been computed
                return(m)                       # Returns previously computed value
        }
        data <- x$get()                         # If there's no cache value
        m <- solve(data, ...)                   # Calculates inverse matrix in case it hasn't been computed yet.
        x$setinvmatrix(m)                       # Saves the result back to x's cache
        m                                       # Returns result
}
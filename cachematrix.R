## The script contains two functions
## makeCacheMatrix is a function that returns a list of functions
## cacheSolve is a function that returns the inverse of a matrix
## from the cache if available or uses the solve function to return the value

## Returns a list of getter and setter functions for the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
# store the inverse of the matrix
        minverse <- NULL
        # getter and setter for the matrix
        set <- function(y){
                x <<- y
                minverse <- NULL
        }
        
        get <- function() x
        
        # getter and setter for the inverse
        setminv <- function(inverse) minverse <<- inverse
        getminv <- function() minverse
        
        # return list of functions
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## Returns the inverse of a matrix. If inverse is available in the cache
## return from cache or calculate the inverse using solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minverse <- x$getminv()
        # return if inverse is in the cache
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        # inverse not cached use solve to get the inverse
        data <- x$get()
        minverse <- solve(data, ...)
        # store inverse in cache
        x$setminv(minverse)
        # return value
        minverse
}

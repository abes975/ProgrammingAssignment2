## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is an opaque handle for a matrix object
## It will have a setter and a getter method and
## then an inverse metod in oder to get inverse matrix
## inverse method will call cacheSolve to save time
## when matrix has already been calculated.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # Setter  for x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #setter for inverse
    setinverse <- function(y) {
        inv <<- y
    }
    
    # Don't like to write one liner functions.. here'sr the gettgers
    get <- function() {
        x
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get,
            setinverse= setinverse,
            getinverse = getinverse)                  
}

## Write a xshort comment describing this function
## Check if we have already calculated inverse
## matrix...in this case we will return cached copy
## othewise will calculate it again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        } 
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return(inv)
} 

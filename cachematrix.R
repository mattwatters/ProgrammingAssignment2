## Matthew Watters - 27 April 2014
##
## The following functions will allow creation of a custom matrix object that 
## allows caching of its inverse, rahter than re-calculating it every time it is
## needed.


## Creates a new custom matrix object which is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialise inversed matrix
    inversedMatrix <- NULL
    
    # set stored matrix to new value and re-initialise the cached inverse
    set <- function(y) {
        x <<- y
        inversedMatrix <<- NULL
    }
    
    # return stored matrix
    get <- function() x
    
    # set the inverse of the stored matrix
    setInverse <- function(inverse) inversedMatrix <<- inverse
    
    # return inverse of the stored matrix
    getInverse <- function() inversedMatrix
    
    # methods within makeCacheMatrix
    list(set = set
       , get = get
       , setInverse = setInverse
       , getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Attempt to fetch cached inverse
    inversedMatrix <- x$getInverse()
    
    # If inverse has been previously calculated, return inversed matrix
    if(!is.null(inversedMatrix)) {
        message("getting cached inverse")
        return(inversedMatrix)
    }
    
    # If not, obtain stored matrix to calculate its inverse
    matrix <- x$get()
    
    # Calculate the inverse
    inversedMatrix <- solve(x, ...)
    
    # Cache the calculated inverse so it can be fetched when it's next required
    x$setInverse(inversedMatrix)
    
    # Return the calculated inverse
    inversedMatrix
}

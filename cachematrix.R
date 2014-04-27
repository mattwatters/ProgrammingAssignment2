## Matthew Watters - 27 April 2014
##
## The following functions will allow creation of a custom matrix object that 
## allows caching of its inverse, rahter than re-calculating it every time it is
## needed.


## Creates a new custom matrix object which is able to cache its inverse.

makeCacheMatrix <- function(storedMatrix = matrix()) {
    # initialise inversed matrix
    inversedMatrix <- NULL
    
    # set stored matrix to new value and re-initialise the cached inverse
    set <- function(newMatrix) {
        storedMatrix <<- newMatrix
        inversedMatrix <<- NULL
    }
    
    # return stored matrix
    get <- function() storedMatrix
    
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
        ## Return a matrix that is the inverse of 'x'
}

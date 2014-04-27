## Matthew Watters - 27 April 2014
##
## The following functions will allow creation of a custom matrix object that 
## allows caching of its inverse, rahter than re-calculating it every time it is
## needed.


## Create a new custom matrix object which is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialise the cached inversed matrix
    inversedMatrix <- NULL
    
    # Set the stored matrix to the supplied matrix and re-initialise the cached
    # inverse
    set <- function(y) {
        x <<- y
        inversedMatrix <<- NULL
    }
    
    # Return the stored matrix
    get <- function() x
    
    # Set the inverse of the stored matrix
    setInverse <- function(inverse) inversedMatrix <<- inverse
    
    # Return the inverse of the stored matrix
    getInverse <- function() inversedMatrix
    
    # List of the functions within makeCacheMatrix
    list(set = set
       , get = get
       , setInverse = setInverse
       , getInverse = getInverse)
}


## Calculate the inverse of the matrix. If the inverse has already been 
## calculated then it will fetch the cached result.
cacheSolve <- function(x, ...) {
    
    # Attempt to fetch cached inverse
    inverse <- x$getInverse()
    
    # If inverse has been previously calculated, return inversed matrix
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    # If not, obtain stored matrix to calculate its inverse
    matrix <- x$get()
    
    # Calculate the inverse
    inverse <- solve(matrix, ...)
    
    # Cache the calculated inverse so it can be fetched when it's next required
    x$setInverse(inverse)
    
    # Return the calculated inverse
    inverse
}

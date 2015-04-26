# Here, we have two functions that when used together, enable to cache 
# matrix inversion computation potentially time-consuming. 

# First create a "Matrix"-vector with makeCacheMatrix(). 
# The latter takes a matrix as argument and returns matrix value and caches its inverse. 
# Second, to compute the inverse of the matrix, use the cacheSolve() function.  
# It takes as argument a "matrix" created with makeCacheMatrix(). 
#     It checks if the inverse has already been calculated. If so, it gets the 
#     inverse from the cache and skips the computation. If not, it calculates 
#     the inverse and sets the value in the cache. 


makeCacheMatrix <- function(x = matrix()) {
# creates a "matrix" which is a list of functions
        matrix.inverse <- NULL
         # Initialize the inverse matrix to NULL when object is created
        set <- function(passed.value) {
                x  <<- passed.value
                matrix.inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(passed.inverse) matrix.inverse <<- passed.inverse
        getInverse <- function() matrix.inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        matrix.inverse <- x$getInverse()
        if(!is.null(matrix.inverse)) {      # inverse is already present in x 
                message("getting cached data")    # return the stored value 
                return(matrix.inverse) 
        } 
        data <- x$get() 
        matrix.inverse <- passed.inverse(data, ...)
        x$setInverse(matrix.inverse)
        matrix.inverse
}

#########################################################################
#
#  This function creates four functions for setting and reading the
#  cached Matrix and Matrix Inversion.
#
#########################################################################

makeCacheMatrix <- function(x = matrix()) {

        # Create a null "solved" matrix - s will hold the inverse matrix.
    
        s <- NULL
        
        # Set - store the Matrix and nullify the "solved" matrix.
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }

        # Get: Returns the Matrix
        
        get <- function() x
        
        # setsolve: Store away the inverse matrix in the original environment.
        
        setsolve <- function(solve) s <<- solve
        
        # getsolve: Retrieve the inverse matrix.
        
        getsolve <- function() s
        
        # The output of makeCacheMatrix is a list of the necessary functions.
        # to perform this caching function.
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



#########################################################################
#
#  This function reads the cached inverse matrix to make sure it is 
#  set.  if it is set, then it returns the value.  If it is not set,
#  then it retrieves the matrix, computes the inverse, then caches the
#  result.
#
#########################################################################

cacheSolve <- function(x, ...) {
    
        # Fetch the cached value
    
        s <- x$getsolve()
    
        # If it's NULL, tell the user we are returning the cached value.
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # Otherwise, fetch the cached matrix.
        
        data <- x$get()
        
        # Calculate the inversion of the matrix.
        
        s <- solve(data, ...)
        
        # Store the new value in the cache.
        
        x$setsolve(s)
        
        # Return the inversion to the caller.
        
        s
}

############################################
## Randy Oswald                            #
## R Programming - Coursera                #
## 7-22-2014                               #
############################################

# Create a matrix 'object' that is capable of caching the result.
makeCacheMatrix <- function(x = matrix()) {
    # Inverse member variable - start NULL until we cache an inverse.
    inverse <- NULL
    
    # Set a new matrix member function.
    set <- function(y) {
        x <<- y
        # Clear cached inverse if we set cached matrix to something new.
        inverse <<- NULL    
    }
    
    # Get the matrix member function.
    get <- function() { 
        x 
    }
    
    # set the inverse of the matrix (after solving).
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    # Get the matrix member function.
    getInverse <- function() {
        inverse
    }
    
    # Assign and return a list of the functions.
    list(set = set
         , get = get
         , setInverse = setInverse
         , getInverse = getInverse
    )  
}


## Solving routine that takes in a "matrix object" created by makeCacheMatrix.
cacheSolve <- function(x, mat) {
    # Check if we have cached an inverse (it would be non-NULL)
    inverse <- x$getInverse()
    
    # If we have a cached inverse, print message and return the inverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # If no cached inverse is available, we have to solve.
    data <- x$get()         # Get original matrix from object
    inverse <- solve(data)  # Solve
    x$setInverse(inverse)   # Don't forget to cache result for next time!
    inverse                 # Return inverse.
}

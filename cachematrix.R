############################################
## Randy Oswald                            #
## R Programming - Coursera                #
## 7-22-2014                               #
############################################

# Create a cache'd matrix 'object'
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # Clear cached inverse if we set cached matrix to something new.  
  }
  get <- function() { 
    x 
  }
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function() {
    inverse
  }
  list(set = set
       , get = get
       , setInverse = setInverse
       , getInverse = getInverse
       )  
}


## Write a short comment describing this function

cacheSolve <- function(x, mat) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse 
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return a list of all the functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If inverse already exists, return it with a message
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Use solve() to compute inverse
  x$setinverse(inv)        # Cache the inverse
  inv
}

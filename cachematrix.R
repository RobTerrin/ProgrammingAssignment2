## Rob Terrin's Second Programming Assignment using the <<- operator, matrix inversion and
## environment caching. 5/30/2016

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      ## Temp variable for returning the matrix
      m <- NULL
      
      ## This function sets the given matrix to the environment cache & clears the return variable
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## This function gets the cache
      get <- function() x
      
      ## This function sets the inverse matrix
      setinverse <- function(solve) m <<- solve
      
      ## This function gets the inverse function
      getinverse <- function() m
      
      ## Store the functions in a list
      list(set= set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated,
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
      ## Gets the inverse of x cache 
      m <- x$getinverse()
      
      ## Checks if the inverse of x cache is empty
      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

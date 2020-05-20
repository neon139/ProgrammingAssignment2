## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { ## setting the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x  ##returns the matrix
  setinverse <- function(inverse) s <<- inverse ##sets the inverse of a matrix
  getinverse <- function() s ##returns the inverse of a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse() ##calls the getinverse function
  if(!is.null(s)) {  ##checks if s is not NULL
    message("getting cached data") 
    return(s)  ## returns the cached data
  }
  data <- x$get()
  s <- solve(data, ...)  ##finds the inverse 
  x$setinverse(s)
  s
}

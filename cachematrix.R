## Saves the inverse of a matrix in the cache. 
## If the inverse already exists it retrieves it from the cache

## makeCacheMatrix() initializes get and set functions to be used with a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## Looks if the inverse of x already exists in the cache
## otherwise creates one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("creating an inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

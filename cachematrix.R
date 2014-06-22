## Creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
 set <- function(y) {
           x <<- y
           m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Computes the inverse of matrix object returned by makeCacheMatrix
## If inverse has been calculated, it is retrieved from the cache
cacheSolve <- function(x = matrix(), ...) {
 ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
      }
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setinverse(m)
      m
}

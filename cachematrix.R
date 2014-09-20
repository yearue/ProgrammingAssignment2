## makeCacheMatrix creates a matrix object that can cache its inverse

  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,       
         setinverse = setinverse,
         getinverse = getinverse)
  }
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## if the inverse has been calculated, it will be retrieved from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Matrix inversion is usually a costly computation and there may be some benefits
## to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
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

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      ## Solve takes two required elements, a and b where a is the matrix
      ## we want to take the inverse of, and b is the identity matrix.
      ## The identity matrix is a square matrix with 1's down the diagonal
      ## from the top left to the bottom right and zeros everywhere else.
      ## This can be attained by using the diag function.
      m <- solve(data, diag(nrow(data)), ...)
      x$setinverse(m)
      m
}

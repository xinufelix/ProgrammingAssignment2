## Create a matrix that can cache its inverse.
##
## example
## cacheMatrix <- makeCacheMatrix()
## cacheMatrix$set(matrix(c(1,1,-1,1),nrow=2,ncol=2))
makeCacheMatrix <- function (x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Solve for a matrix that is the inverse of 'x'. 
## 
## If the inverse for 'x' has already been calculated, then return 
## the cached inverse.
##
## example
## cacheMatrix <- makeCacheMatrix(matrix(c(1,1,-1,1),nrow=2,ncol=2))
## cacheSolve(cacheMatrix)
## cacheSolve(cacheMatrix)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if (!is.null(inverseMatrix)) { 
    message("getting cached data") 
    return(inverseMatrix) 
  } 
  data <- x$get() 
  inverseMatrix <- solve(data)
  ## Cache the solution in 'x' to avoid recomputing.
  x$setinverse(inverseMatrix) 
  inverseMatrix
}

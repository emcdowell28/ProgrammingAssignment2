## makeCacheMatrix initializes the functions we'll apply to a matrix
## which is then stored to cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver <- ginv(x)  ## using library(MASS) this finds and assigns
    ## the inverse of x matrix, similar to solve(x), but for square
    ## and non-square matrices.
    inver%*%x  ## multiplies the inverse matrix by the original matrix
    ## if they are conformable.
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  ## prints list showing call functions and
  ## their environments
}


## This function solves for the inverse of the matrix using the
## initializations created in the previous function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {  ## verifies that the inverse isn't NULL
    message("getting cached data")
    return(inv)   ## prints the inverse using x$getinv()
  }
  data <- x$get()
  i <- solve(data) ## solves for the inverse of the matrix
  x$setinv(inv)  ## stores the inverse to cache
  inv
}

x <- makeCacheMatrix(matrix(1:4, 2, 2))  ## basic 2x2 matrix
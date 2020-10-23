## The following pair of functions compute matrix inversion with
## the aid of a cache process to reduce the number of redundant
## computations. First, a matrix is used as input for makeCacheMatrix
## and special object (a list of functions) is created. The use of
## cacheSolve on the makeCacheMatris return object, computes or
## retrieves from cache the inverted matrix.
## Usage:
## a <- makeCacheMatrix(inputmatrix)
## cacheSolve(a)


## makeCacheMatrix uses a matrix to create a list of 4 functions
## (get, set, getinverse, setinverse) to get and set the value of
## the input matrix, and to get and set the corresponding inverse
## matrix. This function does not check is a matrix is invertible.
## When using getinverse on a matrix which has not yet inverted,
## NULL is returned.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix provided in the form of
## a makeCacheMatrix object. If the inverse matrix has been already
## computed, it returns the cache value. If it has not been calculated
## or the matrix has been modified after inversion, it calculates the
## inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
## Put comments here that give an overall description of what your
## functions do

## The following function (makeCacheMatrix) serves as the storage area for calculated matrices.
## These matrices are stored inside the "list" function insdie makeCacheMatrix and can be called upon
## by the cacheSolve function at anytime.

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## The following function (cacheSolve) helps to calculate and cache the inverse of matrices.
## The function will first check if the follwing matrix's inverse has been calculated before, and if so
## will obtain the result from the cache without having to calculate the actual inverse. If it is not
## contained in the cache it will then calculate the inverse of the matrix and store it inside
## the cache, which can be found in the makeCacheMatrix function.

cacheSolve <- function(x, ...)
{
  m <- x$getmatrix()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}

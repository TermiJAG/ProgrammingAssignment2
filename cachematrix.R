## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates the matrix and the list of necessary functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    #Sets a new matrix and deletes the inverse value that might be stored in Cache
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve takes the matrix and checks if the inverse of the Matrix
## is already stored in cache and returns the cached data.
## If the result is not in cache, the inverse matrix is calculated, stored in cache
## and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) { 
    ## Data is already in Cache
    message("getting cached data")
    return(i)
  }
  ##Data is not in cache yet =>Calculate inverse, store and return.
  data <- x$get()
  i <- solve(data)
  x$setInv(i)
  i
}

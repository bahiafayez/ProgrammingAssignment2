# The following two functions are for getting the inverse
# of a matrix with better performance.
# The idea is, if the inverse is already calculated for
# a matrix m, and that matrix has not changed, then if I cache
# that inverse, I can just retrieve it again without recalculating


## makeCacheMatrix creates a 'special' matrix by adding
## 4 functions that enable getting, setting the matrix and
## getting and setting the inverse of the matrix
## When calculating the inverse of a matrix, we will use
## this function to get from the cache or set the cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # The set function sets the value of the matrix (x) by using <<-
  # to set it in the defining environment. It sets the inverse to NULL since the
  # matrix has changed, so the old inverse(if any) is no longer valid.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #returns the matrix
  get <- function() x
  # sets the inverse
  setinverse <- function(inv) inverse <<- inv
  # returns the inverse
  getinverse <- function() inverse
  
  # when you call makeCacheMatrix it will return those 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of
## a matrix but the input is not a matrix, it is the 'special' 
## matrix created using makeCacheMatrix. This function will 
## check the cache first, if it finds the inverse it will
## return it as is, otherwise it will calculate is using
## the solve() function and set the cache so that next time
## we won't recalculate it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  # if inverse found in cache, then will return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise, will get the matrix, calculate the inverse, save it in the cache
  # Then return that result.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

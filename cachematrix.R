#
# Sample usage:
# > m <- rbind(c(1, -1/4), c(-1/4, 1))
# > c <- makeCacheMatrix(m)
# > cacheSolve(c) # solve matrix for the first time
# > cacheSolve(c) # get inverse from cache
#

## function that cache the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # the cached inverse value
  inverse <- NULL
  
  # assign a new matrix and clearing inverse value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # set inverse value after calculation for the first time
  setCache <- function(i) inverse <<- i
  
  # get cache inverse value
  getCache <- function() inverse
  
  # print available functions
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## use this function to execute makeCacheMatrix
## Return a matrix that is the inverse of 'x

cacheSolve <- function(x, ...) {
  
  # get inverse from cache if exist
  inverse <- x$getCache()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if cache doesn't exist:
  # get inverse for the first time
  data <- x$get()
  
  # calculate inverse
  inverse <- solve(data, ...)
  
  # save to cache
  x$setCache(inverse)
  
  # return result
  inverse
}

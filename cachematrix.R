## Overall function description makeCacheMatrix
## This function sets a matrix (x), gets its value  and  
## then sets the value of the inverse of x and gets its value.
## It caches the value. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function firstly checks whether the inverse of x has 
## been computed. If so, it gets it from cache and skips computation. 
## Otherwise, it calculates the inverse of x (using Solve()) 
## and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}

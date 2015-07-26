## These functions cache inverse of a matrix. For time consuming 
#repeated computations, it is better to cache the value once found, so that it 
##can be used again if needed without actually computing again.

## The below function creates a special matrix and is a list containing four functions 
##  to set and get the value of matrix and set and get the inverse of the matrix.

makeCacheMatrix<- function(x) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setin <- function(solve) i <<- solve
  getin <- function() i
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}


## This function computes the inverse of the special vector created above and
##displays the result after checking whether inverse has already been
##calculated. if it has already been done, it simply displays that data and 
##avoids computation. Otherwise it calculates the inverse and sets the value of
##inverse in the cache using setin function.
cacheSolve <- function(x, ...) {
  i <- x$getin()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setin(i)
  i
}

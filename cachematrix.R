so## The given functions aim to cache the inverse of a matrix when it is made
## instead of computing it repeatedly. This makes matrix inversion less 
## resource and time intensive.

## Here makeCacheMatrix primarily dooes 4 tasks via a list that contains the function to do them:
## set the matrix' value
## get the said value
## set the value of the matrix' inverse
## get the said value

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## This function assumes that the matrix is always invertible and then checks
## if its inverse has been computed. If yes, it returns the result directly.
## If not, it computes the inverse and then sets the value in the cache via 
## setInverse function. NOTE: On some versions of RStudio the following function
## shows a coercion error.

cacheSolve <- function(x, ...) {
  a <- (x$getInverse())
  if(!is.null(a)){
    message("showing cached data")
    return(a)
  }
  data <- (x$get())
  a <- solve(data)
  (x$setInverse(a))
  a
}





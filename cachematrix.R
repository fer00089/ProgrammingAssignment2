## Put comments here that give an overall description of what your
## functions do

##THe first function takes a matrix as an input and caches it, while the second 
##one is in charge of checking the cache and if its a new matrix it returns the 
##inverse of it, whilst if the matrix is in the cache it only returns the
##previously computed inverse

## Write a short comment describing this function
##this function sets and gets the value of the matrix and sets and gets the 
##value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##this function firsts gets de inverse value from the cache, if it already exists
##it returns it with a message, if its null, it calculates the matrix inverse
#and saves it on the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y){
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinv <- function(slv) m1 <<- slv
  getinv <- function() m1
  list(set = set, get = get,
   setinv = setinv,
   getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m1 <- x$getinv()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data)
  x$setinv(m1)
  m1
}

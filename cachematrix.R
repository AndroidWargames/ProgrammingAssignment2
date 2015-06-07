# These functions provide a means of creating and then caching
# the inverse of an invertible, square matrix


# This function outputs a list of functions that,
# when read by the cacheSolve function, return
# an inverted matrix

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


# This function either computes the inverse of the matrix
# previously inputted into makeCacheMatrix, or returns
# the cached value of that matrix if it has already been
# computed

cacheSolve <- function(x) {
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

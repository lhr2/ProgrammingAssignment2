##this function create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##sets inverse matrix to null
  invmatrix <- NULL
  
  ##sets the matrix value
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  ##gets the matrix value
  get <- function() x
  
  ##sets the inverse matrix value
  setSolve <- function(solve) invmatrix <<- solve
  
  ##gets the inverse matrix value
  getSolve <- function() matrix
  
  ##returns results in list format
  list(set = set, get = get, setSolve = setSolve, getSOlve = getSolve)
}


##this function returns an inverse matrix of 'x'
##by pulling it if it has already been solved
##or calculating it otherwise
cacheSolve <- function(x = matrix(), ...) {
  
  ##look for already solved inverse matrix
  invmatrix <- x$getSolve()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  
  ##if the matrix has not already been inverted, calculate inverse
  data <- x$get()
  
  ##inverse being calculated
  invmatrix <- solve(data, ...)
  
  ##set the cache for later use
  x$setSolve(invmatrix)
  
  
  ##return the inverse
  invmatrix
}
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  } 
  setinvmat <- function(inverse){
    m<<-inverse
  }  
  
  ##setting inverse matrix to this environment
  
  
  getinvmat <- function() {
    m

  }
  ##returns the inverse matrix
  
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}  


cacheSolve <- function(x, ...) {
  m <- x$getinvmat()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmat(m)
  m
}


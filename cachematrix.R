## These functions find the inverse of a matrix,
## using a cached value if availible

## makeCacheMatrix creates an list of functions 
## and returns these to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve takes an object of type makeCacheMatrix
## and calculates the matrix inverse if it doesn't already
## exist as a cached object.  If it exists it returns the
## cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

B = matrix( c(2, 70, 3, 2), nrow=2, ncol=2)
amatrix <- makeCacheMatrix(B)
cacheSolve(amatrix)


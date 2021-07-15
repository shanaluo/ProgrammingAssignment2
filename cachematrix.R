# These two functions, when used together, will allow you to cache
# the time-consuming computation of calculating the inverse of a
# matrix. These functions take advantage of lexical-scoping rules
# in R to preserve the state inside an R object.

# makeCacheMatrix creates a list of four functions that will allow
# the user to 
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

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


# cacheSolve calculates the inverse of the list created with
# makeCacheMatrix. It will first check and retrieve a cached 
# inverse if one is available. Otherwise, it will calculate 
# the inverse of the matrix, caching the value of that inverse
# to avoid repeating the time-consuming calculation if called 
# again.

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

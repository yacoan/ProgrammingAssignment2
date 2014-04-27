# This function creates a special "matrix" object that can cache its inverse.
# It returns a list with 4 items(set, get, setinverse and getinverse)

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated then cachesolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()   #query the x vector's cache
  if(!is.null(m)) {     #if there is a cache
    message("getting cached data")
    return(m)           # return the cache, no computation needed
  }
  data <- x$get()       #continue if there's no cache
  m <- solve(data, ...) # compute inverse matrix
  x$setInverse(m)       #save the result back to x's cache
  m                     #return the result
}


#The first function, makeVector creates a special "vector", which is really a list containing a function to:
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmeaninverse <- function(solve) m <<- solve
  getmeaninverse <- function() m
  list(set = set, get = get,
       setmeaninverse = setinverse,
       getmeaninverse = getinverse)
}

cachemean <- function(x, ...) {
  m <- x$getmeaninverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(m)
  m
}

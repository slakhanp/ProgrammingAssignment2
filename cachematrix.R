# Caching the Inverse of Matrix will speed the process. usually inverse of Matrix is very slow process.
# Caching will help speed it up. The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

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


# CacheSolve function returns the inverse of the matrix. It checks if inerse is already calculated 
# and gets results from cache else it computes and stores in cache.

cacheSolve <- function(x, ...) {
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

# Sample steps to see result
# > m<-makeCacheMatrix()
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > m$set(matrix(c(9,3,3,11),2,2))
# > cacheSolve(m)
# [,1]        [,2]
# [1,]  0.12222222 -0.03333333
# [2,] -0.03333333  0.10000000
# > cacheSolve(m)
# getting cached data
# [,1]        [,2]
# [1,]  0.12222222 -0.03333333
# [2,] -0.03333333  0.10000000
# > 


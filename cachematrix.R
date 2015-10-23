## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions: makeCacheMatrix and cacheSolve, are used to cache the inverse of a matrix.

##  makeCacheMatrix creates a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix
## 4. get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL    # it initialises the result of the inversion to null
  set <- function(y) {   #set the matrix
    x <<- y             
    m_inv <<- NULL
  }   
  get <- function() {x}  #return the initial matrix
  setinverse <- function(inverse) { # set the inversed matrix
    m_inv <<- inverse
  }
  getinverse <- function() {m_inv} #return the inversed matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
} # #return with set, get, setinverse and getinverse functions


## cacheSolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## cacheSolve function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  m_inv <- x$getinverse() #get the inversed matrix 
  if (!is.null(m_inv)) {
    message("getting cached data.")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data,...)
  x$setinverse(m_inv)
  m_inv
}
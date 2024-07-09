# ~~~ MY ANSWERS BELOW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## This function creates a special "matrix" object that can cache its inverse.
## 'set' the value of the matrix
## 'get' the value of the matrix
## 'setI' the value of the inverse of the matrix
## 'getI' the value of the inverse of the matrix
## The 'inv' variable is used to store the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Initialize the inverse property
  
  ## The method is for setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  ## Reset the inverse property when the matrix is changed
  }
  
  ## This method is for getting the matrix
  get <- function() x
  
  ## This method is for setting the inverse
  setI <- function(inverse) inv <<- inverse
  
  ## This is for getting the inverse
  getI <- function() inv
  
  ## Returning a list
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}

q <- matrix(c(1, 2, 3, 4), 2, 2)
q

cachedMatrix <- makeCacheMatrix(q)


## Computes an inverted matrix based on the makeCacheMatrix. 
## If the inverse is already done, 
##   then this function should retrieve the inverse from the cache.

## Firstly it checks if the inverse is done and cached.
## If already cached, retrieves the inverse from the cache and returns it.
## If the inverse isn't cached, it computes the inverse using the solve function, 
##   caches the result using the setInverse function, and then returns the computed inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  ## Retrieve the cached inverse
  
  ## If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ## Return the calculated inverse
  inv
}


invMatrix <- cacheSolve(cachedMatrix)
invMatrix











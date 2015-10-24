## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1. set the matrix 2. get the matrix 3. set the inverse matrix 4. get the inverse matrix
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinverse = function(inverse) i <<- inverse 
  getinverse = function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function calculates the inverse of the special matrix created with the makecachematrix function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  i = x$getinverse()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  mat.data = x$get()
  i = solve(mat.data, ...)
  
  x$setinverse(i)
  
  return(i)
}
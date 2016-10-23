
## This code creates two functions:
## one that create a matrix object that caches its inverse
## and one that checks a matrix object for inverse
## that then computes and stores the inverse if not already solved.

## Take a matrix 'mtx' and creates a new matrix object
## capable of caching the inverse

makeCacheMatrix <- function(mtx = matrix()) {
  ## this function intializes an encapsulated value 'mtx
  ## initialize the inverse as encapsulated value 'inv'
  inv <- NULL
  ## create a function that sets a new value for matrix 'mtx'
  set <- function(y){
    mtx <<- y
    inv <<- NULL 
  }
  ## create a function that returns the stored matrix
  get <- function() mtx
  ## create a function that sets inverse
  setinv <- function(invrs) inv <<- invrs
  ## create a function that gets the inverse
  getinv <- function() inv
  ## return this new object as a list of the functions created
  ## the matrix 'mtx' and inverse 'inv' are encapsulated
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Check an existing matrix object for cached inverse
## Return cached inverse if it exists, otherwise
## compute and store inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## use the getinv() function for the matrix object
  inv <- x$getinv()
  ## check if a value is cached; if it is (i.e. not NULL), return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if no value is cached, get the matrix
  mtx <- x$get()
  ## solve for the inverse
  inv <- solve(mtx, ...)
  ## and cache the inverse
  x$setinv(inv)
  ## return the inverse
  message("no cached inverse; solving for new inverse")
  inv
}
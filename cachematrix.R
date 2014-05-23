## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  
  ## This function sets the value of the matrix
  set <- function(y) {  
    x <<- y
    mtx <<- NULL
  }
  
  ## This function gets the value of the matrix
  get <- function() x
  
  ## This function sets the value of the Inverse Matrix and 
  ## put it in the cache
  setInvMtx <- function(InvMtx) mtx <<- InvMtx
  
  ## This function gets the value of the Inverse Matrix
  getInvMtx <- function() mtx
  
  list(set = set, get = get,
       setInvMtx = setInvMtx,
       getInvMtx = getInvMtx)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, 
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if the inverse is cached
  mtx <- x$getInvMtx()
  if(!is.null(mtx)) { 
    ## if cached, return the cached value
    message("getting cached data")
    return(mtx)
  }
  
  ## If not, calculate the inverse
  data <- x$get()
  mtx <- solve(data, ...)
  x$setInvMtx(mtx)
  mtx
}

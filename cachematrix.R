## The first function, makeCacheMatrix, sets up an object which is 
## a list of functions to be used by cacheSolve.  The functions are
## used to set objects in the cache, in conjuction with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Initialize my inverse object as null
  set <- function(y){
    x <<- y ## Initialize x as the y argument
    inv <<- NULL ## Initialize my inverse object as null
  }
  get <- function() x ## Simple get function just returns x (once set)
  setinverse <- function (solve) inv <<- solve  ## Key function which 
  ## sets the inverse matrix with the inv object passed from the
  ## cacheSolve function
  getinverse <- function () inv ## Simple get function just returns the inverted matrix (once set)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ## returns the list of functions
}

## The second function, cacheSolve, can call each of the functions
## within makeCacheMatrix, using the list of functions returned by
## makeCacheMatrix.  This can be used to set/get the cache and then
## calculate the inverted matrix using the solve function.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse() ## As x is a makeCacheMatrix object, it can
  ## first call the getinverse function of makeCacheMatrix to obtain
  ## the inverted matrix from the cache, if it had been previously set
  if(!is.null(inv_matrix)){
    ## If there indeed is an inverted matrix in the cache, print a
    ## message to screen and return the matrix (ending the function)
    message("getting cached data...")
    return(inv_matrix)
  }
  orig_matrix <- x$get() ## Call the get function from makeCacheMatrix
  ## to get the original matrix
  inv_matrix <- solve(orig_matrix,...) ## Use the solve function
  ## on the original matrix just obtained
  x$setinverse(inv_matrix) ## And then set it in the cache
  inv_matrix ## Return the inverted matrix
}
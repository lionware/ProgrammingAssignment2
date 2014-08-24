## These functions enable a matrix and its inverse to be cached,
## which can assist reduce the time/ cost of calculating the inverse repeatedly


## Creates an object that contains functions to store the matrix in cache
## Returns a list of functions to access the original and inverse matrices
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL

   ## Initialises this object and its cache,
   ## with the original matrix as passed and the inverse set to null
   set <- function(y) {
      x <<- y
      i <<- NULL
   }

   ## Returns the original matrix
   get <- function() {
      x
   }

   ## Caches the inverse matrix, by setting it to the passed matrix
   setinverse <- function(matrix) {
      i <<- matrix
   }

   ## Gets the cached inverse matrix
   getinverse <- function() {
      i
   }

   ## Return the list of functions
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Solves the inverse of a matrix and stores it in cache, given a makeCacheMatrix object
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## get matrix from cache
   inv <- x$getinverse()
   
   ## if matrix has already been cached, return it (ending the function call)
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   ## if we've got to here, the matrix was not cached so
   ## get the original matrix,
   ## find its inverse
   ## and cache it (for next time)
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setinverse(inv)
   
   ## return the inverse matrix
   inv
}

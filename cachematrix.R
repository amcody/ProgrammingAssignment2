## This is a pair of functions that allows one to set a matrix,
## determine its inverse, and cache the inverse for later use. 
## If the inverse is requested more than once, and the matrix has 
## not been reset in the meantime, then the code saves time by
## pulling out the cached result.

## The following function inputs a matrix and returns a list of
## four functions that 1)allow for resetting the matrix ('set'),
## 2)retrieve the original matrix ('get'), 3)store a new matrix
## as the inverse ('setinv'), and 4)retrieve the stored inverse
## ('getinv')

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
       	      x <<- y
       	      inv <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) inv <<- inverse
       getinv <- function() inv
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv
            ) 
}

## The following function takes the matrix defined by
## either a call of the function 'makeCacheMatrix' or
## its "child" function 'set', and returns the inverse
## of that matrix. Note that if the matrix has been 
## changed manually before calling this function, it will
## NOT return the correct inverse. We assume such manual
## changing without the use of 'set' does not occur.
## The 'cacheSolve' function saves computation time by
## only computing the inverse if it has not already been
## determined. If it has already been determined, the function
## will simply return the previously stored value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinv()
         if (!is.null(inv)) {
         	message("getting cached data")
         	return(inv)
         }
         data <- x$get()
         inv <- solve(data, ...)
         x$setinv(inv)
         inv
}

## Matrix inversion can be slow.
## To avoid re-computing inverse of a matrix, we cache results

## Helper function to store cached results of solve()
## It returns a list of 4 functions:
##  set(x) - sets the value of the input matrix
##  get() - get the original matrix
## setsolve() - saves the result of matrix inverse
## getsolve() - gets the cached result of matrix inverse, NULL if value is not yet computed
##
## Wrap your matrix in makeCacheMatrix() and pass it to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solv) s <<- solv
    
    getsolve <- function() s

    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Compute matrix inverse
## Function expects a list of functions created by makeCacheMatrix()
## It will first look for a cached value, and, only if cached value does not 
## exist, it will compute the inverse
## Example
##
## m <- matrix(randu[1:16,1],4,4)
## cacheSolve(makeCacheMatrix(m))

cacheSolve <- function(x, ...) {
        
    i <- x$getsolve()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
    
}

## This two functions work together. Them allow user to bring 
## cached answers from solve(), that is a very heavy  processing
## function.

## First function create a special kind of matrix that can save
## cached results when solve() is applied. 
## Normal matrix "x" can be solved by solve(), but special cache
## matrix produced by this function not.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function can solve special matrix created by first 
## function, instead solve(). It provides the same result with
## a big difference: If the special matrix has been solved before
## (and it has bben not modified), it doesn´t need to recalculate
## the result - as solve() does - , just bring cached answer and
## saves lots of processing.


cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    ## Return a matrix that is the inverse of 'x'. 
    ## Similar to solve(), but checking cached results
    
    ## How to probe it: Create a complex matrix with
    ## > makeCM <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+")}
    ## > x <- makeCM(8)  ... to produce an 8 square complex matrix
    ## > a <- makeCacheMatrix(x)  ... to produce special cacheable
    ## version of "x" special matrix
    ## > cacheSolve(a)  ... to return result equal to solve(x) but
    ## using cache. Checking for stored results.   
}

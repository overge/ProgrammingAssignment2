## This function uses the ability to cache a function that is costly in
## computation time. To do so, we create a special structure (CacheMatrix)
## that has several functions. Then we run the cacheSolve function to
## check if the structure has cached data or calculate the inverse matrix.

## Function :: makeCacheMatrix
## Function to create a special matrix object that supports
## the storage of the cached calculation of the inverse matrix function.  
## This structure is to be used in the cacheSolve() function.
## This structure has several functions: set, get, setinv and getinv
## x : matrix

makeCacheMatrix <- function(x = matrix()) {

    ## initialize the CacheMatrix by setting var matrix_inv (cache) to null
    matrix_inv <- NULL

    ## Internal Function :: set
    ## Establish the values of the CacheMatrix
    set <- function(y) {
        x <<- y
        matrix_inv <<- NULL
    }

    ## Internal Function :: get
    ## Return the values of the CacheMatrix
    get <- function() x

    ## Internal Function :: setinv
    ## Store the inverse matrix cache
    setinv <- function(solve) matrix_inv <<- solve
  
    ## Internal Function :: getinv
    ## Return the inverse matrix cache
    getinv <- function() matrix_inv
  
    ## Assign the functions to the cache so that they can be called
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## Function :: cacheSolve
## This function uses the available functions in the CacheMatrix structure
## to return the inverse matrix of x (using, if possible, cached data)
## Steps:
## A) Validate if the inverse matrix has been previously calculated.
## B) If it has, 
##    Return value
## C) If it has not,
##    Perform the inverse matrix, store and return
## Note: this function requires a suitable CacheMatrix structure as the
## input argument. We assume that this CacheMatrix is invertible.
cacheSolve <- function(x, ...) {
    
    ## Collect the cached inverse matrix from the CacheMatrix (if available)
    m <- x$getinv()
  
    ## Validate if the inverse matrix had been calculated before
    if(!is.null(m)) {
      
        ## If it had been calculated, notify the end user and return the value
        message("getting cached data")
        return(m)
    }
  
    ## If it had not been calculated, perform the calculation of the inverse,
    ## store in the CacheMatrix structure and return the value
    m <- solve(x$get(), ...)
    x$setinv(m)
    m
}

## Function :: testCacheSolve
## Present two executions using the system.time R function
## The test matrix is a square matrix with random values
## First execution creates new, clean matrix
## Second execution uses cached data
## x : size (nrows, ncols) of the test matrix, max. 2000
testCacheSolve <- function(x = 1000) {
  if(x > 2000) {
    message("Limited to 2000 x 2000 matrices")
    x <- 2000
  }
  matrix_for_test <- makeCacheMatrix()
  matrix_for_test$set(matrix(rnorm(x*x),x,x))
  message("First execution")
  xx <- system.time(cacheSolve(matrix_for_test))
  print(xx)
  cat("\n")
  message("Second execution")
  xx <- system.time(cacheSolve(matrix_for_test))
  print(xx)
}
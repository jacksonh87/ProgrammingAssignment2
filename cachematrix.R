## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
##
## The function is used as follows:
## makeCacheMatrix(x), where x is a square matrix
##
## Per the assignment instructions it assumes a square matrix is 
## supplied, so there is no checking in the function to ensure the
## inverse can actually be calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ### 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ### 2. get the value of the matrix
  get <- function() x
  
  ### 3. set the value of the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  
  ### 4. get the value of the inverse of the matrix
  getsolve <- function() m
  
  ### Returns the list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function takes the list generated when makeCacheMatrix is used on
## a matrix, and returns the inverse of that original matrix.
## 
## If the result already exists in cache, the cached value of the inverse 
## is returned. If not, the inverse is calculated, cached and returned.
## 
## The function is used as follows:
## cacheSolve(x, ...), where x is a list outputted by the makeCacheMatrix 
## function
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempt to retrieve inverse from cache
  m <- x$getsolve() 
  
  ## Check if cached value is NULL or not
  if(!is.null(m)) { 
    ## If it isn't NULL then it contains the inverse of the matrix
    message("getting cached data")
    return(m)
    ## Function ends here if it was cached already
  }
  
  ## But if the cache is NULL we need to solve the inverse of the matrix
  ## Get the matrix:
  data <- x$get()  
  ## Use solve to find the inverse of the matrix:
  m <- solve(data, ...) 
  ## Cache it:
  x$setsolve(m)
  ## Return the inverse of the matrix:
  m		      
  ## Function ends here
}
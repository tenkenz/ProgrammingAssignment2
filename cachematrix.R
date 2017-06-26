## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The two functions below are about teaching how to cache data
## Typically, certain operations are costly in processing power
## and time. Calculating the inverse of a matrix is one of those.
## For this assignment, we calculate the inverse of a matrix.
## Then attempt calculate it again but check if it's inverse 
## already exists in the cache. If it does, it is pulled
## from the cache rather than recalculated.
##
## Note that the cache is another R environment that is not
## loaded into memory at the given time. It is assumed that the 
## matrix is always invertible (as per assignment instructions).

## makeCacheMatrix - creates a 'special matrix'.
## This actually is a list containing a function to:
## 1. Set a value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set matrix value
  set <- function(y) {
    # Use <<- to cache
    x <<- y
    m <<- NULL
  }
  ## Get matrix value
  get <- function() x
  ## Set matrix inverse
  setinverse <- function(inverse) m <<- inverse
  ## Get matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve determines the aformentioned 'special matrix'
## If the inverse has not changed, ie has already been calculated
## then it is retreived from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    # Get cached value if exists
    message("getting cached data")
    # Return retreived value, skipping further calculation
    return(m)
  }
  # Otherwise calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  # Output inverse matrix value
  m 
}

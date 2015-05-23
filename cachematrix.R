## Pair of functions to store and retrieve the inverse of a matrix from a cache.
## This saves the time to compute the matrix inverse if the calculation is likely
## to be repeated.

## makeCacheMatrix takes a matrix and returns a list of functions that cache 
## the matrix inverse 

## The list should be assigned to a variable, e.g.
## matrixList <- makeCacheMatrix(someMatrix)

## cacheSolve(matrixList) returns the inverse of someMatrix. If it has been
## calculated and stored previously it returns the stored value and skips the 
## computation. 

##

## For any matrix, this function creates a list of functions to get and store the
## matrix and its inverse in a distinct environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## For any list created above, this function looks to see if the inverse of the 
## matrix has been stored already. 

## If it has, the inverse is returned. If not, the inverse is calculated and 
## stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

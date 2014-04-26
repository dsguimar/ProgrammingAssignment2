## THanks to Fu Sheng Wang...
## https://class.coursera.org/rprog-002/forum/thread?thread_id=696&sort=oldest

## These pair of functions are intended to either compute and cache or to retrive a cached
## matrix inverse.

## This function takes a matrix input and outputs a list of functions corresponding
## setting / getting both the matrix itself and the matrix inverse
## An example usage would be use a<- makeCacheMatrix(matrix(1:4,2))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                   ## set a placeholder for the inverse to null

  set <- function(y) {        ## sets a new value for the matrix itself, 
    x <<- y               
    i <<- NULL
  }
  
  get <- function() x         ## retrives the value of the matrix
  
  setInv <- function(inv) i <<- inv   ## sets the value of the inverse direclty
  
  getInv <- function() i      ## retrives the value of the inverse
  
  return(list(set = set, get = get, setInv=setInv, getInv = getInv))  ## returns the list
}


## This function takes the input of a list variable (x) made with the function makeCacheMatrix
## the function then examines x to see if there is an associated cached value for the matrix inverse.
## If there is, the function returns that cached value, if not it computes the inverse
## using the solve function.  To check accuracy use x$get() %*% x$getInv(), matrix times its inverse
## will return identity matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()         ## query the x vector's cached value for the inverse
  if(!is.null(i)) {       ## if there is a cached value for the inverse then this
    message("getting cached data")
    return(i)             ## exit function and return cached value for the inverse
  }
  data <- x$get()         ## if there is no cached value then we must 
  i <- solve(data, ...)   ## actually calculate the inverse, which we do here
  x$setInv(i)             ## this line saves the inverse value back into x's cache
  return(i)               ## return the result of the inverse calculation
  
}

## These functions enable to generate a matrix object that can cache 
## its inverse.
##
## a <- makeCacheMatrix()    initializes an empty matrix
## a$set(x)                  assigns values to matrix
## a$get()                   returns the matrix
## cacheSolve(a)             calculates or retrieves the inverse of 'a'
##                           from cache and stores it using a$setinv()  
## a$getinv()                returns the inverse of the matrix


## Function makeCacheMatrix generates a matrix object with four methods
## (1) set: assigns values to the matrix elements
## (2) get: returns the matrix
## (3) setinv: caches the inverse of the matrix
## (4) getinv: returns the inverse of the matrix

makeCacheMatrix <- function(a = matrix()) {
  
  #initilaize the inverse with NULL
  inv <- NULL
  
  #function for setting matrix values
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  
  #function to read matrix values
  get <- function() a
  
  #function for caching the inverse
  setinv <- function(invers) inv <<- invers
  
  #function to read the inverse
  getinv <- function() inv
  
  #return the four functions as a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## The function cacheSolve calculates the inverse of a matrix when not 
## already cached, otherwise returns the cached value

cacheSolve <- function(a, ...) {
  
  # make sure only square matrices are considered
  nr <- nrow(a$get())
  nc <- ncol(a$get())
  if(nr != nc) {
    message("Can only inverse square matrices")
    return()
  }
  
  # checks if inverse already in cache
  inv <- a$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  mtrx <- a$get()
  
  #generate a unity matrix of apropriate size
  unity <- diag(nr)
  
  #inverse the matrix solving mtrx * x = 1 for x
  inv <- solve(mtrx,unity, ...)
  
  #cache the inverse of the matrix
  a$setinv(inv)
  inv
}



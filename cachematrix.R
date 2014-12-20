## These functions allow to create a cache area to store a matrix and its inverse, 
## and to return the inverse of the matrix from cache if it has been already calculated, otherwise to calculate it and return it.
## The functions assume the matrix is inversible.

## The makeCacheMatrix function receives as argument a matrix and defines four functions that allow to store (set) the matrix and its inverse in cache, 
## or to retrive (get) the matrix and its inverse from cache. 
## The function returns a list that has the four functions as its elements 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmatrix <- function(y = matrix()) {
    x <<- y
    inv <<- NULL 
  }
  
  getmatrix <- function() { 
    x 
  }
  
  setinv <- function(invmatrix) {
    inv <<- invmatrix
  }
  
  getinv <- function() {
    inv
  }
  
  list(set = setmatrix, get = getmatrix,
       setinverse = setinv,  
       getinverse = getinv)    
    
}


## The cacheSolve function receives as argument a list of functions as returned by the makeCacheMatrix function and 
## if the inverse of the matrix has been already stored in cache it retrieves it from there, if not it is calculated and stored in cache for faster
## retrieval the next time. The cache is reset when a new call to makeCacheMatrix causes that a new instance of a list of functions be passed to cacheSolve.
## The function returns a matrix that is the inverse of the matrix passed as argument to makeCacheMatrix (or a matrix set using the setmatrix function).

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv() 
  
  if (!is.null(inv)) {                # It is cached, use cache 
    message("getting cached data")
    return(inv)
  }
  else                                # It is not cached, calculate inverse and store in cache
  {
    inv <- solve(x$get())
    x$setinv(inv)
    inv
  }   
  
}

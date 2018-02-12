#The makeCacheMatrix will create a special object for matrix that can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



#Function will calculate the inverse of the special matrix object returned by
#the makeCacheMatrix function earlier. 
#1) if inverse has already been calcualted, then cachesolve should retrieve 
#inverse from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #computes the inverse of a square matrix
  x$setinverse(inv)
  inv
}
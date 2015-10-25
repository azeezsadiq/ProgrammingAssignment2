## This .R script calculates the inverse of a matrix and caches it so that it
## can be stored for future use. If it does not exist, then it is calculated. After that,
## it is retrieved via caching

## The function makeCachematrix serves to set the value of the matrix and its inverse 
## as well as retrieve them later.

makeCacheMatrix <- function(x = matrix()) {  
  
     m <- NULL

## The set function serves to change the value of the existing matrix. This is done
## via the superassignment operator so that the value is accessible in the containing 
## environment outside the function body. This enables the value to be visible to the 
## other function cacheSolve. The variable m is also reset to NULL globally so that
## the cachesolve function will know whether to recalculate it or not.
     
  set <- function(y) 
     {
          x <<- y
          m <<- NULL}
  
## The get function simply retrieves the value of the matrix.It is to be noted
## that this retrieves the value outside the function.
  
  get <- function() x
  
## The setinverse function  receives the inverse from cacheSolve and sets its value globally.
 
   setinverse <- function(inverse) m <<- inverse
  
## The getinverse function simply retrieves the value of the inverse of the matrix.
## It is to be noted that this retrieves the value outside the function.
  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  

## The function cacheSolve calculates the inverse of the matrix if it is not stored in the 
## global environment already.

cacheSolve <- function(x,...) {
  
  m <- x$getinverse()

## This checks whether the inverse is stored in m in the global environment or not. If it is,
## then it is displayed.
  
  if(!is.null(m)) 
    {
    message("getting cached inverse")
    return(m)
  }
  
## If the inverse was not stored( because it hadn't previously been calculated), then it is
## calulated. The value is stored globally in the variable m via the setinverse function.
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

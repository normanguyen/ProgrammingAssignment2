## the following functions allow us to cache the inverse of a matrix to save on 
## computation. 
##  makeCacheMatrix allows us to set & get a matrix
##  and set anjd get the inverse of the matrix

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

## cacheSolve returns the inverse of the matrix, returning the cached inverse
## if it has previously computed to save computation

cacheSolve <- function(x, ...) {
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

m1<-matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2)
myMatrix_object<-makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

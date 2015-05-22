## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     
     set <- function(y) {
          matx <<- y
          ## Set up matp to hold previous matrix
          if(is.null(inv)) matp <<- NULL
          
          inv <<- NULL
     }
     get <- function() matx
     setinv <- function(solve) {
       ## Store matrix used
       matp <<- matx
       ## Store inv matrix
       inv <<- solve
     }
     
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     
     ## If there is an inverse and the matrix has not changed, return cached data
     if(!is.null(inv) && (matp == x$get())) {
          message("Same Matrix, getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}

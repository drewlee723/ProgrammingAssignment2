## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below are a pair of functions that cache the inverse of 
## a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
              
              if(det(x) == 0){
                    message("matrix not invertible")
              }
              
              else{
              i <- NULL  
              set <- function(y){
                  x <<- y
                  i <<- NULL
              }
              get <- function() x
              setrev <- function(solve) i <<- solve
              getrev <- function() i
              list(set = set, get = get,
                   setrev = setrev,
                   getrev = getrev)
              }
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getrev()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setrev(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

# Coursera R programming course second assignment
# drewlee723@gmail.com / UOU College of Medicine, South Korea
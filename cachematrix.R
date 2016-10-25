## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix  <- function(x = matrix()) {
  inver<- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) inver <<- inversa
  getinversa <- function() inver
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.
          
cacheSolve  <- function(x, ...) {
  inver <- x$getinversa()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinversa(inver)
  inver
}

         
# Sample:

ej1= rbind(c(1, 2), c(1,3))

m= makeCacheMatrix(ej1)

m$get()
#     [,1] [,2]
#[1,]    1    2
#[2,]    1    3

cacheSolve(m)

#       [,1] [,2]
#[1,]    3   -2
#[2,]   -1    1


cacheSolve(m)
#getting cached data
#       [,1] [,2]
#[1,]    3   -2
#[2,]   -1    1


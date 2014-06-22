## This function is implemented to calculate inverse of the matrix.
## If the inverse already exist then the cached copy of inverse is returned

## makeCacheMatrix function makes a cached copy of inverse of matrix if it does
## not already exist

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv<<-solve
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of matrix. If cached copy is present then it is
## returned otherwise inverse calculate an returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

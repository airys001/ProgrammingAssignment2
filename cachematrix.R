## makeCacheMatrix just create a matrix, and store its inverse.
## test code: a <-makeCacheMatrix(matrix(c(3,2,4,5,-1,2,-1,3,-3),3,3))
## test code: b<-cacheSolve(a)
## test code: a$getm(), to get the matrix a
## test code: a$getinv(), to get the stored inverse, if any. Should be NULL for the first time(or matrix changed)


makeCacheMatrix <- function(x = matrix()) {
  inv_p <- NULL
  setm <- function(y) {
    x <<- y
    inv_p <<- NULL
  }
  getm <- function() x
  setinv <- function(inve) inv_p <<- inve
  getinv <- function() inv_p
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}



## Cachesolve is to solve the inverse and store the value into makeChaeMatrix's inv_p

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_p<-x$getinv()
  if(!is.null(inv_p)) {
    message("getting cached inverse matrix")
    return(inv_p)
  }
  data2 <- x$getm()
  inv_p <- solve(data2, ...)
  x$setinv(inv_p)
  inv_p
}

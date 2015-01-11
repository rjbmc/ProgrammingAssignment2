## Those functions are used for avoid the calculus of the inverse of the a matrix
## if the calculus has been already done before.

## This function create a list with 4 functions, get, set, getInv and setInv that 
## gets and sets respectively the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() {
      x
   }
   setInv <- function(invers) {
      inv <<- invers
   }
   getInv <- function() {
      inv
   }
   list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns, from cache if possible or by calculus if not, the 
## inverse of the matrix included in the x-structure 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getInv()
   if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat)
   x$setInv(inv)
   inv
}

## These functions create a list object which allows you to
## set or get the values of a matrix and its inverse.  The
## inverse is calculated if it doesnt exist, and then cached
## for later reference so it doesnt have to be recalculated.


## This function creates a list of 4 functions for managing
## a matrix and its inverse.  The first stores the matrix,
## and initializes the inverse variable since the matrix has
## been changed.  The second returns the matrix.  The thrid
## sets the value of the inverse matrix, and the fourth returns
## the inverse matrix.

makeCacheMatrix <- function(val = matrix()) {
     inv <- NULL
     set <- function(newVals) {
          val <<- newVals
          inv <<- NULL
     }
     get <- function() val
     setInv <- function(mtrInv) inv <<- mtrInv
     getInv <- function() inv
     list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns a cached value for the inverse
## from the caching list of the matrix if it exists,
## otherwise it calculates the inverse, stores it in the
## cache, and returns it. First it calls the caching list
## for the inverse, and returns it if it is found.  If it
## is not, it brings in the original matrix, calculates
## the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     mtr <- x$get()
     inv <- solve(mtr)
     x$setInv(inv)
     inv
}

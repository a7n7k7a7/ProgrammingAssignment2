## The function calculates the inverse of a matrix, if the inverese is calculated
## for the second (or more) time - the inverse matrix is loaded from the memory 
##(not calculated for the second time).

## The makeCacheMtrix function creates a list of functions, which enable saving inverse matrices
## and getting them back from the memory

makeCacheMatrix <- function(x = matrix()) {
	
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mxinverse) inv <<- mxinverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The cacheSolve function either calculates inverse matrix (if it wasn't yet calculated), 
##or loads the already calculated inverse matrix from the memory.


cacheSolve <- function(x, ...) {
        
	inv <- x$getinv()

      if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
      }

      a <- x$get()
      inv <-  solve(a, diag(nrow(a)))
      x$setinv(inv)
      inv
	

}

##Overall description:

##The script contains two functions that cache the inverse of a matrix: one called makeCacheMatrix and one called cacheSolve.
##makeCacheMatrix is used to create a special "matrix" object. 
##Because matrix inversion is can be a costly computation, caching the inverse of a matrix may be the best solution when working with large ones.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
		x <<- y
		inv <<- NULL
	}
    get <- function() x
    setInv <- function(inv_matr) inv <<- inv_matr
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve() should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
		inv <- x$getInv()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setInv(inv)
		inv		
		
}

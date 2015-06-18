##Overall description:

##The script contains two functions that cache the inverse of a matrix: one called makeCacheMatrix and one called cacheSolve.
##makeCacheMatrix is used to create a special "matrix" object. 
##Because matrix inversion is can be a costly computation, caching the inverse of a matrix may be the best solution when working with large ones.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
	#set the values for the matrix using the <<- operator to assign to object defined in a different environment
    set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	#retrieve the values of the matrix
    get <- function() x
	#define a function that sets the inverse of the matrix
    setInv <- function(inv_matr) inv <<- inv_matr
	#retrieve the inverse of the matrix
    getInv <- function() inv
	#create a list with the 4 functions defined above to store and retrieve elements of the matrix and its inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve() should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
		#read the cached matrix inverse
		inv <- x$getInv()
		#if the cached matrix already exists in memory, return it
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		#if no matrix inverse has been already cached, read the matrix and calculate its inverse
		data <- x$get()
		inv <- solve(data, ...)
		#set (cache) the current matrix inverse into the special matrix object created with makeCacheMatrix() 
		x$setInv(inv)
		inv		
		
}

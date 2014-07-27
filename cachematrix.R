## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
	mI <- NULL
	set <- function(y) {
	m <<- y
	mI <<- NULL
	}
	get <- function() m
	setInverse <- function(inverse) mI <<- inverse
	getInverse <- function() mI
	list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
	mI <- x$getInverse()
	if(!is.null(mI)) {
		message("Retrieving cached data")
		return(mI)
	}
	data <- x$get()
	mI <- solve(data, ...)
	x$setInverse(mI)
	mI
}

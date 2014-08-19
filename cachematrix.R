## Matrix inversion is a costly computation.  These 
## functions cache the inverse of a matrix so as to 
## avoid computing it repeatedly.

## Creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## Computes the inverse of a special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been
## calculated (and the matrix has not changed), then the
## cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
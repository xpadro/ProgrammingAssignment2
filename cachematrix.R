## Calculation of the inverse of a given matrix.
## Previous results will be cached in order to avoid
## repeating costly calculations.


## Creates a special matrix which exposes a set of functions
## to store or retrieve it and its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
	inv <- NULL
	set <- function(newMatrix) {
		mtx <<- newMatrix
		inv <<- NULL
	}
	get <- function() mtx
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	list(
		set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## Calculates the inverse of a given matrix.
## If the calculation was previously done, it returns
## the cached result.

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
	cache <- mtx$getinverse()
	if (!is.null(cache)) {
		message("getting cached data")
		return(cache)
	}
	data <- mtx$get()
	cache <- solve(data)
	mtx$setinverse(cache)
	cache
}

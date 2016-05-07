## Assignment: Caching the Inverse of a Matrix

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse 
## get the value of the Inverse

makeCacheMatrix <- function(matrixdata = matrix()) {
		solvedata <- NULL
		##set the value of the matrix
		set <- function (y) {
			 matrixdata <<- y
			 solvedata <<- NULL
		}
		## get the value of the matrix
		get <- function () matrixdata
		## set the value of the Inverse
		setinverse <- function(inverse) solvedata <<- inverse
		## get the value of the Inverse
		getinverse <- function () solvedata 
		
		list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Computing the inverse of a square matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        	## check the cached data
	  	solvedata <- x$getinverse()
        	if(!is.null(solvedata)) {
                	message("getting cached data")
                	return(solvedata)
        	}
        	## Computing the inverse of a square matrix
		matrixdata <- x$get()
        	solvedata <- solve(matrixdata)
        	x$setinverse(solvedata)
        	solvedata
}

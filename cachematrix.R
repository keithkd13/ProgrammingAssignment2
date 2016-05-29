## makeCacheMatrix creates a list containing a function to
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the value of inverse of the matrix
## 4.Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function first checks if the inverse has already been computed. 
## If yes, it gets the result and skips the computation.
## If no, it computes the inverse, sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

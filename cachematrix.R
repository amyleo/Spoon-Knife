
## In order to cache an inverse of a matrix, we are using the following functions
## which are 'makeCacheMatrix' and 'cacheSolve'.
## 1. 'makeCacheMatrix' function creates a special matrix that can cache its
## inverse.
## 2. 'cacheSolve' function computes the inverse of the special matrix created by
## 'makeCacheMatrix'. If the inverse has been already been calculated, the
## function will get the inverse from the cache.

## Here is an example to see if my functions work correctly

## > source("cachematrix.R")
## > m1 <- matrix(1:4, 2, 2)
## > m <- makeCacheMatrix(m1)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinverse()
## NULL
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > m$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## 'makeCacheMatrix' function creates a specical "matrix", which is in order to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv 
	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)

}


## 'cacheSolve' function returns the inverse of the specical "matrix" created
## with the 'makeCacheMatrix' function. If the inverse has already been
## calculated, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets it in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv) 
	inv                 

}

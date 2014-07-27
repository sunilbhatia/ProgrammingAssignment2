## The code has 2 main functions viz. makeCacheMatrix() and cacheSolve().
## The makeCacheMatrix() takes a matrix as input and creates an object that is returned
## The cacheSolve() takes the object returned from the makeCacheMatrix() and computes 
##		inverse of matrix. If the computation is cached the same is returned from cache
##		else the inverse is calculated using the solve() function.
##
## Additionally, the set() method of the makeCacheMatrix has a check to see if the new matrix being
## 		passed is identical to the existing object then the set method does nothing. 
##		However, if a new matrix is passed then the cache is cleared and new object is stored locally

## The makeCacheMatrix method takes a matrix object and returns an object contain the following methods:
##		- set - sets a non identical matrix
##		- get - gets the matrix stored in object
##		- setinverse - sets the inverse of matrix
##		- getinverse - gets inverse of matrix from local cache
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    set <- function(y) {
		if(identical(x, y) == FALSE) {
			message("Matrix is different")
			x <<- y
        	inverse <<- NULL
		}
		else {
			message("Matrix is identical")
		}
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve method returns an inverse of a matrix. The method returns the result either from
## its local cache or if the result is not cached the result is computed, stored in the cache and 
## then returned
cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        else {
        	message("Computing new inverse...")
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


## This is a test method that can be executed after the .R file is loaded
testCacheSolve <- function() {
	## Create a new matrix and solve
	a = makeCacheMatrix(matrix(1:4, 2,2))
	print(cacheSolve(a))

	## Create an identical matrix to 'a'
	a$set(matrix(1:4, 2,2))
	print(cacheSolve(a)) #This should return from cache

	## Create an identical matrix to 'a'
	a$set(matrix(5:8, 2,2))
	print(cacheSolve(a)) #This should compute a new inverse
}
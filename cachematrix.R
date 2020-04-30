## Put comments here that give an overall description of what your
## functions do
#Edit: the initial submit didn't have any explanatory comments
#Because it wasnt there in example so i missed it. 
# This function creates a special “matrix” object that can cache its inverse.
# return: a list containing functions to
# 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
#this list is made so that it can be used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	set <- function(y) {
		x<<- y
		i<<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i<<- inverse
        getinv <- function() i
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)

}


#computes the inverse of the “matrix” returned by makeCacheMatrix(). 
#If the inverse has already been calculated and the matrix has not changed,
#it’ll retrieves the inverse from the cache directly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}

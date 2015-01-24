## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
##    that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set       = set, 
		 get       = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## It is assumed that x is an invertible matrix
        m <- x$getinverse() 
        if(!is.null(m)) {
            message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m		
}

## Calculating the inversion of a matrix can be volatile and take time.
## Instead of calcuting the inverse repeatedly, the below two functions have been created to check the cache
## first if the matrix has been previously computed. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i.matrix <- NULL
        set <- function(y) {
                x <<- y
                i.matrix <<- NULL
        }
        get <- function() x
        set.imatrix <- function(matrix) i.matrix <<- matrix
        get.imatrix <- function() i.matrix
        list(set = set, get = get,
             set.imatrix = set.imatrix,
             get.imatrix = get.imatrix) 
}

## This function returns the inverse of the matrix created, i.e. when multipled against the original matrix will equate to 1.

cacheSolve <- function(x, ...) {
	i.matrix <- x$get.imatrix()
        if(!is.null(i.matrix)) {
                message("getting cached data")
                return(i.matrix)
        }
        data <- x$get()
        i.matrix <- solve(data, ...)
        x$set.imatrix(i.matrix)
        i.matrix        
}

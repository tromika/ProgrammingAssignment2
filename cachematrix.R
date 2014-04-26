
## Get and set functions for source and calculated inverse matrixes
## Cache source and inverse matrixes for future use


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set_matrix <- function(y){

		x <<- y
		m <<- NULL

	}
	get_matrix <- function() x ##You got NULL result if matrix is not stored before

	store_inverse <- function(inverse) m <<- inverse
	return_inverse <- function() m
	list(set_matrix = set_matrix, get_matrix = get_matrix, 
	      store_inverse = store_inverse, 
	      return_inverse = return_inverse)
}


## The function calculates the inverse of a given x matrix
## After its calculated store it for future use
## Before the calculation the function checks the the results are cached before or not.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$return_inverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)        	
        }
        data <- x$get_matrix()
        inverse <- solve(data)
        x$store_inverse(inverse)
        inverse
}

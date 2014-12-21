## The following functions try to mimic the operation of a cache
## wherein redundant operations are computed just once and stored.

## makeCacheMatrix is a function that returns a list contains getter and
## setter functions for storing the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	  #Setter for matrix x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	  #Getter for matrix x
        get <- function() x

        #Setter for inverse matrix m
        setMatrix <- function(matrix) m <<- matrix

	  #Getter for inverse matrix m
        getMatrix <- function() m
        list(set = set, get = get,
             setmatrix = setMatrix ,
             getmatrix = getMatrix )

}


## Returns the inverse of a matrix
## Checks if it is present in the cache.
## Returns the value in the cache, if present.
## Else, generates the value, stores it in the cache and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getMatrix()
	  ##Checking if present in cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  ##Computing if not present
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}

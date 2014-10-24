## Put comments here that give an overall description of what your
## functions do: 2 functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    get <- function() {
    	m
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("Please wait. Getting cached data...")
            return(m)
    }

    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}

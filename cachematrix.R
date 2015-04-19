## This module provides two functions - makeCacheMatrix and cacheSolve.
## These functions can be used in tandem to compute and store the
## inverse of a given matrix wihtout recomputing it every time.
## Example:
##
## a <- matrix(c(1, 2, 3, 4),  nrow=2, ncol=2)
## b <- makeCacheMatrix(a)
## cacheSolve(b)
## cacheSolve(b)
## cacheSolve(b)
##
## Even though we called cacheSolve(b) 3 times, the inverse was
## only calculated once, because the answer was stored in memory
## on the matrix cache.
## 

## This function creates a special matrix cache that
## allows one to store a pre-computed cache of the solved
## inverted matrix for a given matrix.
makeCacheMatrix <- function(x = matrix()) {
    solved_matrix <- NULL
    set <- function (m) {
        x <<- m
        # Because we're overwriting the matrix stored in this cache,
        # we need to clear any solved matrix that may or may not
        # have been computed with it.
        solved_matrix <<- NULL
    }
    get <- function() x
    setsolved <- function(s) solved_matrix <<- s
    getsolved <- function() solved_matrix
    list(set = set,
         get = get,
         setsolved = setsolved,
         getsolved = getsolved
    )
}

## This function serves as a wrapper function around R's `solve`
## function.  When this function is called, it first checks to see
## if there is a cached value of the solved matrix already in the
## special matrix cache, and if so, it returns that.  Otherwise,
## it computes the solved matrix and stores it on the special
## matrix cache, and returns the answer.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolved()
    
    if (!is.null(m)) {
        # The cache matrix had a pre-computed inverted matrix.
        # Let's just return that.
        return(m)
    }
    # Get the underlying matrix to solve.
    data <- x$get()
    
    # Do the work to compute the inverted matrix.
    m <- solve(data)
    
    # We now need to tell the matrix cache to store the inverted
    # matrix.
    x$setsolved(m)
    m
}

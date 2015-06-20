## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix
## Usage: Assign function to var - x <- makeCacheMatrix()
##        Then use setter to assign matrix -  x$set(matrix(1:4,2,2))
##        To check value was assigned use get - x$get()
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
            setmatrix = setmatrix,
            getmatrix = getmatrix)
    
}


## Write a short comment describing this function
## Function cachesolve
## Will use function assigned to function parm to retrieve matrix and assign to m
## Then Test if m is not empty and unchanged then will return cached value
##
## Otherwise calculates inverse using solve(), sets cache and returns matrix
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ## Testing if matrix exists and it has not changed.  If true, return cache
        if(!is.null(m) && identical(m, x)) {
            message("getting cached data")
            return(m)
        }
        
        ## Test failed, use solve to calculate inverse of matrix and set
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

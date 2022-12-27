## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a list of functions that set and get the matrix and
## set and get the inversion of the matrix, it does this by:
## 1. creating 2 shell variables x and m
## 2. defines function 'set' and assigns values to x and m (in parent environ)
## 3. defines the 'get' for x
## 4. defines the 'get' and 'set' for m
## 5. assign each of the functions defined above to elements in a list to be
##      returned by the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function
## Creates the inversion of the matrix, but checks to make sure that
## the inversion does not already exist using the makeCacheMatrix, above
## if no inversion exists, then this will invert the matrix
## 1. attempt to retrieve a cached inverted matrix from the list created by
##      makeCacheMatrix by looking at the 'getinvmatrix' element
## 2. if this is not null, then it returns the cached inv matrix
## 3. if it is null, then it pulls down the matrix and applies the 'setinvmatrix'
##      function defined in makeCacheMatrix list

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}

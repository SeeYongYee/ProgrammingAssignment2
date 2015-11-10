## Matrix inversion is usually a costly computation and there may be some benefit to 
## cache the inverse of a matrix rather than compute it repeatedly. This R script
## code contains a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix is a function that creates a special "matrix" object that can cache
## its inverse. It contains 4 functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the "inverse of the matrix"
## 4) get the value of the "inverse of the matrix"
makeCacheMatrix <- function(x = matrix()) {
    # variable used to store the "inverse of the matrix"
    myInverseMatrix <- NULL
    
    # function to set the value of the matrix and initialize Inverse Matrix to NULL
    set <- function(y) {
        x <<- y
        myInverseMatrix <<- NULL
    }
    
    # function to get the value of the matrix
    get <- function() x
    
    # function to set the Inverse Matrix
    setInverseMatrix <- function(inverseMatrix) myInverseMatrix <<- inverseMatrix
    
    # function to get the Inverse Matrix
    getInverseMatrix <- function() myInverseMatrix
    
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## cacheSolve is a function that computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the "inverse of the matrix" and sets this "inverse value"
## in the cache via the setInverseMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the "inverse of the matrix" from cache
    m <- x$getInverseMatrix()
    
    # if cache contain previously calculated value, return the cache value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # since there is no caching of the "inverse of the matrix" available, there is a
    # need to compute and cache it
    data <- x$get() # obtain the matrix
    m <- solve(data, ...) # compute the inverse
    x$setInverseMatrix(m) # cache the inverse
    
    # return the freshly computed "inverse of the matrix"
    m
}

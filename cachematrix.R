## contains two functions which enable the retrieval of the inverse 
## of a specified square matrix that was previously cached
## for matrices whose inverse is not yet cached, code calculates
## the inverse of the specified matrix & caches result


## Returns a list of four functions capable of storing & retrieving 
## a square matrix and its inverse; formal argument is a matrix
## with an equal number of rows and columns

makeCacheMatrix <- function(x = matrix()) {
    inverse <- matrix(nrow = nrow(x), ncol = ncol(x))
    setMatrix <- function(y) {
        x <<- y
        inverse <<- matrix(nrow = nrow(y), ncol = ncol(y))
    }
    getMatrix <- function() x
    setInverse <- function(inverse_calc) inverse <<- inverse_calc
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}

## Calculates & returns the inverse of the square matrix specified
## in makeCacheMatrix if not cached; otherwise, function will return
## the inverse from cache to prevent unnecessary computing time

cacheSolve <- function (x, ...) {
    inverse <- as.matrix(x$getInverse())
    
    if(unique(sapply(inverse, is.na))[1] == FALSE &
       length(unique(sapply(inverse, is.na))) == 1) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$getMatrix()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
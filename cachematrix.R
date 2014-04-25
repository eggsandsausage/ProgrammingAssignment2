
## Functions that caches and returns inverse of a given matrix
##

## Returns an object that allows for storing and retrieving a matrix
## and its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
        invmtx <- NULL
        set <- function(y) {
                mtx <<- y
                invmtx <<- NULL
        }
        get <- function() mtx
        setInverseMatrix <- function(inverseMatrix) invmtx <<- inverseMatrix
        getInverseMatrix <- function() invmtx
        list(set = set, 
             get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## Uses the makeCacheMatrix object. Returns the inverse of the matrix,
## caches the result in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtx <- x$getInverseMatrix()
        if(!is.null(invmtx)) {
                message("getting cached data")
                return(invmtx)
        }
        mtx <- x$get()
        invmtx <- solve(mtx, ...)
        x$setInverseMatrix(invmtx)
        invmtx
}

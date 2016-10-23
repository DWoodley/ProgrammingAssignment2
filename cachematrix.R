## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a "matrix Object" that contains a list
## of "get" and "set" functions that set or return the matrix to be inverted 
## and the function ONLY sets up the "matrix Object" but does not perform a 
## roll in the inversion calculation

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # y is an invertible matrix as input
    # x is a free variable that is assigned using set function
    set <- function (y) {
        # Tests: dimensions of input matrix (y) are same as dimensions of 
        # cached matrix (X); if so it then checks that all entries in y are
        # equal to x. If either condition is false the matrix y is not the same
        # as the martix x 
        # if not, 
        if(sum(dim(x) != dim(y)) > 0 || !all.equal(x,y)){
            x <<- y
            inv <<- NULL
        }
    }
    
    get <- function () x
    setInverse <- function (Inverse) inv <<- Inverse
    getInverse <- function () inv
    
    list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}


## The parameter 'matrixObj' is a matrix object created by makeCacheMatrix,
## that may or (may not) have had its inverse previously calculated.
cacheSolve <- function(matrixObj, ...) {
    ## Return a matrix that is the inverse of 'x'
        
    # If x already has a calculated inverse, assign it to inv
    inv <- matrixObj$getInverse()
    
    ## Check to see if 
    ## Calculate an inverse for the matrix object if one does not exist
    if(!is.null(inv)) {
        message("Getting cached inverted matrix.")
        return(inv)
    }
    
    message("Calculating inverted matrix.")
    data <- matrixObj$get()
    
    inv <- solve(data)
    
    matrixObj$setInverse(inv)
    
    lastMatrix <- list()
    
    invisible(inv)
}

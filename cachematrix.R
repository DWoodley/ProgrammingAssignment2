## cachematrix.R  DWoodley 10/23/2016
## Two functions: 1. makeCacheMatrix(x = matrix()) Create matrix cache object.
##                2. cacheSolve(matrixObj) Calculate inverse of matrix.

##Example use:
##    n <- 10000
##    a <- matrix(rnorm(n*n),n,n)
##    m <- makeCacheMatrix(a)
##    cacheSolve(m) 
##    ==> 'Calculating inverted matrix.'
##    aa <- m$getInverse()
##    aa
##    ==> print out of first row of inverted matrix
##    cacheSolve(m)
##    ==> 'Returning cached inverted matrix.'


## makeCacheMatrix will create a "matrix Object" that contains a list
## of "get" and "set" functions that set or return the matrix to be inverted 
## and the function ONLY sets up the "matrix Object" but does not perform a 
## role in the inversion calculation
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (y) {
    # y is an invertible matrix as input
    # x is a free variable that is assigned using set function
    # inv is a free variable that will later store the inverted matrix
        x <<- y
        inv <<- NULL
    }
    
    # Return original matrix
    get <- function () {
        x
    }
    
    ## Function setInverse(Inverse) assigns the calculated inverse of matrix
    ## of the matrix object.
    setInverse <- function (Inverse) {
        inv <<- Inverse 
    }
    
    ## Function getInverse() returns precalculated inverse of matrix object.
    ## Returns NULL if inverse matrix has not been assigned
    getInverse <- function () {
        inv
    }
    
    ## return list of functions containing data for matrix.
    list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}

## Function cacheSolve(matrixObj, ...) will, if necesary calculate, assign 
## then and return an inverse matrix of the matrix object 'matrixObj'.
## The parameter 'matrixObj' is a matrix object created by makeCacheMatrix,
## that may or (may not) have previously had its inverse matrix calculated.
## The function will first check for the existance of an inverse matrix and
## calculate a value if necessary.
cacheSolve <- function(matrixObj, ...) {

    ## Get value of inverse matrix. May be NULL
    inv <- matrixObj$getInverse()
    
    ## Check to see if inverse matrix has been calculated for this matrix;
    ## if so do not calculate the inverse and return the existing inverse 
    ## matrix.
    if(!is.null(inv)) {
        message("Returning cached inverted matrix.")
    }
    else {
        ## This matrix does not have an inverse matrix assigned
        ## Calculate an inverse for the matrix object.
        message("Calculating inverted matrix.")
        
        ## Retrieve original matrix
        data <- matrixObj$get()
    
        inv <- solve(data)
    
        ## Assign newly calculated inverse matrix value
        matrixObj$setInverse(inv)
    }
    
    ## Return a matrix that is the inverse of 'x'
    invisible(inv)
}

## Utility function to compare two matrices.
matMatch <- function(x,y)
{
    a <- x$get()
    b <- y$get()
    
    returm (sum(dim(a) != dim(b)) == 0 && all.equal(a,b)) 
}

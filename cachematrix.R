## Functions for caching matrix inversion
# 'makeCacheMatrix' creates a list of functions
# 'cacheSolve' uses functions from 'makeCacheMatrix' to access the original
#matrix and calculate inverse if required

## Function to create a caching object
# this esentially is a list of functions 
# that set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    #Init inverse to null
    inverse <- NULL
    
    set <- function(y){
        #Use << to assign value to the already existing object
        x <<- y
        inverse <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInv <- function(y){
        inverse <<- y
    }
    
    getInv <- function(){
        inverse
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## Function to calculate inverse of matrix, if not calculated before

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check if an inverse already exists...
    # if no...calculate and set
    
    if (is.null(x$getInv())){
        message("Calculating inverse...")
        x$setInv(solve(x$get()));
    }
    
    #output the inverse
    return(x$getInv())
    
    
}

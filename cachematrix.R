## author: garyf for Coursera R class Jan 2015
## Two functions for caching and inverting a matix.
## makeCacheMatrix - holds a matrix and its inverse
## cacheSolve - fills in the object's inverse for the makeCacheMatrix Object.

## Create an object of type makeCacheMatrix that holds a matrix and might hold its inverse.

makeCacheMatrix <- function(cachedX = matrix()) {
    
    # Create a blank so we are always working against something
    myInverse <- NULL
    
    # set/ initialize
    set <- function(y){ 
        cachedX <<- y
        myInverse <<- NULL  # why not just solve() it right here ?
    }
    
    get <- function() cachedX
    
    setInverse <- function(inverse) myInverse <<- inverse
    getInverse <- function() myInverse
    
    # This is like public in other languages and exposes these methods/functions.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Solves and updates a makeCacheMatrix type object ... without typing/checking
## Checks for a cached version and uses it or if null solves and caches.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Solve for the inverse of x in ix
    invX <- x$getInverse()
    
    ## check for null (set called, or never calculate)
    if(!is.null(invX)){
      message("Using the Cached Data")
      return(invX)
    } 
    # technically ELSE
    message("Calculating the new solve/inverse; setting for X and returning")
    invX <- solve(x$get())
    print(invX)
    x$setInverse(invX)
    invX  #I wanted to return(invX) but the system seemed to complain
}




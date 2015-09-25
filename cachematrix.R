## Put comments here that give an overall description of what your
## functions do

## return a list of functions to
## be used as the input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) m <<- inverse
    
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## return inverse of a matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    if(!is.null(m)) {
        
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m<-solve(data,...)
    
    x$setinv(m)
    
    return(m) 
    
}
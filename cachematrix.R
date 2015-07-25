## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <-function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get function obtains the stored value
    get <- function() x
    #sets the function to be used 
    setsolve <- function(solve) m <<- solve
    #gets the name of fucntion to be used
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <-function(x, ...) {
    ## assign the return value of getsolve function (from makeCacheMatri) to m
    m <- x$getsolve()
    ## if m is notnull use cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives
## to matrix inversion that we will not discuss here). Your assignment is to write a pair 
##of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.


makeCacheMatrix <-function(x = matrix()) {
    #Initialize inverse mztrix (for first run)
    if (!(exists("m"))){
        m<<-NULL
    }
    #Initialize old matrix vlaue (for first run) to comapare latter
    if (!exists("oldmatrix")) {
        oldmatrix<<-x
    }
    # set current matrix value
    currmatrix<<-x
    
    #Function to get old metrix value
    getoldmatrix <- function() oldmatrix
    
    #function to get current matrix value
    getcurrmatrix<- function() currmatrix
    
    #get function obtains the stored value
    get <- function() x
    
    #sets the inverse of input matrix value
    setsolve <- function(solve) m <<- solve
    
    #gets the inverse of input matrix value
    getsolve <- function() m
    
    #output list of all required functions. This will be input to cacheSolve 
    list(get = get,
         setsolve = setsolve,
         getsolve = getsolve,getoldmatrix=getoldmatrix,getcurrmatrix=getcurrmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <-function(x, ...) {
    ##Verify if new input matrix is same as last run
    if (identical(x$getoldmatrix(), x$getcurrmatrix())) {
        ## Check if inverse was calculated
        if (!is.null(m)){
            ## If yes then use cached value
            message("getting cached data")
            return(m)
        }
    }
    # If this is a first run then get the matrix 
    data <- x$get()
    #compute inverse
    m <- solve(data, ...)
    #set the value for makecacheMatrix
    x$setsolve(m)
    #Set the value of current run as oldmatrix to comapre for next run
    oldmatrix<<-data
    #output inverse
    m
}
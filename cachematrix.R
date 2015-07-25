### Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <-function(x = matrix()) {
    #Initialize inverse metric (for first run)
    if (!(exists("m"))){
        m<<-NULL
    }
    #Initialize old matrix vlaue (for firat run) to comapare latter
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
## Write a short comment describing this function

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
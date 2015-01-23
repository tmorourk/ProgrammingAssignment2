##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix called m
    m<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    
    ## set the inverse of the matrix using the solve function
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
    
    ## get the inverse of the matrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    ## get the inverse of the matrix        
    m <- x$getInvMatrix()
    
    ## check if there is the matrix   
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if not: get the inverse of the matrix   
    data <- x$get()
    m <- solve(data, ...)
    ## set the inverse of the matrix 
    x$setInvMatrix(m)
    
}

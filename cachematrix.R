## The two functions here make use of scoping rules in R to cache the time consuming
## matrix inverse data to be reused.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    #setmatrix
    setmatrix<-function(y){
        x<<-y
        m<<-NULL
    }
    #getmatrix
    getmatrix<-function() x
    #setinversematrix using solve function
    setinversematrix<-function(solve) m<<- solve
    #getinversematrix
    getinversematrix<-function() m
    #return functions used to get and set matrix and inverse matrix
    list(setmatrix=setmatrix, getmatrix=getmatrix,
         setinversematrix=setinversematrix,
         getinversematrix=getinversematrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache. The function assumes that matrix used is always invertible
cacheSolve <- function(x, ...) {
    m<-x$getinversematrix()
    #if inverse matrix is not null then get it from cache
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$getmatrix()
    m<-solve(matrix, ...)
    x$setinversematrix(m)
    m
    ## Return a matrix that is the inverse of 'x'
}


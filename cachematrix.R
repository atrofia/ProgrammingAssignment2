## The two functions below, makeCacheMatrix and cacheSolve are used to create a special object
## that stores a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse: 
## first it sets the value of the matrix, then gets the value of the matrix, 
## sets the inverse matrix, and finaly gets the inverse matrix:

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## First, it checks if the inverse matrix has already been computed. 
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it computes the inverse matrix and sets it in the cache via the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached matix")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

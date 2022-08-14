## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that sets the input values and gets the inverse
matrix stored in the cache

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y){
                   x <<- y
                   i <<- NULL
                 }
          get <- function() x
          setinverse <- function(inverse) i <<- inverse
          getinverse <- function() i
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve takes the function makeCacheMatrix as input and checks to see if
the i (inverse) is null or not. if it not null, the cache is returned. If it is
null then the matrix input from the parent environment of the makeCacheMatrix is
taken and the inverse of the matrix is calculated and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (! is.null(i)){
        message("getting cached data")
        return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}

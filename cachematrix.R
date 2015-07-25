## The makeCacheMatrix and cacheSolve functions are used to create a special object
## which stores a matrix and caches the inverse of that matrix.

## This function takes a matrix as an argument and returns a list of functions 
## which can be used to get or set a matrix, and to get or set the inverse of that matrix. 
## Once set, the inverse of the matrix is cached in this special object and can be
## accessed using the getInverse function. The cache is cleared once a new matrix
## is stored using the set function.

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverse <- NULL
    
    set <- function(y){
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inversedMatrix) cachedInverse <<- inversedMatrix
    getInverse <- function() cachedInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes the special object from the makeCacheMatrix function
## as an argument. The function first checks if there's a cached inverse, and
## returns that matrix if found. If there's nothing cached, it computes the inverse
## using the solve function and then caches the result using the setInverse function.

cacheSolve <- function(x, ...) {
     i <- x$getInverse()
     if(!is.null(i)){
         message("getting cached data")
         return(i)
     }
     
     m <- x$get()
     i <- solve(m)
     x$setInverse(i)
     
     i
}

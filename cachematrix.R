## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

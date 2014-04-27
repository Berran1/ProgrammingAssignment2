## These functions take an invertible matrix as input, and create an 
## object that stores the matrix and either calculates its inverse, 
## or finds that inverse in cache and returns the cached inverse. 

## The makeCacheMatrix function takes an invertible matrix as input 
## and creates a list containing functions to: 
## 1. Set matrix value
## 2. Get matrix value
## 3. Set matrix inverse using solve function
## 4. Get matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function takes input from makeCacheMatrix 
## and computes the inverse of the matrix object . 
## If the inverse has already been calculated 
## (and the matrix has not changed), then it will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

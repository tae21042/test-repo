## This program will find the inverse of a matrix and put it into cache. 
## The program will retrieve the inverse from the cache instead of solving it again.

## Stores the inverse of the matrix into cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <-function()x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,getinv = getinv)
    
}


## Solves the inverse of the matrix and checks to see if there is data in cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

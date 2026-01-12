## Creates a special "vector" object that can cache its mean
makeCacheVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## Computes the mean of the special "vector" returned by makeCacheVector
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    return(m)
}
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## holds the cached inverse matrix
    set <- function(y) {                    ## assign new matrix value
        x <<- y
        inv <<- NULL                       ## reset cache for new matrix
    }
    get <- function() x                     ## return the matrix
    setinverse <- function(inverse) inv <<- inverse  ## cache the inverse
    getinverse <- function() inv            ## return cached inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Computes inverse of the special matrix, using cache if available
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)                 ## compute inverse
    x$setinverse(inv)                       ## cache it
    return(inv)
}

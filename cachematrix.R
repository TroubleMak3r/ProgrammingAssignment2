## the function checks whether the inverse of a given matrix has been produced (and cached) before. If so, it gets the 
## cached data, if not it produces and caches the inverse of that matrix so it doesn't have to compute it next time.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolution <- function(solve) m <<- solve
    getsolution <- function() m
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix, ...) {
    m <- x$getsolution()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolution(m)
    m
}

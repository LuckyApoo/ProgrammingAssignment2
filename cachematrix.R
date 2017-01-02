## makeCacheMatrix creates a special matrix object that can be input 
## to the cacheSolve function. If an inverse is already cached, cacheSolve 
## returns the inverse. Otherwise it creates the inverse.

## Creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the matrix
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        # get the matrix
        get <- function() x
        # set the matrix inverse
        setinv <- function(inverse) inv <<- inverse
        ## get the matrix inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) { 
          # Matrix inverse was calculated and cached
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Use this function to test
## Pass the output of makeCacheMatrix(mat) as input to test
# test <- function(y)
# {
#   start.time <- Sys.time()
#   
#   cacheSolve(y)
#   
#   end.time <- Sys.time()
#   time.taken <- end.time - start.time
#   time.taken
# }
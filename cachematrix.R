## The first function reads a matrix into the global environment. The second
## function will take the inverse of the matrix and read that into the global
## environment. If the cacheSolve function is run again with the same argument,
## R will retrieve the result from the global environment.

## This function takes a square matrix and reads it into the global environment.
## Each time the function is run it reads the object into the global
## environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function takes the output of makeCacheMatrix() and runs the solve
## function on it. The output is read into the global enviroment. The fun-
## -ction checks to see if the inverse is already in the global enviroment and
## will just pull it from there if cacheSolve() has already been run.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
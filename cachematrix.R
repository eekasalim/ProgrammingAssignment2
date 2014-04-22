## First function, makeCacheMatrix, creates a matrix and store inverse of the matrix in a list
## In addition, the list have 2 set functions to reset the matrix as well as the inverse

## Second function, cacheSolve, computes the inverse of a matrix in a list (output from makeCacheMatrix)
## and store the result (inverse matrix) back in the list.
## If such inverse exists in the list, then the function will simply retrieve the inverse
## from the list and output the inverse

## This function creates a special "matrix" object which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse (solve)
## 4) get the value of the matrix inverse (solve)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)

        m

}

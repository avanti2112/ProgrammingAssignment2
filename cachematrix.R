##Function 'makeCacheMatrix' will cache the inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse           ##Function setinverse() will cache the inverse of the matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Function 'cacheSolve' will calculate the inverse of a Matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }                       ## This 'if' statement checks if the inverse of the matrix has been cached or not
        data <- x$get()
        m <- solve(data, ...)   ## Calculate the inverse of a matrix using built-in R function solve() and store in object 'm'
        x$setinverse(m)
        m
}


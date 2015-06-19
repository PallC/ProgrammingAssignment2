## These functions create a special object that can store a matrix and cache its inverse.

## This function creates a special object which is essentially a list containing functions
## that set the value of the matrix, get the value of the matrix, set the value of the inverse
## of the matrix and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) 
{       m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mat) m <<- mat
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix that is stored as a special object.
## If the inverse has already been calculated, it retrieves it from the cache and skips the
## computation.

cacheSolve <- function(x, ...)
{
        m <- x$getinverse()
        if(!is.null(m))
        {
                message("retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m ## Returns a matrix that is the inverse of 'x'.
}

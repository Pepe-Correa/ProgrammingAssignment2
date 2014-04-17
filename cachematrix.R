## Put comments here that give an overall description of what your
## functions do

## This fuction create a special matrix with some attributes

makeCacheMatrix <- function(x = matrix()) {
        # my code here!
        i <- NULL
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function check if a matrix created by makeCacheMatrix has its inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

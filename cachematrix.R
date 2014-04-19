## This fuction creates a special matrix with some attributes (or sub-functions).
## In addition, you can get and set the matrix data using the following attributes: 
##      get and set, respectively. 
## Also, you can get and set the matrix inverse using the following attributes: 
##      getinv and setinv, respectively. 

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


## This function checks if a matrix created by makeCacheMatrix has its inverse (cached data).
## if not, the matrix is gotten and then its inverse is calculated, and finally it is set by 'setinv'. 

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

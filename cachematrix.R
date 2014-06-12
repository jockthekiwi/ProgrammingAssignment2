## R Programming Assignment 2
## 12 June 2014
## Author: jockthekiwi

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## m.c is the global variable for the Matix cache
        m.c <- NULL
        set <- function(y) {
                x <<- y
                m.c <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m.c <<- solve
        getinvert <- function() m.c
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinvert()
        if(!is.null(m.c)) {
                message("getting cached data")
                return(m.c)
        }
        data <- x$get()
        m.c <- solve(data)
        x$setinvert(m.c)
        m.c
        
}

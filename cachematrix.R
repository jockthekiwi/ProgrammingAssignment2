## R Programming Assignment 2
## 12 June 2014
## Author: jockthekiwi


## Function manages the global list m which caches a matrix and its inverse (m#mat and m$mat.inverse)
## The function compares x with global m and:
##      if global cache m does not exist then CREATE
##      if global cache m exits and source matrix matches new matrix then return Cached value
##      if global cache m does not match new matrix the RESET

makeCacheMatrix <- function(x = matrix()) {

        ## Does global list m exist? If no then create
        if (exists("m")==FALSE) {
                ## cached variable does not exist so create it
                m <<- list(mat=x, mat.inverse=solve(x))
                action <-  "Create"
        } 
        else {
                ## Global Cache exists
                
                ## check if matches hte passed Matrix , 
                ## 1. Check x and Global cache are matrixes
                ## 2. Check matrix dimensions are the same
                ## 3. Check if the matrix values are the same
                if (is.matrix(x) && is.matrix(m$mat) && dim(x) == dim(m$mat) && all(x == m$mat)) {
                        ## No action required - cached value exists and is an exact match of x
                        action <- "Cached"       
                }
                else {
                        ## Cache exists but does not match supplied matrix
                        ## reset the cache
                        m <<- list(mat=x, mat.inverse=solve(x))
                        action <- "Reset Cache"
                }
        }
        ## Return value to indicate operation 
        action
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Call matrix cache handler - after this call global variable
        ## m will contain be a list and matrix and its inverse (m#mat and m$mat.inverse)
        a<-makeCacheMatrix(x)
        
        ## return inverse from the global cache
        m$mat.inverse
        
}

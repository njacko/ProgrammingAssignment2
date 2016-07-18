## Nicholas Jackson
## 2016
## Programming Assignment 2 for the Coursera Course R Programming
## 

## makes a cached Matrix object (actually a list)

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse
        inv <- NULL
        ## locally defined function to set the value of x within makeCacheMatrix
        ## and clears any existing cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## locally defined function to get the value of x
        get <- function() x
        ## locally defined function to set the value of inv in makeCacheMatrix
        setinv <- function(x_inv) inv <<- x_inv
        ## locally defined function to get the value of inv
        getinv <- function() inv
        
        ## return a list object of all the functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a matrix, using a cached version if available
## the argument, 'x' must be an object created by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse value of x (if any)
        inv <- x$getinv()
        ## if this already exists, return this value
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        ## otherwise get the matrix from x and solve to get the inverse
        mat <- x$get()
        inv <- solve(mat)
        ## then store it in the makeCacheMatrix object
        x$setinv(inv)
        ## and return the inverse
        inv
}


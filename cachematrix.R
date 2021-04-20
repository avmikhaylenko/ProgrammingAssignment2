## There are two functions. One of them creates special "matrix"
## objects and and caches inverse of matrix. The second one retrieves the
## inverse of matrix from cache or calculates it


## This function creates a special "matrix" object that can cache
##its inverse. 
## 'x' is a matrix for caching. 'makeCacheMatrix' is really containing 
## the list of functions for setting the value of the matrix, for getting 
## the value of the matrix, for setting the value of inverse of the matrix
## and for getting the value of inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
        
        ## 's' is a variable for inverse of matrix
        
        s <- NULL
        
        set <- function(y)
        {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) s <<- solve
        
        getsolve <- function() s
        
        list(set = set, get = get, setsolve = setsolve,
             getsolve = getsolve)

}


## 'cacheSolve' is a function for computing of inverse of the matrix.
## If the inverse of the matrix has already been calculated, it gets
## the inverse of the matrix from the cache. Also the function checks
## if the matrix is invertible

cacheSolve <- function(x, ...) {
        
        s <- x$getsolve()
        
        if (!is.null(s))
        {
                message("getting cached data")
                return(s)
        }
        
        else
        {
                ##Checking the matrix is invertible
                
                if (ncol(x$get()) != nrow(x$get()))
                {
                        message("The matrix is not invertible")
                        s <- NA
                }
                
                else
                {
                    s <- solve(x$get())
                    x$setsolve(s)
                }   
        }
        
        ## Return a matrix that is the inverse of 'x' or NA
        
        s
}

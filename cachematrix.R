## Put comments here that give an overall description of what your
## functions do
## 
## These functions create a special matrix object that can cache its inverse.
## This avoids redundant computations by retrieving the cached inverse if it
## has already been calculated.

## Write a short comment describing this function
## This function creates a special "matrix" object that can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize inverse as NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset inverse when new matrix is set
        }
        
        get <- function() x  # Retrieve the matrix
        
        setinverse <- function(inverse) inv <<- inverse  # Set cached inverse
        
        getinverse <- function() inv  # Get cached inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves the cached value.
## Otherwise, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  # Get cached inverse
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()  # Retrieve matrix
        
        inv <- solve(data, ...)  # Compute inverse
        
        x$setinverse(inv)  # Cache the inverse
        
        inv  # Return the inverse
}


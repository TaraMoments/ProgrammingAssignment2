## This R file caches the inverse of a matrix. 

## makeCacheMatrix returns a list containing a function to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL # begin by setting the inverse of the matrix to be null. 
    
    # Define a function that sets the matrix x to be the new matrix y, and reset
    # the inverset of the matrix to be null.
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # Get the matrix x
    get <- function() x
    
    # set the inverse m to be inverse. 
    setinverse <- function(inverse) m <<- inverse
    
    # get the inverse m. 
    getinverse <- function() m
    
    # return a list that containing all the function that are just defined. 
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
 
}


## cacheSolve computes the inverse of the special ``matrix" returned by 
## makeCacheMatrix. 
## If the inverse has been already calculated, cacheSolve retrieves the inverse 
## from the cache and returns the retrieved inverse. Otherwise, it computes the 
## inverse of the matrix returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## check to see if the mean has already been calculated. 
        m <- x$getinverse() 
        ## If the inverse m has been calculated, then return the inverse. 
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
    
        # If the inverse is not calculated, computer the inverse of the matrix.
        
        # Get the matrix. 
        data <- x$get()
        # Computer the inverse of the matrix. 
        m <- solve(data)
        # Caches the inverse of a matrix
        x$setinverse(m)
        # return the inverse. 
        return(m)
} 
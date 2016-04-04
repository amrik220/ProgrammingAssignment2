## makeCacheMatrix do the following: 1. set the matrix, 2. get the matrix,
## 3. set the inverse of the matrix, 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

    invrs <- NULL
    set <- function(y){
        x <<- y
        invrs <<- NULL
        
    }
    
    get <- function() x
    setinvrs <- function(solve) invrs <<- solve
    getinvrs <- function() invrs
    
    list(set = set, get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)
}


## The following function calculates the inverse of the matrix created with the
## above function. However, it first checks if the invere is already calculated.
## If so, it gets the inverse from the cache and skips the inversion.
## Otherwise, it calculates the inverse matrix and cache it setinvrs function.

cacheSolve <- function(x, ...) {
    
    invrs <- x$getinvrs()
    if(!is.null(invrs)){
        message("getting cached data")
        return(invrs)
        
    }
    
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinvrs(invrs)
    invrs
}


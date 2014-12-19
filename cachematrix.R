## This function is used to create a cached version of matrix object. The main purpose is
## to cache the expensive calculation operations to improve the performance.
## In this function, we are caching only the matrix inverse operation.

## This function is used to create a cached matrix.
## It takes only one parameter which is the matrix for which the inverse operation is cached
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initially, the value of the inverse represented by variable i is NULL 
        i <- NULL
        
        ## The set function is used to set a new matrix; therefore, the inverse is set to NULL
        ## because the previously calculated value became invalid
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        
        ## The get function is used to return the matrix 
        get <- function() x
        
        ## This function is used to set a new value for the matrix inverse
        setInverse <- function(inverse) i <<- inverse
        
        ## This function is used to get the matrix inverse
        getInverse <- function() i
        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function is used to calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## If the returned value of the inverse is not null; then return the cached value.
        ## There is no need to calculate the inverse again
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        ## Otherwise, get the matrix and calculate the inverse usign solve functon
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

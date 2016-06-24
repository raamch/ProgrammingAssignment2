## Part of Courseera assignment 2, this code constructs a matrix 'x', 
## creates an inverse matrix 'inv' of matrix 'x' and caches it. So that
## Inverse matrix can be read from cache whenever needed with out reconsutrcting.

## makeCacheMatrix() function will contruct the special matrix 'x', 
## Inverse matrix 'Inv' and caches it. 

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initiate Inverse matrix with NULL
    inv <- NULL
    set <- function(y) {
        x <<- y       ## Create matrix
        inv <<- NULL  ## reset inverse matrix as orginal changed 
    }
    get <- function() x  ## function to read matrix
    setinverse <- function(solve) inv <<- solve  ##Inverse Matrix
    getinverse <- function() inv  ## read inverse matrix

    ## construct list and return
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## functon to check and use cache before constructing the inerse matrix

cacheSolve <- function(x, ...) {
    inv<-x$getinverse()  ## read cache

    ## check in Inverse matrix exists in cache
    if(!is.null(inv)) {
        message("getting cached data")
    
        ## return from cache
        return(inv) 
    }
    
    ## Inverse not found in cache 
    data <- x$get()
    inv <- solve(data)
    
    ## push constructed inverse matrix to cache 
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv    
}

## makeCacheMatrix takes a square matrix and cache the inverse
## of the matrix for retrieval by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL           # initiate cache as NULL
    set <- function(y) {    # function to set the matrix   
        x <<- y             # updates matrix
        cache <<- NULL      # updates cache
    }
    get <- function() x     # gets the matrix
    
    ## set inverse and update cache with inverse
    setinverse <- function(inverse) cache <<- inverse
    
    ## retrieves inverse from cache
    getinverse <- function() cache
    
    ## returns all functions created in makeCacheMatrix as list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Returns inverse of a matrix from cache if already calculated,
## and calculates inverse if no value is found in cache
cacheSolve <- function(x, ...) {
    ## returns cache if already calculated 
    cache <- x$getinverse()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    matrix <- x$get()       # retrieves matrix 
    cache <- solve(matrix, ...) # calculate inverse
    x$setinverse(cache)         #store inverse to cache
    cache
}


## MyTEST
matrix1 <- matrix(rnorm(144), 12)
matrix1
n1 <- makeCacheMatrix(matrix1)
n2 <- cacheSolve(n1)
n2
matrix1 %*% n2
round(matrix1 %*% n2)

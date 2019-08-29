## These functions are creating a special data structure
## which can store matrix and its inverse and recall
## it without repeating the computation

## z<-makeCacheMatrix(mymatrix) creates an object containing a 
## matrix which can be accessed using z$get() and its
## inverse set by z$setinverse(inv) and recalled by
## z$getinverse(inv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the matrix
## passed by the object defined above and stores its
## value to its inv attribute. If this inv already exists
## it doesn't calculate again and just recalls the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix,...)
    x$setinverse(inv)
    inv
}

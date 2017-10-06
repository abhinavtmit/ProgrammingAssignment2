## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##input should be matrix
        inver <- NULL                         ## initialize inver as NULL. It will hold value of inverted matrix 
        set <- function(y) {                  ## define the set function to assign matrix
        x <<- y                               ## value of matrix in parent environment
        inver <<- NULL                        ## if there is a new matrix then reset inv to NULL
                           }
        get <- function() x                   ## get the value of the matrix
    
        setinverse <- function(inverse) inver <<- inverse  ## assign value of inver in parent environment
        getinverse <- function() inver                     ## get the value of inver when called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                ## use makeCacheMatrix to invert matrix
        ## Return a matrix that is the inverse of 'x'
                inver <- x$getinverse()         ##get natrix inverse from list
                if(!is.null(inver)) {           ##if not null then get cache value
                message("getting cached data")  
                return(inver)                   
                                  }
        data <- x$get()                         ##if not calculated, then get actual matrix
        inver <- solve(data, ...)               ##calculate inverse of matrix
        x$setinverse(inver)                     ##set inverted matrix to setinverse; caching the inverse
        inver                                   ##Print matrix inverse
}

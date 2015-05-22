## Function "makeCacheMatrix" creates a list containing functions to:

## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y){     ## set function sets values of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() {     ## get values of matrix x
        x
    }
    setinv <- function(inverse){    ## set inverse matrix
        inv <<- inverse
    }     
    getinv <- function(){    ## get inverse matrix
        inv
    } 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function "cacheSolve" returns the inverse of a matrix 'x' of 
##the matrix in the "makeCacheMatrix" function. It first checks to see if the 
## inverse has been calculated, and if so, returns the inverse matrix stored in 
## the cache. If the inverse matrix has not been stored in the cache, it uses 
## the Solve function to calculate the inverse, and will set the inverse in the 
## cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {             ## check if inverse is cached
        message("getting cached data")
        return(inv)                 ## if inverse is cached, return inverse and 
    }                               ## exit program
    data <- x$get()
    inv <- solve(data, ...)         ## if inverse is not in cache, get matrix 
    x$setinv(inv)                   ## data, compute inverse of matrix, and call
    inv                             ## set function to store inverse in cache
}

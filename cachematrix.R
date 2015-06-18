## We have two functions here which are used to create a special object that stores
## a matrix and chache's its inverse

## This function creates a special "matrix", which is really a list of the four 
## following functions:
## "set": sets the value of the matrix
## "get": gets the value of the matrix
## "setInverse": sets the value of the inverse
## "getInverse": gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##setting the value of the matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    ##getting the value of the matrix
    get <- function() x
    ##Set the inverse
    setInverse <- function(inverse) i <<- inverse
    ##Get the inverse
    getInverse <- function() i
    ##return the list of four functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInverse function. If the inverse
## is retrieved from the cache, the function also prints out the message which 
## tells that the inverse is from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    ##calculate the inverse
    i <- solve(data, ...)
    ##set the calculated inverse for future uses
    x$setInverse(i)
    i
}

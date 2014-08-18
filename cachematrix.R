## The function takes out the inverse of a matrix and stores them as cache if
## the inverse for same matrix is to be determined it prints the existing value

## makeCacheMatrix store the matrix and its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y){
       x <<- y
       i <- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set=set, get=get, setinverse=setinverse,
        getinverse=getinverse)
}

## cacheSolve checks if the inverse exists if yes then publishes it 
## otherwise determines it and pushes it to setinverse for storing it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()   
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

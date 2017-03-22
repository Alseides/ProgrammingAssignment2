## This two functions caches the inverse of a matrix.
##This could be handy as needed.

##The function makeCacheMatrix works as follows:
##Set the value of the matrix
##Get the value of the matrix
##Set the value of the inverted matrix
##Get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Assuming that the given matrix is always invertible, this fuction returns the inverted matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Sample Run:
##> x<-rbind(c(4, 3), c(3, 2))
##> x
##[,1] [,2]
##[1,]    4    3
##[2,]    3    2
##> m<-makeCacheMatrix(x)
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##>
##Source: http://www.mathwords.com/i/inverse_of_a_matrix.htm

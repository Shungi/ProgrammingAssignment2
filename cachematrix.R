#Notes below functions. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

## ****makeCacheMatrix function creates a special "matrix"**** 
##     object that can cache its inverse

## Takes argument x which is a matrix object (assumed invertible),x default 
## is a 1x1 matrix containing NA

##set() takes an argument y which is a matrix
##      and sets x to y, and i to NULL used to edit
##      matrix x without a second call to makeCacheMatrix

##get() retrieves returns the value of x  

##setinverse() takes in the argument inverse and sets it to that value

##getinverse() returns the value of i 

## makeCache returns a list of the functions contained herein 

## ****cacheSolve computes the inverse of the special ****
##    "matrix" returned by makeCacheMatrix above. 

##takes in x, a makeCacheMatrix object as it's argument

## i is retrieved from the getinverse function contained within x
##      if it is NOT null the function simply returns the value of i saved in x

## data is the value of the matrix contained in x retrieved using it get()
## i is set to the inverse of data and the inverse is stored within x using 
## setinverse within x and i is returned. 
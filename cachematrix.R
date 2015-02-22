## OVERALL DESCRIPTION
## This file contains two functions.
## 
## 1. makeCacheMatrix() optionally takes a matrix as argument and 
## returns a list containing 4 functions that allow to read/write
## the value and inverse of this matrix. If a matrix is not provided
## initially, then it can later be set using the set() function in the
## returned list as described below. The list returned by this function
## can be used as an object for storing a matrix and for caching its
## inverse.
##
## 2. cacheSolve() is used to calculate the inverse of a "matrix"
## created by makeCacheMatrix() function. This function is intelligent
## enough to use cached inverse between multiple lookups if the original
## matrix has not changed in the meanwhile.


## This function optionally takes a matrix as its first argument
## and returns a list containing 4 functions. Its argument is the
## matrix whose value and inverse it caches. Description of the four
## functions that it returns in a list is given below:
##
## 1. set(): this function takes a new matrix as argument and 
##           stores it in its defining scope (makeCacheMatrix function)
##           Additionally, it also resets the cached value of inverse to
##           NULL so that future lookups don't get the outdated cached
##           inverse
## 2. get(): this function returns the matrix stored in its defining
##           scope
## 3. setinverse(): this function takes a matrix as argument and
##           stores it as the inverse of the other matrix in its
##           defining scope
## 4. getinverse(): this gunction returns the cached inverse of matrix
##           from its defining scope

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
    setinverse = setinverse, getinverse = getinverse)
}


## This function takes a "Matrix" object created by the makeCacheMarix()
## function and returns its inverse. It first looks up the inverse in 
## cache. If the inverse is found in the cache then it returns it.
## Otherwise, it retrievese the matrix data, calculates its inverse,
## stores the inverse in cache for further lookups, and then returns the
## inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

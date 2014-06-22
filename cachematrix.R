
# Function makeCacheMatrix(x)  
# This function creates a special "matrix" object that can cache its inverse
# Here, x is a matrix
# This function returns a vector of functions:
# 1.set: set the value of the matrix
# 2.get: get the cached value of the matrix
# 3.setinverse: set the inverse of the matrix
# 4.getinverse: get the cached value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    # initialize inverse matrix to NULL
    x_inv <- NULL

    # function set(), sets the value of the matrix
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }

    # function get(), get the cached value of the matrix
    get <- function() {
        x
    }    

    # function setinverse(), set the inverse of the matrix
    setinverse <- function(solve) {
        x_inv <<- solve
    }

    # function getinverse(), get the cached value of inverse of the matrix
    getinverse <- function() {
        x_inv
    }

    # return a vector of functions
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}



# Function cacheSolve(x, ...)
# where, x is a matrix
# This function returns a matrix that is the inverse of x.
# If the inverse has already been calculated (and the matrix has not changed), 
# then it retrieves the inverse from the cache.
# NOTE: The function assumes that the matrix x is inversible

cacheSolve <- function(x, ...) {

    x_inv <- x$getinverse()
    
    if(!is.null(x_inv)) {
        message("cacheSolve: getting cached matrix inverse")
        return(x_inv)
    }

    data <- x$get()

    x_inv <- solve(data, ...)
    
    x$setinverse(x_inv)
    
    message("cacheSolve: getting newly computed matrix inverse")
    x_inv
}



# Function testme(a)
# where, a is a matrix whose inverse is to be computed
# This function tests the cache functionality of matrix inversion.
# NOTE: The function assumes that the matrix a is inversible

testme <- function(a) {
    b <- makeCacheMatrix(a)
    
    # fist call to cacheSolve will compute the inverse of matrix
    c <- cacheSolve(b)
    print(c)
    
    # second call to cacheSolve will retrieve the inverse of matrix from cache
    c <- cacheSolve(b)
    print(c)
}

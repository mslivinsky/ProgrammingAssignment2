# makeCacheMatrix takes any matrix as an input and returns a list 
# containing 4 functions:
# 1. set: set the value of the matrix
# 2. get: get the value of the matrix
# 3. setinv: set the value of the matrix inverse
# 4. getinv: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}

# cacheSolve takes as input the output list of functions from makeCacheMatrix
# cacheSolve checks if the matrix inverse has been calculated, in which case it
# returns the value (by calling $getinv()).
# If $getinv() is empty, it pulls the original matrix by calling $get() and 
# calculates the inverse matrix using solve(), storing it in the inv variable. 
# Then it calls $setinv(inv) to modify inv in the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

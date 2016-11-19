## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix effectively creates a matrix object, technically a list that
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. sets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(matrix_inverse) inv <<- matrix_inverse
        getinv <- function() inv
        list(set = set,
                get = get,
                setinv = setinv,
                getinv = getinv)
}


## Calculates the inverse of a matrix. 
## Where there is already a value provided, immediately returns the result
## Else, calculates the inverse and saves the result.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function set and get a matrix and its inverse
## 1st initilize the inverse
## 2nd set the matrix
## 3th get the matrix
## 4th set the inverse of the matrix
## 5th get the inverse of the matrix
## 6th return a list of the methods in the function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMat <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        getMat <- function() { x }
        inverseMat <- function(inverse) { m <<- inverse }
        returnInv <- function() { m }
        list(setMat = setMat, getMat = getMat, inverseMat = inverseMat, returnInv = returnInv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix returned by makeChaceMatrix
## 1st get the matrix that is the inverse of x
## 2nd If it is set return the inverse
## 3th get the matrix from the object
## 4th calculate the inverse using solve
## 5th set the inverse matrix to the object
## 6th return the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMat()
        m <- solve(data,...) 
        x$inverseMat(m)
        m
}

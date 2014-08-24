## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                invrs <- NULL
                set.matrix <- function(matrix) {
                           x <<- matrix
                           invrs <<- NULL
                }        
                get.Matrix <- function() {
                           x
                }
                set.Inverse <- function(inverse) {
                invrs <<- inverse
                }
                get.Inverse <- function() {
                invrs
                }
                list(set = set.Matrix, get = get.Matrix, setinverse = set.Inverse, getinverse = get.Inverse)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## 
           inverse <- x$get.Inverse()
           if (!is.null(inverse)) {
              message("getting cached data")
              return(inverse)
           }
           data <- x$get.Matrix()
           inverse <- solve(data)
           x$set.Inverse(inverse)
           inverse
}       

# The following functions solve the problem A*B=I, where:
#   A: is a square matrix (user-supplied)
#   B: is the inverse of A
#   I: is the identity matrix
#
# Usage:
#   methods_list <- makeCacheMatrix()             # initializes a list with the set and get methods
#   methods_list$set(direct_matrix)               # saves the input matrix
#   methods_list$get()                            # returns the saved direct matrix
#
#   cacheSolve(methods_list)                      # solves for the inverse and returns it
#
# To test the result:
#   direct_matrix %*% inverse_matrix              # should give 1.0 on the main diagonal and
#                                                 # very low numbers (<=1E-15) on the off-diagonal positions
#

# This function saves the direct matrix in a separate environment. It also creates
# a list containing all necessary methods to set and get the direct and inverse matrices.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {   # method to set the direct matrix, when not supplied directly as argument to the function makeCacheMatrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x   # method to return the direct matrix
        setinverse <- function(inverse) inv <<- inverse   # method to set the inverse matrix
        getinverse <- function() inv   # method to return the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function checks whether the inverse matrix has already been calculated,
# and in this care it returns it directly (with a message declaring it)
# Otherwise it calculated the inverse matrix using R's "solve" function
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   # gets the inverse matrix if already available
        if(!is.null(inv)) {   # checks whether the inverse matrix already exists
                message("getting cached data")
                return(inv)
        }
        data <- x$get()   # gets the direct matrix
        inv <- solve(data, ...)   # calculates the inverse matrix using the R function solve
        x$setinverse(inv)   # saves the inverse matrix
        inv
}

####################################################################################################
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly 
#
#  In this file a pair of functions that cache the inverse of a matrix are written.
# The function names are as follows:
# 1. makeCacheMatrix
# 2. cacheSolve
#################################################################################################



###############################################################################################
# Function makeCacheMatrix
###############################################################################################

#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse matrix
#4.  get the value of the inverse matrix

#
##############################################################################################


makeCacheMatrix <- function(x = matrix()) {
      
            inverse <- NULL
            setmatrix <- function(y) {
                  x <<- y
                  inverse <<- NULL
            }
            getmatrix <- function() x
            setinverse <- function(solve) inverse <<- solve
            getinverse <- function() inverse
            list(setmatrix = setmatrix, getmatrix = getmatrix,
                 setinverse = setinverse,
                 getinverse = getinverse)
      
 }

###############################################################################################
# Function cacheSolve
###############################################################################################
#The following function calculates the inverse of the special "matrix"
#created with the makeCacheMatrix function. 

#However, it first checks to see if the inverse has already been calculated. 
#If so, it `get`s the inverse from the cache and skips the computation. 
# A message "getting cached data" is printed out when it gets the inverse from the cache

#Otherwise, it calculates the inverse of the matrix, 
#and sets the inverse in the cache via the setmatrix function
##############################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$getmatrix()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

#############################################################################################

# Programming Assignment 2: Lexical Scoping
#    by J. York
#
# R Programming tought by Johns Hopkins on Coursera
#
#
# The purpose of these functions is to create a special matrix object to return
# a matrix inverse either by calulating the inverse or returning a previously 
# calculated cached matrix inverse.
#
# Usage example:
#
#       # Create a square matrix
#       B = matrix(c(1,2,3,4),2,2)
#
#       # Create the special "matrix"
#       b = makeCacheMatrix(B)
#
#       # Invoke the get function to return the matrix
#       b$get()
#
#       # Try to get the cached inverse (Just to show it doesn't exist yet)
#       b$getInv()
#
#       # Calculate the inverse matrix (first time run no cache so its calculated)
#       cacheSolve(b)
#
#       # Calculate the inverse matrix (second time run it's retrieved from cache)
#       cacheSolve(b)
#
#       # set b to a new matrix with the set function (clears cache)
#       b$set(matrix(c(4,5,6,7),2,2))
#

# makeCacheMatrix: This function creates a special "matrix" object that can
#                  cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        # clear any previous cache
        inverse_matrix <- NULL
        
        # creates a new matrix copy and clears cached inverse matrix
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        # returns the original matrix passed in
        get <- function() x
        
        # calculates and caches the inverse of matrix x
        setInv <- function(solve) inverse_matrix <<- solve
        
        # gets the inverse
        getInv <- function() inverse_matrix
        
        # returns a list of the 4 functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)        

}


# cacheSolve: This function computes the inverse of the
#             special "matrix" returned by makeCacheMatrix above. If the inverse has already
#             been calculated (and the matrix has not changed), then the cachesolve should
#             retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Get the cached inverse matrix
        inverse_matrix <- x$getInv()
        cache_exists <- !is.null(inverse_matrix)
        if(cache_exists) {
                message("getting cached inverse matrix")
                return(inverse_matrix)
        }
        # Cache does not exists, so calculate inverse
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        
        # Use the setInv to store the new inverse in cache and return inverse
        x$setInv(inverse_matrix)
        inverse_matrix       
}

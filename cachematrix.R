##########################################################################
#makeCahceMatrix stores 4 functions into cache as a list:                #
#get() returns the matrix stored into the function                       #
#set() assigns a new value for matrix x and resets the calculated inverse#
#setinverse() Assigns a new value for the inverse                        #
#getinverse() Returns the inverse value stored in the function           #
#                                                                        #
#Where: args(x = inversible matrix)                                      #
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#######################################################################
# cacheSolve checks the value of matrix x against the cached matrix   #
# Matrix x from the previous function. If the cache is not null it    #
# returns the cached value containing the inverse of matrix x,        #
# If the cache is null it solves for matrix x                         #
#                                                                     #
#Args = cacheSolve(stored output of makeCacheMatrix() )               #
#######################################################################

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
#END#

# The following four lines simply test the function

b<-matrix(c(4,2,7,6),2,2)
c<-makeCacheMatrix(b)
cacheSolve(c)
cacheSolve(c)
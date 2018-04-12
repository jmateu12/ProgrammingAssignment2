## Thank you for grading my assignment! And sorry for my bad English (it is not
## my mother tongue language).

# Overall description of what the following functions do:
# The following two functions are aimed to cache potentially time-consuming
# computations when calculating the inverse of a matrix. After the inverse of an
# specific matrix has been calculated, it is cached so it has not to be computed
# again next time.

# makeCacheMatrix creates a "matrix", which is really a list containing four
# different functions to:
#         1.set the values of the matrix
#         2.get the values of the matrix
#         3.set the values of the inverse of the matrix
#         4.get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve calculates the inverse of the "matrix" that is created with
# makeCacheMatrix. It first checks to see if the inverse has already been
# calculated. If so, it gets the inverse from the cache and skips the
# computation. Otherwise, it calculates the inverse of the matrix and sets it
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# Example of some input and code to run the functions:
# source('[path of the file]')
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# 
# The inverse of m1 should be:
#       [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# 
# We start running the functions:
# myMatrix <- makeCacheMatrix(m1)
# The matrix is created, but we do not have its inverse yet!
#
# myMatrix$get()
# This returns the matrix.
#
# myMatrix$getinverse()
# This returns "NULL", since the inverse has not been calculated yet.
# 
# So now, we calculate the inverse:
# cacheSolve(myMatrix)
# It returns the inverse of the matrix which we used as an input.
#
# cacheSolve(myMatrix)
# Now that the function is called again, the message "getting cached data" is
# shown, because the inverse has already been calculated before and it can be
# retrieved from the cache.
# 
# myMatrix$getinverse()
# Now, instead of "NULL", the inverse of the matrix is returned, since it has
# been calculated.
#
# Thank you again!!! :)

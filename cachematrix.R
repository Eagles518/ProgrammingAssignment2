# Overall, these functions allow the user to 
# create and cache a matrix and its inverse. 
# 
# This function takes a matrix as its input argument, and
# outputs a list.
# It sets the inverse variable to NULL to initialize
# the matrix's inverse to null. A set function is created
# to set a new matrix and re-initialize the inverse in its 
# parent environment.The get function just retrieves the 
# original matrix. setInverse accepts a new inverse as its argument
# (denoted as solve) and sets the new inverse in the parent
# environment. getInverse returns the inverse. 


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i

        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# 
# This function first checks if the inverse of
# the cached matrix is NULL. If so, it solves 
# for the inverse and caches it. If it is not NULL,
# a brief message is returned along with
# the cached inverse. 

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
              message("getting cached data")
              return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setInverse(inverse)
      inverse
}

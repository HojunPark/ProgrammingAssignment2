## This function creates a special "matrix" object, which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { # create a matrix object x    
  # define the cache m
  m <- NULL
  set <- function(y) { # set x
    x <<- y # assign the input matrix y to the variable x in the parent environment
    m <<- NULL # re-initialize m in the parent environment to null
  }
  get <- function() x # get x ; return the matrix x
  setinverse <- function(inverse) m <<- inverse # set the m equal to the inverse of the x
  getinverse <- function() m # return the inverse of x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # Return the inverse of x
  if(!is.null(m)) { #if m is empty, print message
    message("getting cached data")
    return(m)
  }
  data <- x$get() #Return the x
  m <- solve(data, ...) #Computing the inverse of the matrix data with solve function
  x$setinverse(m) #set the inverse of x to m
  m #print m
}
## Assignment 3: Caching the Inverse of a Matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  
  # sets the value of the vector
    set <- function(y) { 
    x <<- y # super-assignment of x 
    inverseMatrix <<- NULL # super-assignment (initalize) of inverseMatrix 
  }

  # gets the value of the vector
  get <- function() x 
  
  # sets the value of the of inverse matrix
  setInverseMatrix <- function(inverse)  inverseMatrix <<- inverse  # super- assignment of inverseMatrix
  
  # gets the value of the inverse matrix
  getInverseMatrix <- function()     
    inverseMatrix # simply return the inverseMatrix
      
  #returns the list of functions
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  
  inv <- x$getInverseMatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # get the vector of the our data 
  inv <- solve(data) # solve our inverse matrix
  x$setInverseMatrix(inv) # finally set our inverse matrix value back to x
  inv # return the matrix
}

## Test Scenarios
# > x = cbind(c(2,3,2), c(1,2,1), c(1,1,2))
# > x
# [,1] [,2] [,3]
# [1,]    2    1    1
# [2,]    3    2    1
# [3,]    2    1    2
# > m <- makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    2    1    1
# [2,]    3    2    1
# [3,]    2    1    2
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]    3   -1   -1
# [2,]   -4    2    1
# [3,]   -1    0    1
# > cacheSolve(m)
# getting cached data
# [,1] [,2] [,3]
# [1,]    3   -1   -1
# [2,]   -4    2    1
# [3,]   -1    0    1
 


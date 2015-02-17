
##  makeCacheMatrix creates a special "matrix" object that can cache its inverse, 
## the returned object is a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the marix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##sets 'i' to NULL as a placeholder for the inverse
  set <- function(y) { ##function to set the value of the matrix
    x <<- y #the matrix is assigned to 'x'
    i <<- NULL ##and the 'i' is NULL as a placeholder for the future inverse calculation
    ## '<<-' operator is used because the variables are defined in a higher level environment than the current function
  }
  get <- function() x ##function to get the value of the matrix
  setinverse <- function(inverse) i <<- inverse ##function to set the value of the inverse
 ## '<<-' operator is used because the i' variable is defined in a higher level environment than the current function
  
  getinverse <- function() i ##function to get the value of the inverse
  list(set = set, get = get, ## creates a list of the functions
       setinverse = setinverse,
       getinverse = getinverse)
}



## function cacheSolve returns the inverse of the square matrix 'x'


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## checks if the inverse has already  been calculated
  i <- x$getinverse()
  
  if(!is.null(i)) { ## check if the inverse has been calculated already
    ## if the inverse already exists, exits the function returning the cached value of the inverse,
    ## and skips the calculation
    message("getting cached data")
    return(i)
  }
  
  ## the following lines are executed if the inverse has not been calculated previously
  data <- x$get() ## the matrix is read using 'get' and stored in the 'data' variable
  i <- solve(data, ...) ##the matrix inverse is obtained using the 'solve' function and assigned to 'i' variable
  x$setinverse(i) ## the calculated inverse is set in the cache matrix using 'setinverse'
  i # the inverse matrix is returned
  
}

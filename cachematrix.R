## These two functions compute the inverse of a non singular matrix, it also saves the results in the cache. 
## If the inverse of the same matrix has to be computed twice it returns the value stored in the cache instead of doing the 
## computation all over again

## makeCacheMatrix function which takes as input an invertible matrix and returns a list of four objects described as below
## get :  a function to read the input matrix
## set : It sets(initializes) the inverse to null 
## setinv : calculates the inverse using the solve function 
## getinv : returns the inverse matrix calculate in the previous step.

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## this function checks if the inverse of the matrix already exists in the cache, if the corressponding value is not null 
## then a message is displayed saying that the inverse is being obtained from the cache. if the inverse does not exist in the 
## it is calculated using the solve function and is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv() 
#  print(s)
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

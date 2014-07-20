## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below are a pair of functions that cache the inverse of a matrix.



## This function creates a CacheMatrix object, containing the value of the matrix
## passed to it as an argument and methods to:
##    get and (re)set the matrix value; 
##    cache the solution to the matrix, the solution being passed as an argument; and
##    pull out the cached solution, if a solution has indeed been cached

makeCacheMatrix <- function(x = matrix()) {

  ## when we create a CacheMatrix object to start with its solution is unknown
  ## so set solution to NULL 
  solution <- NULL

  ## now define a method to update the value of the matrix in case we need it
  set <- function(y){
    
    ## update the value of x and flag that the solution to this new matrix is not
    ## cached in the parent environment i.e., in the CacheMatrix object
    x <<- y
    solution <<- NULL
  }
  
  ## now a method to retrieve the value of the matrix via an anonymous function
  get <- function() x
  
  ## and a method to store a calculated solution to the matrix in the CacheMatrix object
  setsolution <- function(thesolution) solution <- thesolution
  
  ## now a method for returning the value of the stored solution (or NULL if not yet set)
  getsolution <- function() solution
  
  ## return a list containing the methods
  list(set=set, get=get,
       setsolution=setsolution,
       getsolution=getsolution)
}


## This function, which is passed a CacheMatrix object, first checks to see if a solution to
## the matrix has already been calculated and cached away. If it has the function returns that
## cached value and displays a message confirming that. If the solution has not yet been
## calculated, the function does so, caches the result and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## first check to see if a solution has already been cached and, if it has,
  ## return that value straightaway
  solution <- x$getsolution()
  if(!is.null(solution)){
    message("getting cached data")
    return(solution)
  }
  
  ## otherwise we need to get the value of the matrix and calculate its solution
  thematrix <- x$get()
  solution <- solve(thematrix)
  
  ## and then cache the solution in the CacheMatrix object
  x$setsolution(solution)
  
  ## return the solution to the matrix
  solution
  
}

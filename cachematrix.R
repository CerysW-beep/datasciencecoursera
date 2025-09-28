## In the section below I have written 2 functions that cache the inverse of a matrix.  
## One of the purpose of a cache is is to speed up repeated tasks especially if it takes a long time to compute.
## This is because the scoping rules of R means that R will retrieve the value of an R object first.  


## The first script (makeCacheMatrix) creates a special object that stores a matrix and can cache its inverse.
## This contains a list of four functions that manage both the matrix and its inverse. 

##These functions are
## set(y) which sets a new matrix y and clears any cached inverse
## get() returns the current matrix
## setsolve(solve) stores the inverse of the matrix
## getsolve() received the cached inverse if it is available

## makeCacheMatrix starts with no cached inverse, the matrix is updated using the function set(), This function also clears the cached inverse.
## The function setsolve() will store an inverse which has been computed via the second function cacheSolve
## Future calling of the cached inverse is done using the getsolve() saving time by not having inverse the matrix again. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL  
    }
  get <- function() x
  setsolve <- function(solve) s <<- solve  
  getsolve <- function() s
  list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The purpose of this function is to compute the inverse of a matrix or to retrieve a cached inverse of a matrix if one is available.
## An explanation on what each step in the function does is given here
  ## x$getsolve() will try to call the cached inverse
  ## The message("getting cached data") informs the user that cached data is being used.
  ## return(s) will return the cached inverse
  ## s <- solve(data, ...) will compute the inverse
  ## x$setsolve(s) will cache the inverse
  ## s will  return the newly cpmuted inverse. 
  
  
cacheSolve <- function(x, ...) {
  s <- x$getsolve()  
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}     

## To test my function I decided to create and cache a matrix.  Using the function below I created a 2x2 matrix and passed it to makeCacheMatrix
## which stores it in x. The function then prepares the object to cache the inverse later using casheSolve()


cachedMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))



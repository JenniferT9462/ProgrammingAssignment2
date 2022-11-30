## The first function, makeCacheMatrix() creates a R object that stores a vector and its mean.
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix()
## in order to retrieve the mean from the cached value that is stored in the makeCacheMatrix()
## object's environment.

## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   ## Define the argument x to "matrix"
  m <- NULL                                   ## Initialize m to NULL; hold value of matrix inverse.
  set <- function(y) {                        ## Define set function to assign new.
    x <<- y                                   ## Value of matrix in parent environment.
    m <<- NULL                                ## If there is a new matrix, reset to NULL.                                
  }
  get <- function() x                         ## Define the get function, returns value of the matrix argument.
  setmean <- function(mean) m <<- mean        ## Assigns value of m in parent environment.
  
  getmean <- function() m                     ## Gets the value of m when called.
  list(set = set,                             ## You need this in order to refer to the function with the $ operator.
       get = get,                             
       setmean = setmean,
       getmean = getmean)
}

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix() above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve() will retrieve the incverse from the cache. 

cacheSolve <- function(x, ...) {                              ## Argument of x and a ellipsis which allows the caller to pass additional arguments to the function.
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()                                            ## Call the getmean() function on the input object x with the $ operator.
  if(!is.null(m)) {                                           ## Use "If" to check if result m  is NULL;  
    message("getting cached data")
    return(m)
  }
  data <- x$get()                                             ## If  the result of !is.null(m) is FALSE, cacheSolve gets the vector from the input object, 
  m <- mean(data, ...)                                        ## calculates a mean(), 
  x$setmean(m)                                                ## uses setmean() function on the input object to set the mean in the input object,
  m                                                           ## and returns the value of the mean to the parent environment by printing the mean object.
}


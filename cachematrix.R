## Add Comments here that give an overall description of what your
## functions do
## 
## these functions allow you to mimic "storing state" of variables so that you 
## refer to the variables the next time the function is called 

## Write a short comment describing this function
##   
## description : This function creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
# solve(X) returns inverse 
  
  # some comments relate to conceptually translating this to their OO counterparts 
  
  # initialize 
  m <- NULL 
  
  # define 4 "properties" of this "object" 
  # here each "property" is a "function" 
  # - technically 2 "pairs" of getter / setter functions 
  #   to underlying data values "x" and "m", where 
  #     x is the matrix 
  #     m is the inverse matrix 
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  # return "object" 
  # here, a list is returned with reference to 4 "properties" of this "object" 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##   
## description :  
##    This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. 
##    
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # retrieves the cached inverse matrix if already computed, else NULL 
  m <- x$getInverse()

  # if cached inverse matrix is found... 
  if(!is.null(m)) 
  {
    # cached inverse matrix found - return cached inverse matrix 
    message("getting cached data")
    return(m)
  }
  
  # if cached inverse matrix is not found, retrieve the matrix of data 
  data <- x$get()

  # compute inverse matrix 
  m <- solve(data, ...)
  
  # save / cache inverse matrix 
  x$setInverse(m)

  # return m, the result
  # here, the inverse of the input Matrix 
  m
}

# Example: Caching the Mean of a Vector
# 
# In this example we introduce the <<- operator which can be used to assign a value 
# to an object in an environment that is different from the current environment. 
# Below are two functions that are used to create a special object that stores a 
# numeric vector and cache's its mean. The first function, makeVector creates a 
# special "vector", which is really a list containing a function to 
# 
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips 
# the computation. Otherwise, it calculates the mean of the data and sets the
# value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

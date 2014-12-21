## Creates a function makeCacheMatrix that does the following
## 1. creates a variable 'i', which will be used to cache the inverse of matrix(x)
## 2. creates a function set() to set the value of matrix(x)
## 3. creates function get() to get the value of matrix(x)
## 4. creates function setInverse() to assign calculated inverse of matrix(x) from function 'cachesolve' to 'i'
## 5. creates function getInverse() to obtain inverse of matrix(x) stored as 'i'

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(M) {
    x <<- M
    inv <<- NULL
  }
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## creates a funciton "cacheSolve" that apples to function "makeCacheMatrix" to
## 1. if no data is stored as "i", calculcates the inverse of matrix x, then stores the result as "i"
## 2. if data is already stored as "i", returns "i"

cacheSolve <- function(y, ...) {
  i <- y$getInverse()
  if(!is.null(i)){
    message("cached data found, returning cached data")
    return(i)
  }
  else{
    message("no cached data found, calculating inverse matrix")
    data<-y$get()            # obtains matrix value from function y
    i <- solve(data)         # finds inverse of matrix x
    y$setInverse(i)          # sets inverse i  
    return(i)                # return a matrix that is the inverse of 'x'
  }
}

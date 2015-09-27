## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  
  set<-function(y)
  {
    x<<-y
    inv <-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inverse) inv<<-inverse
  
  getinverse<-function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve is a function that returns the inverse of a matrix. It checks 
# if the inverse has been calculated. If so, it gets the inverse from the 
# cache and skips computation. Otherwise, it calculates the inverse of the 
# matrix and sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
  
}

## The overall purpose of both of these functions is to input a matrix into the first function in order to 
##store the data in set variables. This data is used in the second function in order to check and see if the
##inverse of the matrix has already been cached, and if not to inverse the matrix, cache it, and return the
##inverse matrix at the end.

## The makeCacheMatrix funtion is set up to make a special matrix that will store the data that is
##input into set variables for later use in the following cacheSolve function. The list this function produces
##includes the data for these:
##1. set the value of the matrix 
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y) {
    x<<-y
    i<<-NULL

}

get<-function()x
setmatrix<- function(inverse) i<<- inverse
getmatrix<- function() i
list(set=set,get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)

}


## The cachesolve function catches the inverse of the matrix that was created using the makeCacheMatrix function.
## Its first step is to see if the cached matrix has already been created, and if it has, it returns it.
## If the cached matrix has not been created, it gets the data and inverses the matrix, caches the data, and returns it.

cacheSolve <- function(x, ...) {
      i<-x$getmatrix()
      if(!is.null(i)){
        message("getting cached data")
        return(i)
      }
      data<-x$get()
      i<-solve(data,...)
      x$setmatrix(i)
      i
}

##Ilagan, Shaena Ellaine C.
##Set the function that can change the cached value of matrix either null or inverse matrix
##functions will be set as a and b

makeCacheMatrix <- function(x = matrix()) {
  inverse <- null
  get <- function() b
  set <- function(a) {
    x <<- b
    inverse <<- null

##to get the cached inverse of a matrix determine the if it is an inverse function
  getinverse <- function() inverse
  setinverse <- function(inverse) {
    inverse <<- inverse

##set inverse to get access to the object by the cacheSolve function
cacheSolve <- function(a,...) {
  inverse <- a$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  
  m <- solve(x$get())
  a$setinverse(m)
  #return(m)

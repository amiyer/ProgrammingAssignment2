## Put comments here that give an overall description of what your
## functions do
## Following two functions are created in order to store the inverse matrix value
## in the memory and ability to set and get the stored values so that 
## we can avoid doing any repetitive computations when values are required until
## the data(matrix) is changed


## Following function stores matrix in the memory variable mt
## also stores its inverse matrix in memory variable in inver
## In addition of storing values it provides functionality to set
## and get values of matrix and set and get inverse matrix values
makeCacheMatrix <- function(mt = matrix()) {
  inver <- NULL
  set <- function(newmt) {
    mt <<- newmt
    inver <<- NULL
  }
  get <- function() mt
  setinverse <- function(inversionmt) inver <<- inversionmt
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Following function stores expects parameter of type "makeCacheMatrix" function/object
## it first retrieves any stored value of inverse matrix
## if it is not null then it returns the stored inverse matrix value
## if it is null then it get matrix 
## then calcuates inverse matrix value by calling solve function
## then stores the inverse matrix value in the object of "makeCahceMarix" type ( paramter)
## then return the calcualted value

cacheSolve <- function(mt) {
  inver <- mt$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- mt$get()
  inver <- solve(data)
  mt$setinverse(inver)
  inver
}
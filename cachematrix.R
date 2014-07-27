## This function creates a special "matrix" object that can cache its inverse.
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  y<<-x
  if(!is.matrix(y)){ 
    print("not a matrix object/class")  ##checks to see if input is matrix
  
  }  else if(!(nrow(y)==ncol(y))) {
    print("not a square matrix")   ## Quits if not a square matrix
  }
  imatrix<<-solve(x)
  getmatrix<-function() y
  getimatrix<-function() imatrix
    
  
  list(getmartix=getmatrix,getimatrix=getimatrix)
  
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  if(!exists()){
    print("makeCacheMatrix not initialized")}
  else if(!exists(imatrix)){
    print("Inverse matrix doesn't exist")}
  else if(identical(x,y)) imatrix
  else imatrix<<-solve(x)
  
  
}
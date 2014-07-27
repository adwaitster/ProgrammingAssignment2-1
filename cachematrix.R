## This function creates a special "matrix" object that can cache its inverse.


## The makeCacheMatrix function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<<-NULL
  
  if(!is.matrix(x)){ 
    print("not a matrix object/class")  ##checks to see if input is matrix
    }   
        else if(!(nrow(x)==ncol(x))) {
        print("not a square matrix")   ## checks to see wether its a square matrix
        }
  
  m<<-solve(x)
  setmatrix<-function(y){
    x<<-y
    m<<-NULL  
  }
  getmatrix<-function() x
  getimatrix<-function() m
  
  list(setmatrix=setmatrix,getmatrix=getmatrix,getimatrix=getimatrix)
  
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ident_matrix<-diag(1,nrow(x$getmatrix()),ncol(x$getmatrix())) # creates an identity matrix for testing
  m<-x$getimatrix()    #retrives the inverse from makeCacheMatrix 

  if(is.null(m)){
    message ("Inverse not cached. Solving for inverse matrix")
    m<-solve(x$getmatrix())
    
    }else if(!identical(ident_matrix,x$getmatrix()%*%m)){
      # if the matrix presented and inverse are not the same
      # calculate the inverses
      print("cached matrix has changed")
      m<-solve(x$getmatrix())
    } else 
      message("getting cached inv matrix") 
      m
 
  
}
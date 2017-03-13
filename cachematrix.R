## Put comments here that give an overall description of what your
## functions do

## this function stores the inverse of a matrix so that it can be retrieved for later use without recalculation

makeCacheMatrix <- function(x = matrix()) {
  M_inverse <-NULL
  # set the matrix inverse to NULL initially
   
  
  set <- function(Mmatrix) {
    x<<-Mmatrix
    M_inverse<<-NULL
  }
  # this function allows us to store the input matrix
  
  
  get <- function() x
  # this function allows us to retrive the input matrix
  
  
  set_inverse <-function(inverse_input) M_inverse<<-inverse_input
  # this function allows us to store inverted matrix
  
  
  get_inverse <-function() M_inverse
  
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
  # construct a list of all the functions created in this function
    
}


## this function checks if the inverse of a matrix is available in memory from the set_inv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$get_inverse()
  if(!is.null(m)){
    message("obtained cached value of inverted matrix")
    return(m)
  } 
  
#  if(is.null(m)){
    # in this case calculate the inverse from the input matrix
    input<-x$get()
    m<-solve(input)
    x$set_inverse(m)
    return(m)
#  }
}

# test the functions
# testmatrix<-cbind(1:2,2:1)
# z<-makeCacheMatrix(testmatrix)
# y<-cachesolve(z)
# testmatrix%*%testmatrix
# this should produce the 2 by 2 identity matrix as output


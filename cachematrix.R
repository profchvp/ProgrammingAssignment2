##This function creates a special "matrix", 
##which is really a list containing a function to:
## 	set the value of the matrix;	get the value of the matrix;
##  set the value of the inverse; get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  
  cache_Matrix <- NULL  # initialize stor to NULL
  
  # create the matrix in the working environment
  set <- function(matrix_working) {
    x <<- matrix_working
    cache_Matrix <<- NULL
  }
  
  
  get <- function() x # get the value of the matrix
  # invert the matrix
  #store in cache
  setMatrix <- function(inverse) cache_Matrix <<- inverse
  
  getInverse <- function() cache_Matrix # get the inverted matrix from cache
  
  # return the created functions 
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}
##================================================================
## This Function computes the inverse and cache the result
cacheSolve <- function(x, ...) {
  
  cache_Matrix <- x$getInverse()
  
  # return inverted matrix / create the matrix 
  if (!is.null(cache_Matrix)) {
    message("Sorry, Getting Cached Matrix")
    return(cache_Matrix)  # display matrix in console
  }
  
  matrix <- x$get() # create matrix 
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
  tryCatch( {
    cache_Matrix<- solve(matrix, ...) # set and return inverse of matrix
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    x$setMatrix(cache_Matrix) # set inverted matrix in cache
  } )
  
  return (cache_Matrix) # display matrix in console
}


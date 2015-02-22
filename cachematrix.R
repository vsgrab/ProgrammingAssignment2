## Put comments here that give an overall description of what your
## functions do 
##

## Function for creating matrix object, with following functions:
##    set(x) , function to set a new matrix // x - a matrix object
##    get()  , function to return the matrix from object. Returs object with matrix type
##    setinverse(inverseMatrix) , function to set a new inversed matrix// inverseMatrix - a matrix object
##    getinverse() , function to get the inversed matrix. Returs object with matrix type

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y = matrix()) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverseMatrix)
            {
                  
                  ## check for matrix of the same size
                  if (nrow(inverseMatrix) != nrow(x) || ncol(inverseMatrix) != ncol(x))
                  {
                        stop("Your inverse matrix and $get() matrix has different number of rows & columns. Cannot set inverse matrix")
                        inv<-NULL  
                        NULL
                  }
                  else if (nrow(inverseMatrix) != ncol(inverseMatrix))
                  {
                        warning("Your inverse matrix is not square. Cannot set inverse matrix")
                        inv<-NULL
                  }
                  ##check for 0 determinant. Round to 10 digits 
                  else if(round(det(inverseMatrix),10)==0)
                  {
                        warning("Determinant of matrix equals zero. Cannot set inverse matrix")
                        inv<- NULL 
                  }
                  ## check for identity matrix
                  else if (!isTRUE(all.equal(x %*% inverseMatrix, diag(nrow(x)))))
                  {
                        warning("Provided inverse matrix is wrong. Muliplication result is not equal to identity matrix. Cannot set inverse matrix")
                        inv<- NULL 
                  }
                  else inv <<- inverseMatrix
            }
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## helper function to makeCacheMatrix function. 
## x  - list object - output of makeCacheMatrix(matrix()) function
## If inversed matrix is not calculated for the input object, calculates inversed matrix and returns it.
## If have been already calculated the function returns it from cache. 
##

cacheSolve <- function(x, ...) 
      {
            m <- x$getinverse()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            
            ##check for square matrix
            if (nrow(data) != ncol(data))
            {
                  stop("Your matrix has different number of rows & columns. Cannot calculate inverse matrix")
                  m<-NULL
            }
            ##check for 0 determinant.  Round to 10 digits 
            else if(round(det(data),10)==0)
            {
                  stop("Determinant of matrix equals zero. Cannot calculate inverse matrix")
                  m<- NULL          
                  
            }
            else
            {
                  m <- solve(data)
            }
            x$setinverse(m)
            m
      }

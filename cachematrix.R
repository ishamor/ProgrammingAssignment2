## Following two functions allow a user to cache an inverse of a certain matrix
##See detailed description of each function

## This function provides the caching environment. by calling makeCacheMatrix(mat)
## With 'mat' being an invertible matrix, the function returns four function which are bound to
## 'mat' accoring to the R lexical scoping rules. 
## The environmet stores a variable 'inv' which can hold the inverse of 'mat', once computed.
## The set() function sets a new value to 'mat' and nulls 'inv' so it will be computed again
## next time it is requested.
## The get() function returns 'mat', 
## the setinvers(inverse) method sets 'inv' and should be used once the inverse is computed
## for the first time.
## The getinverse() method returns 'inv'

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(newMat){
    mat <<- newMat
    inv<<- NULL #make sure next call to getinverse will return null because current value of 
                # inv does not pertain to the newMat
  }
  get <- function() mat
  setinverse <- function(inverse) inv<<-inverse
  getinverse <- function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cachesolve uses the functions and attached environment created by makeCacheMatrix to
## compute the inverse of the matrix 'mat' from that environment. Function first checks if the
## inverse has already been computed. If not, it computes the inverse and stores the result
## in the makeCacheMatrix environment ('inv' variable)

cacheSolve <- function(matEnv, ...) {
  matInverse <- matEnv$getinverse() #check the inverse in matEnv
  if(!is.null(matInverse)) {
    #message("getting cached matrix inverse") #Message is for debugging
    return(matInverse)
  }
  mat <- matEnv$get()
  matInverse <- solve(mat, ...)
  matEnv$setinverse(matInverse)
  matInverse
}

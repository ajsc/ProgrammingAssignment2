## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## creates a custom object that can cache matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Init var
  
  m <- NULL
  
  ## Function to set the matrix
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## Function to get the matrix
  
  get <- function() {
    x
  }
  
  ## Function to set the inverse of the matrix
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Function to get the inverse of the matrix
  
  getInverse <- function() {
    m
  }
  
  ## List
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve: 
## computes the inverse of custom object returned by makeCacheMatrix. 


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
       
	m <- x$getInverse()
       
	## Return the inverse if its already set
       
	if( !is.null(m) ) {
		message("getting cached data")
        return(m)
    }
        
	## Get the matrix from our object
     
	data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    
    x$setInverse(m)
    
    ## Return the matrix
    
    m      
}

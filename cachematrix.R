
makeCacheMatrix <- function( m = matrix() ) { 
  
  
  i <- NULL ## Creates a special matrix object that can cache its inverse
  
  
  set <- function( matrix ) { ## Set the matrix
    m <<- matrix
    i <<- NULL
  }
  
  
  get <- function() {  ## Get the matrix
    
    m  ## Return the matrix
  }
  
  
  setInverse <- function(inverse) {  ## Now set the inverse of the matrix
    i <<- inverse
  }
  
  
  getInverse <- function() {  ## Get the inverse of the matrix
    
    i  ## Return the inverse property
  }
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## Return a list of the methods
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  
  m <- x$getInverse()   ## Return a matrix that is the inverse of 'x'
  
  
  if( !is.null(m) ) {  ## Just return the inverse if its already set
    message("getting cached data")
    return(m)
  }
  
  
  data <- x$get()  ## Get the matrix from our object
  
 
  m <- solve(data) %*% data    ## Calculate the inverse using matrix multiplication
  
  
  x$setInverse(m)   ## Set the inverse to the object
  
  
  m   ## Return the matrix
}
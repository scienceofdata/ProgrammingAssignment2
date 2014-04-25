## makeCaheMatrix and cacheSolve work together to speed up the calculation of 
## matrix inverses for applications where the elements of the matrix do not 
## change and will need to bused many times in a calculation.  This is
## especially useful when the inverse will be used in loops and the matrix
## is large

## MakeCacheMatrix --
## This function takes a matrix as an argument and creates a list of functions
## that allow the matrix to be accessed.
## $get() returns the constructed matrix.
## $set(x = matrix) sets changes the value of the matrix to the matrix given
## as the argument of the set() operation.
## $setInverse(x = matrix) sets the value of the inverse to the argument x
## $getInverse() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
   i <- NULL  ## set inverse to null
   
   #set will change the value of stored matrix to new value y
   set <- function(y) {
      x <<- y  ## set environ variable x to the argument in $set(y)
      i <<- NULL  ## reset the environ inverse to NULL to compute new inverse
   }
   
   #return the stored matrix
   get <- function() x
   
   #set the value of inverse to the given arument of $setInverse(i)
   setInverse <- function(solve) i <<- solve
   
   #return the value of the inverse where it has been computed or is stil null
   getInverse <- function() i
   
   #return the list of functions
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
   
}


## cacheSolve --
## this function takes a matrix as constructed by makeCacheMatrix() as an 
## argument and returns its inverse.  It does not check for invertability and this must be checked 
## by the calling function before use.  If the matrix does not have a cached
## matrix inverse it will compute the inverse and then cache the inverse for
## use later. 

cacheSolve <- function(x, ...) 
{
   #find the value stored in x for its inverse
   i <- x$getInverse()
   
   #check of inverse has been computed, if so return cached value
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   
   #the inverse has not been computed.
   data <- x$get()
   #compute the inverse
   i <- solve(data, ...)
   #cache the inverse
   x$setInverse(i)
   #return the inverse
   i
}

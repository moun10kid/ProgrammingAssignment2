## OK, this allow you to create a specialized instance of a matrix
## which will cache the computationally intensive inverse as
## appropriate.  In my humble opinion, this is the screwiest, and
## really pretty messy, solution to this problem that I have seen
## in years.  But I have seen this technique before, by a complete
## newbie.

## The following creates a specialized version of a matrix
## that will cache the inverse once computed.  If the matrix
## is changed, the cached value will be removed until
## the inverse is recomputed.

makeCacheMatrix <- function(x = matrix()) 
{
  ## Default the cached inverse
  inv <- NULL
  
  ## Reset the matrix values, and get
  ## rid of the cached inverse
  set <- function (y)
  {
    x <<- y
    inv <<- NULL
  }
  
  ## Return the original matrix
  get <- function () x
  
  ## Save the cached inverse as defined
  setinverse <- function (inInverse)  inv <<- inInverse
  
  ## Get the cached inverse
  getinverse <- function () inv
  
  ## I have no idea what this piece is!
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following will compute the inverse of the given
## specialized "CacheMatrix" from makeCacheMatrix using
## an optimized algorithm that caches the result once
## computed, so that it does not need to be recomputed
## if called again.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the cached inverse and see if it has been
  ## previously computed
  i <- x$getinverse()
  if (!is.null(i))
  {
    message("Getting cached data")
    return (i)
  }
  
  ## If not previously computed, get the data,
  ## compute the inverse, save it, and then
  ## return it.
  theMatrix <- x$get()
  i <- solve(theMatrix)
  x$setinverse(i)
  i
}

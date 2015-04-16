# makeCacheMatrix is a function that creates a matrix and caches its inverse.
# cacheSolve solves for the matrix unless the solved matrix is already stored in the cache,
# in which case it prints the stored cache.

# makeCacheMatrix creates a matrix and stores the inverse of that matrix in a cache.

makeCacheMatrix<-function(x = numeric()){
    # The function initially sets the cache to NULL.
    cache<- NULL
    # Creates a matrix.
    setmatrix<- function(y){
      x<<-y
      # Since the matrix has a new value, the cache is reset to NULL.
      cache<<-NULL
    }
    # Returns the matrix stored as x.
    getmatrix<-function() x
    # Stores the solved matrix in the cache.
    setcache<-function(solve) cache <<- solve
    # Returns the cache.
    getcache<-function() cache
    # Returns a list of the functions created in makeCacheMatrix.
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setcache = setcache, getcache = getcache)
}

# cacheSolve produces the inverse of a matrix by either 1) checking to see if 
# the inverse is stored in the cache or 2) calculating the inverse, storing it
# in the cache and printing the cache.

cacheSolve <-function(x,...){
  # Searches for the cache and returns the cache if there
  # is something stored in the cache.
  cache<-x$getcache()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  # If no cache is found the function retrieves the matrix
  # and calculates it's inverse, and then stores the inverse in the cache.
  data<-x$getmatrix()
  cache<-solve(data,...)
  x$setmatrix(cache)
  # Returns the cache.
  cache

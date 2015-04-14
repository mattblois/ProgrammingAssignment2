## The makeCache function creates a list of functions (set, get, setmatrix and get matrix)
## 

## Write a short comment describing this function

makeCacheMatrix<-function(x = numeric()){
    m<- NULL
    set<- function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m <<- solve
    getmatrix<-function() m
    list(set = set, get = get, 
         setmatrix = setmatrix, getmatrix = getmatrix)
}

## Write a short comment describing this function

cacheSolve <-function(x,...){
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setmatrix(m)
  m
}

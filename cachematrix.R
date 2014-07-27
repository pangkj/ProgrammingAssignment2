## The two functions below cache the inverse of a matrix.

## This function below creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 matr<-NULL
  set<- function(y){
    x<<- y
    matr<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) matr<<- inverse
  getinverse<- function() matr
  list(set=set, get=get,
       setinverse= setinverse,
       getinverse= getinverse)
}

## This function below computes the inverse of the "matrix" returned by makeCacheMatrix above.
## If the inverse has already been computed (and the matrix has not changed), 
## then this cacheSolve function will retrieve the inverse from the cache.
cacheSolve <- function(x) { 
 matr <- x$getinverse()
  if(!is.null(matr)) {
    message("getting cached data")
    return(matr)
  }
  data <- x$get()
  matr <- solve(data)
  x$setinverse(matr)
  matr
        ## Return a matrix that is the inverse of 'x'
}

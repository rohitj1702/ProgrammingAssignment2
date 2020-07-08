#Assignment : Catching the Inverse of a Matrix

#Function-01

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Function-02

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#To check the output

abmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
abmatrix$get()

abmatrix$getInverse()

#if it returns "NULL" that means the matrix hasn't been cached. So try to get cache data by CacheSolve

cacheSolve(abmatrix)

#Now try to inverse with get inverse function

abmatrix$getInverse()